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
#define YYLAST   23607

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  309
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1104
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2227

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
    2675,  2680,  2683,  2682,  2686,  2689,  2696,  2701,  2700,  2711,
    2716,  2721,  2726,  2731,  2732,  2737,  2739,  2744,  2746,  2748,
    2750,  2755,  2756,  2762,  2763,  2764,  2771,  2772,  2774,  2775,
    2776,  2778,  2780,  2787,  2788,  2790,  2792,  2797,  2798,  2804,
    2805,  2807,  2808,  2813,  2814,  2815,  2817,  2825,  2826,  2828,
    2831,  2833,  2837,  2838,  2839,  2841,  2843,  2848,  2850,  2855,
    2857,  2866,  2868,  2873,  2874,  2875,  2879,  2880,  2881,  2886,
    2887,  2892,  2893,  2894,  2895,  2899,  2900,  2905,  2906,  2907,
    2908,  2909,  2923,  2924,  2929,  2930,  2936,  2938,  2941,  2943,
    2945,  2968,  2969,  2975,  2976,  2982,  2981,  2991,  2990,  2994,
    3000,  3006,  3007,  3009,  3013,  3018,  3020,  3022,  3024,  3030,
    3031,  3035,  3036,  3041,  3043,  3050,  3052,  3053,  3055,  3060,
    3062,  3064,  3069,  3071,  3076,  3081,  3089,  3094,  3096,  3101,
    3106,  3107,  3112,  3113,  3117,  3118,  3119,  3124,  3126,  3132,
    3134,  3139,  3141,  3147,  3148,  3152,  3156,  3160,  3162,  3175,
    3177,  3179,  3181,  3183,  3185,  3187,  3188,  3193,  3196,  3195,
    3207,  3206,  3219,  3218,  3232,  3231,  3245,  3244,  3260,  3266,
    3268,  3274,  3275,  3286,  3293,  3298,  3304,  3307,  3310,  3314,
    3320,  3323,  3326,  3331,  3332,  3333,  3334,  3338,  3344,  3345,
    3355,  3356,  3360,  3361,  3366,  3371,  3372,  3378,  3379,  3381,
    3386,  3387,  3388,  3389,  3390,  3392,  3427,  3429,  3434,  3436,
    3437,  3439,  3444,  3446,  3448,  3450,  3455,  3457,  3459,  3461,
    3463,  3465,  3467,  3472,  3474,  3476,  3478,  3487,  3489,  3490,
    3495,  3497,  3499,  3501,  3503,  3508,  3510,  3512,  3514,  3519,
    3521,  3523,  3525,  3527,  3529,  3541,  3542,  3543,  3547,  3549,
    3551,  3553,  3555,  3560,  3562,  3564,  3566,  3571,  3573,  3575,
    3577,  3579,  3581,  3593,  3598,  3603,  3605,  3606,  3608,  3613,
    3615,  3617,  3619,  3624,  3626,  3628,  3630,  3632,  3634,  3636,
    3641,  3643,  3645,  3647,  3656,  3658,  3659,  3664,  3666,  3668,
    3670,  3672,  3677,  3679,  3681,  3683,  3688,  3690,  3692,  3694,
    3696,  3698,  3708,  3710,  3712,  3713,  3715,  3720,  3722,  3724,
    3729,  3731,  3733,  3735,  3740,  3742,  3744,  3758,  3760,  3762,
    3763,  3765,  3770,  3772,  3777,  3779,  3781,  3786,  3788,  3793,
    3795,  3812,  3813,  3815,  3820,  3822,  3824,  3826,  3828,  3833,
    3834,  3836,  3838,  3843,  3845,  3847,  3853,  3855,  3858,  3861,
    3863,  3867,  3869,  3871,  3872,  3874,  3876,  3880,  3882,  3887,
    3889,  3891,  3893,  3928,  3929,  3933,  3934,  3936,  3938,  3943,
    3945,  3947,  3949,  3951,  3956,  3957,  3959,  3961,  3966,  3968,
    3970,  3976,  3977,  3979,  3988,  3991,  3993,  3996,  3998,  4000,
    4014,  4015,  4017,  4022,  4024,  4026,  4028,  4030,  4035,  4036,
    4038,  4040,  4045,  4047,  4055,  4056,  4057,  4062,  4063,  4068,
    4070,  4072,  4074,  4076,  4078,  4085,  4087,  4089,  4091,  4093,
    4096,  4098,  4100,  4102,  4104,  4109,  4111,  4113,  4118,  4144,
    4145,  4147,  4151,  4152,  4156,  4158,  4160,  4162,  4164,  4166,
    4173,  4175,  4177,  4179,  4181,  4183,  4188,  4190,  4192,  4199,
    4201,  4219,  4221,  4226,  4227
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

#define YYPACT_NINF (-1816)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1103)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     161, 12853,   177,   197, 17527,    92, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,    71,   860,
     145, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,    90,   312,
   -1816, -1816, -1816, -1816, -1816, -1816,  4403,  4403,   193, 12853,
     233,   332, 23285, -1816,   345, -1816, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816, -1816,  3322, -1816,   790,   462, -1816,
   -1816, -1816, -1816, -1816, 17375, -1816, -1816,   388,   486,   553,
     194, -1816,  4403,   486,   486,   486,   475,  4445,   652,   838,
   13015, -1816, -1816,   609, 17223,  1522, -1816, -1816, -1816,  2333,
     736, 15621,  7655,   797,  2333,   963,   526, -1816, -1816, -1816,
   -1816,   629, -1816, -1816, -1816, -1816,   607, -1816, -1816, -1816,
   -1816, -1816,   628,   644,   629, -1816,   629,   661, -1816, -1816,
   -1816, 18522,  4403, -1816, -1816,  4403, -1816, 12853, -1816,   683,
   18674, -1816, -1816,  4916,  6031, -1816, -1816,  1047,  1047,   669,
    2353, -1816, -1816, -1816, -1816,   518, 14765,  3571,   629, -1816,
   -1816, -1816, -1816, -1816, -1816,   686, -1816,   694,   731,   751,
   -1816,   792, 22676, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
   16246,  4209,  3322,   697,   775,   782,   801,   803,   815,   821,
   -1816, -1816, 18826, 11864,   832, -1816, 17824, -1816, -1816, -1816,
   -1816,   834, -1816, -1816,   833, -1816,  9824,   930, 20900, -1816,
     852,  4403,   644,   857,   869,   880,   886, -1816, -1816, -1816,
    3396,  3250,   891,  1005,    95,  1005, -1816,   629,   629,   130,
     226,   187,  1005, -1816,   629,   629,   130,   629, -1816,   629,
   -1816,  4415, -1816, -1816,   950,   965,  1047, 20146, -1816, 17375,
   -1816, -1816,  2333, -1816,  1761,   526,   960,  1026,   226,  4403,
    4403,   553, -1816, 14282, -1816,  1047,  1047,   969,  1026,   226,
    4403, -1816, 23484, -1816, -1816, -1816,  1047, -1816, -1816, -1816,
   -1816,  1047, -1816,   892,  4714,  4403, -1816,  2318,   985, -1816,
   -1816, -1816, 17036,   644,   237, -1816, -1816, 20095, -1816,  1005,
     167, -1816, 22676,  6031,  3640,  4415, -1816,   293, -1816, -1816,
   -1816, -1816, -1816, 18674,  4403, -1816,   984, -1816, -1816, -1816,
   -1816,  4403,  3876,   360,   561, -1816,  4403,   694, -1816,   918,
     629,   629,   966, 18978,   623, 15248, 20648,  2333,  2333, -1816,
    2333,  1047,  2333,  1047, -1816, -1816,   629, -1816,   993, -1816,
   19130, -1816, -1816, -1816, 19282,   834, -1816,   410,   666,   266,
     472,   526,   996, -1816,  2353,   973,   694,  2353,  1614, -1816,
    1006,  1021, 22750,  1013,  1019,  1032, 22676, 22824,  1039, 23389,
   -1816, -1816, -1816, -1816, -1816, -1816, 22898, 22898, 16090,  1035,
    4281, -1816, -1816, -1816, -1816,   676, -1816,   699, -1816,  1615,
   -1816, 22676, 22676, -1816,  1030,   653,   916,   967,   519,  1046,
    1060,  1070,  1073,  1120,   329, -1816,   793, -1816,  1122, -1816,
    1000,  5185, 16558, -1816, -1816,   738,  1122, -1816, -1816,   800,
   -1816, -1816,  4209,  1126,  1129,  1131,  1152,  1158,  1168, -1816,
   -1816,   375,  1189, -1816,   818,  1189, -1816, -1816, 18522, -1816,
    1050,  1190, 16714, -1816, -1816,  5343,  2818,  1161, 15248,  1215,
    1137,  1249, -1816, -1816, -1816, -1816, -1816,  4403,  5396, -1816,
   -1816, -1816, -1816, -1816, -1816,  3616,  4514,  1035,  9824,  1193,
    1199, -1816, -1816,  1198, 20900,   779, -1816, -1816, -1816, 20974,
    1211, -1816, -1816, -1816, -1816, -1816,  3396,   794,  1214,  1220,
    1250,   811,  1252,  1254,  1258,  1299,  1328,  1334,  3250, -1816,
   -1816, -1816,   629,  1246,  1271,  1277, -1816, -1816,  1320,   553,
   -1816, -1816,   644,  1026, -1816, -1816, -1816,   553, -1816, -1816,
     644, -1816, -1816,  4415, -1816, 16558, 16558, -1816,  1047,  4916,
   20736, 15409, -1816, -1816, -1816, -1816, -1816,   644,  1026,   167,
    1335, -1816, -1816,  2333,  1342,  1026,   226, -1816,   644,  1026,
   -1816, 23535, -1816,  1047,  1047, -1816, -1816,  1357,   384,  1361,
     526,  1384, -1816, 17688, -1816,   883, -1816,  1477, 20545, -1816,
    4916, 17124, 20146, -1816, 17036, 22972, -1816, -1816, -1816, -1816,
   -1816,  3640,   861,  4415, -1816, 15409,  1005, 12853, -1816,  1392,
   -1816,  1402, -1816, -1816, -1816, -1816, -1816,  2353, -1816, -1816,
    1478,  5274,  3994, 19282, 11864, -1816, 19434, -1816,  1047,  1047,
   -1816, -1816,   834, -1816,   781,  1427,  1567, 22676,   979,  1320,
    1410, -1816,   629,   629, -1816,  1189, -1816, 18978, -1816, -1816,
   18119,  1047,  1047, -1816,  5274,   629, -1816, 19950, -1816, -1816,
   19130, -1816,   518, -1816, -1816, -1816,  1430,  4403,   996,  1429,
     898, 18674,   941, -1816, -1816, -1816, -1816, -1816, -1816,   956,
   -1816,  1439,  1419, -1816, 16402, -1816,  4281, 19586, 19586, -1816,
   16402, -1816, 22676, -1816, -1816, -1816, -1816, -1816, -1816, 16402,
   -1816, -1816, 18218, 19586, 19586,  1000,  1364,  1447,   698,  1777,
   -1816,   972,  1440,  1063,  1443, -1816, 20974, 22676, 21048,  1441,
   22676,  2318, 22676,  2318, -1816,  2636, -1816, -1816, 21122,  2255,
   22676, 21122,  2318, -1816, -1816, 22676, 22676, 22676, 22676, 22676,
   22676, 22676, 22676, 22676, 22676, 22676, 22676, 22676, 22676, 22676,
   22676, 22676, 22676, 22676, 21196,  1422,   792,  4583, 11864, -1816,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
    1442, 22676, -1816, -1816,   738,  1016, -1816, -1816,   629,   629,
   -1816, -1816, 16558, -1816,   378,  1189, -1816,   862,  1189, -1816,
   -1816, -1816,  1320, -1816, -1816,  1320, 23046, -1816, -1816, 11864,
    1448,  1450,  2603,  1586,  3264,   399,  1410, -1816,   629,   629,
    1410,   408, -1816,   629,   629, 22676,  4403,  1175,  1181,  1410,
     240, 14604, 14604,  4403, -1816, -1816, 22676,  1198, -1816,  9824,
    1458, -1816,  3146, -1816, -1816, -1816, -1816, -1816,   974, -1816,
   14604,  2318,  4916,  2318,   982,  1461,  1465,  1466,  1003,  1470,
    1476,  1479,  1481,  1485,  1486,   440,  1189, -1816, -1816,   466,
    1189, -1816, -1816,   512,  1189, -1816, -1816, -1816,  4916,   792,
    1607,  1189, 20240, -1816, -1816,   644, 17688, -1816, -1816, -1816,
    1012,  1487,  1024,  1488, -1816,  1494, -1816,   644, -1816,  1495,
   -1816,   644,  1026,  1494, -1816,   644,  1472,  1489,  1490, -1816,
   -1816, 18119, -1816,  1497, -1816, -1816, -1816,  2318,  4403, 11008,
    1582,  1482, -1816, -1816, 19747, -1816,  1190, -1816, 14604,  1049,
   -1816, -1816,  1494, -1816, 18674, 16558,  1484, -1816,  1484, -1816,
   -1816, -1816,   266,   629,   629, -1816, 19130, -1816, 12029, 16870,
   -1816, 17688,  1501,  1504,  1507, -1816,  7770,   629, -1816,   979,
   -1816, -1816, -1816, -1816,  1320, -1816, -1816, -1816,  1047, -1816,
    4136, -1816, -1816,   526,   428,  1513,  1491,  1505,   266, -1816,
   -1816,  1511,  1518,  1614, 21122, -1816,  1519,  1516,   462,  1520,
    1523,  1525,  1526,  1529, 22676,  1531,  1534,  1535, 11864, 22676,
   -1816, -1816,  2292, -1816, -1816, -1816, 22676, -1816,  1536,  1537,
    9957,  1197, -1816, 21122,  1538, -1816,  1539, -1816, -1816,  5058,
   -1816, -1816,  1056, -1816, -1816, -1816, -1816,  5058, -1816, -1816,
    1204,    21, -1816, -1816,  1030,  1030,  1030,   653,   653,   916,
     916,   967,   967,   967,   967,   519,   519,  1046,  1060,  1070,
    1073,  1120, 22676,  1156, -1816,  1540,  5058, -1816, -1816,  9824,
   -1816, 17688,  1544,  1545,  1546,  1016, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816,  1320, -1816, -1816,  1320, 17688, 17688,
   -1816, -1816,  2603,   872,  1547,  1549,  1550,  1551,  2341,  3264,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816,  1552, -1816,  1410, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816, -1816,  1553,  1556, -1816,   553,  5058,
    1208,    19, -1816, -1816,  1542, -1816, 20900, -1816, 22676,   629,
   21270, 14604, -1816, -1816, -1816,  1500,   527,  1189, -1816,   541,
    1189, -1816, -1816,   543,  1189, -1816, -1816, -1816,  1320, -1816,
   -1816, -1816,  1320, -1816, -1816, -1816,  1320,  1005,  1557, -1816,
    1320,   258, -1816,  1122,  1548, -1816, -1816, -1816, -1816, -1816,
   -1816,  1561, -1816, -1816, -1816, 18674,  1494, -1816,   644, -1816,
   -1816, -1816, -1816, -1816, 13656,  1559,  1572, -1816,   298, -1816,
     445,   373, 11699,  1563, 15769,  1565,  1581,  2426,  2512,  3203,
   21344,  1583, -1816, -1816,  1590,  1592, -1816, -1816,   644, 22676,
   22676,  1720,  1588,   713, -1816, 15934,  1671,  1593,  1571, -1816,
   -1816, -1816, 10833, -1816, -1816, -1816, -1816, -1816,  1778, -1816,
   -1816, -1816,  1293,   465, -1816,   508, -1816,   465, -1816, -1816,
   -1816,  2318, -1816, -1816, 13177, 17375,  1595, -1816,  4403, -1816,
    1575,  1598,  1599, -1816,  1231, -1816, -1816, -1816, -1816,  4916,
   -1816, -1816,  1589,  1591,  1061, 18674,   694,   694,  1430,   996,
     996, -1816, -1816,  1035,  1190, 16714, -1816,  1122, -1816, 12194,
   -1816,   562,  1189, -1816,  1047,  9656, -1816, -1816,   266,   629,
     629,   518,  4403, -1816, 21418, -1816,   266,  1430,  1609, -1816,
   -1816,  1062,   636, 18119, 11864,  2318, -1816,   636, 18370,   636,
   -1816, 22676, 22676, 22676, -1816, -1816, -1816, -1816, 22676, 22676,
    1613,  9824, -1816, -1816,  1600,   740, -1816, -1816, -1816,  1880,
   -1816, -1816,  1233, -1816,   306, -1816, 21122,  1238, -1816, 20974,
   -1816, -1816, 22676,  1597,  1244,  1257,  1198, -1816,   574,  1189,
   -1816, -1816, 17688, 17688, -1816, -1816,  1623,   582,  1189, -1816,
     595,  3697,   629,   629, -1816, -1816, 17688, 17688, -1816,  1612,
   -1816, 15409, 15409,  1626,  1624,  1627,  1630, -1816,  1644, 22676,
   22676,  1260,  1646, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
    1650, 22676, -1816, -1816, -1816,  1320, -1816, -1816, -1816,  1320,
   -1816, -1816, -1816,  1320, 17688, 17688, 17688,   553,   629, -1816,
   -1816,  1269, 22676, 20389,  1648,  1654,  1659, -1816, -1816, -1816,
    1661, 13811, 13966, 14121, 18674, 20146, 19586, 19586,  1663, -1816,
    1636,  1651,   767, 10278, -1816,   499,  4403, -1816, -1816,  4403,
   -1816, 21122,   409,   617, -1816, -1816, -1816, -1816, 22676,  1675,
    1749, 11533, 11183, -1816,  1655, -1816,  1656, 22676,  1658,  9824,
    1660, 22676, 20974, 22676,   955, -1816,  1665,    87, -1816,   143,
    1739,   429,  1684, -1816, -1816,  1687, -1816,  1666, -1816,  1668,
    1688,  1694, 15769, 15769, -1816, -1816,  1758, -1816, -1816,    37,
      37,   823, 14443,   629,   559, -1816, -1816, -1816,  1697, -1816,
    1706, -1816,  1707, -1816,  1701, -1816,  1702, -1816, -1816, -1816,
   -1816,  1710,  1703,  1705, 12359,  1709,  1711,  1716, -1816,  1725,
   -1816, -1816, -1816,  1320, 22676, 22676,  1190,  1717, -1816,  1430,
   -1816,   996,   314,  1491,  9824, -1816,  1430,  1730, -1816, 18674,
   -1816,   890,  1740,  1737,  1064, -1816,  1738, -1816, -1816, -1816,
   -1816, -1816,  9824,  1198, 20974, -1816,  1779,  5058, -1816,  1779,
    1779, -1816,  5058,  2895,  4222, -1816, -1816,  1274, -1816, -1816,
   -1816,  1751,  1752, -1816, -1816, -1816,  1320, -1816, -1816,  1753,
    1756,   629, -1816, -1816, -1816,  1320, -1816, -1816, -1816,  1757,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816,  1746, -1816, -1816, -1816, -1816,  1760,  1759,
     629, -1816, 17688, 17688, 17688, -1816, -1816, -1816, -1816, -1816,
   22676, -1816,   258, -1816,  1122, -1816, -1816, -1816,  1768,  1771,
   -1816,  1663,  1663,  1663,  4104,   927,  1748,   569, -1816,  4104,
     584, 16558, -1816, -1816, -1816,  3847, 22676,  5115,   454, -1816,
   -1816,   149,  1767,  1767,  1767,  4403, -1816, -1816, 17985, -1816,
    1071, -1816, -1816, -1816, -1816,  1085,  1775, 15769,  1593,  1776,
   22676,   388,  1780,   475,  9445, 18674, -1816, -1816, -1816,   939,
   15769, 22676,   842,   639, -1816, 22676, 20746, -1816, -1816,   586,
   -1816,  1198, -1816,  1099,  1100,  1101, -1816, -1816, -1816, -1816,
     644,   955,  1781, -1816, -1816, 22676, -1816,  1784,   792, -1816,
   11699, -1816, -1816, -1816, -1816, 22676, 22676, -1816, -1816,   391,
      37, -1816,   613, -1816, -1816, 10658, -1816,   629, 15409, -1816,
   -1816, 18674, -1816, -1816, -1816,   266,   266, -1816, -1816, -1816,
    1787, -1816, 17688, -1816, -1816,  1789, -1816,  1791,  1790,   996,
    1793, -1816, -1816,  1198,  1807, -1816, -1816,  1809, -1816, -1816,
   22676, -1816, 18370, 22676,  1198,  1810,  1280, -1816,  1284, -1816,
    5058, -1816,  5058, -1816, -1816, -1816, -1816, 17688,  1811,  1813,
   -1816, -1816, 17688, 17688,  1814,  1815,  1298, 14926, 15087, -1816,
    1818, -1816, -1816, -1816, -1816, -1816,  1817,  1822,  1827,  1310,
   22676, -1816, -1816, -1816, -1816, -1816,   591,   927,  1287,   592,
   -1816, -1816, -1816, -1816,   629,   629, -1816, -1816, -1816,   594,
   -1816,  1114,  3847,   785, -1816,  5115, -1816,   629, -1816, -1816,
   -1816, -1816, -1816, -1816, -1816, -1816, -1816, 15769,    73, 21492,
    1889, 15769,  1593, 15570, -1816, -1816, -1816, -1816, 22676, -1816,
   21566,  1897,  1806, 20823, 21640, 15769, 11358,  1593,   637,  1027,
    1824, 22676, -1816,  1835,   353, 15769, -1816, -1816,  1851, -1816,
   -1816,  1828,   792,   733,  1850,  1852,  1311,  1115, 15769,  1857,
   15769, 15769, 15769, 15769, -1816, -1816, -1816, -1816,  4403,  4916,
    1430,  1430, -1816, -1816,  1853,  1854, -1816, -1816, -1816,  1855,
     266,  1863, -1816,  1866, -1816, -1816, -1816, -1816,  1872, -1816,
   -1816, -1816,  1316,  1322, -1816, -1816, -1816, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816,  1870, -1816, -1816,  1874,  1875,  1876,
   -1816, -1816, -1816, -1816, -1816, -1816,  1878,  1881,  1884,  1287,
   -1816,   629, -1816, -1816, -1816, -1816, -1816,  1883,  4104, -1816,
    3018,   103, 12527, -1816, 15664, -1816,    54,  1121, 15769,  1951,
     614,  1877,   585, 15769, 22676,  1887,   637,  1027,  1869, 23120,
    1885,   634,  1973, -1816, 21714, 21788, 22676,  1593,  1879, 12691,
   -1816, -1816, -1816, 19798, -1816,  1892,  1888,   152, 15769, -1816,
   22676, 21122, -1816, -1816, 22676,   465, -1816, -1816,   465, -1816,
   -1816,  1910,  1911,  1912, -1816, -1816,   266,  1430, -1816, -1816,
   -1816, -1816, -1816,  1914,  1916,  1918, 15409,  1906, -1816, -1816,
   -1816,   608,  1189, -1816, -1816,   927, -1816, -1816,   199, -1816,
     254, -1816, -1816, -1816,  1922, 13339, -1816, -1816, 15769, -1816,
      79, -1816, 15769, 22676,  1921, 21862, -1816, -1816, 21936, 22010,
   22676,  1887,  1593, 22084, 22158, 15769,  1908,   659,  1913,   662,
   -1816, -1816,  1927, 13339, 19798, -1816,  4745, 19434,  2318,  1920,
   -1816,  1980,  1933,   746,  1928, -1816,  2013, -1816,  1128,  1138,
     436,   539, -1816, -1816, -1816,  1430,  1939, -1816, -1816, -1816,
   -1816, -1816, -1816, -1816,  1320, -1816, 22676, -1816, 22676, -1816,
   -1816,  1408, 13501, -1816, -1816, 15769, -1816, -1816,  1593, -1816,
   -1816,  1593,  1925,   678,  1929,   685, -1816, -1816,  1593, -1816,
    1593, -1816,  1943, 22232, 22306, 22380, -1816,  1408, -1816,  1923,
    2949,  3920, -1816, -1816, -1816,   152,  1942, 22676,  1924,   152,
     152, 15769, -1816, -1816, 15769,  2027, 15769,  2029,  1955, -1816,
   17688, -1816, -1816, 15664, -1816,  1408, -1816, -1816,  1954, 22454,
   22528, 22602, -1816, -1816,  1593, -1816,  1593, -1816,  1593, -1816,
    1923, 22676,  1956,  3920,  1950,   792,  1957, -1816,   755, -1816,
   -1816, -1816, 15769, -1816, 15769, -1816, -1816, -1816, 10414,  1961,
   15664, -1816, -1816,  1593, -1816,  1593, -1816,  1593,  1962,  1963,
   -1816,   644,   792,  1964, -1816,  1948,   792, -1816, -1816,  1972,
   -1816, -1816, -1816, 10536, -1816,   644, -1816, -1816,  1332, 22676,
   -1816,  1139, -1816, -1816,   792,  2318,  1974,  1968, -1816, -1816,
    1151, -1816, -1816,  1969,  2318, -1816, -1816
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
     521,   522,   523,   524,   525,   526,   533,   534,   840,   536,
     609,   610,   613,   615,   611,   617,     0,     0,     0,   482,
       0,     0,    17,   580,   586,     9,    10,    11,    12,    13,
      14,    15,    16,   797,   103,     0,    20,     0,     2,   101,
     102,    18,    19,   856,   482,   798,   422,     0,   425,   721,
     427,   436,     0,   426,   456,   457,     0,     0,     0,     0,
     563,   484,   486,   492,   482,   494,   497,   548,   535,   466,
     541,   546,   468,   558,   467,   573,   577,   583,   562,   589,
     601,   840,   606,   607,   590,   662,   428,   429,     3,   805,
     818,   487,     0,     0,   840,   878,   840,     2,   895,   896,
     897,   482,     0,  1082,  1083,     0,     1,   482,    17,     0,
     482,   445,   446,     0,   563,   492,   476,   477,   478,   808,
       0,   612,   614,   616,   618,     0,   482,     0,   841,   842,
     608,   537,   714,   715,   713,   774,   769,   759,     0,     0,
     806,     0,     0,   499,   799,   803,   804,   800,   801,   802,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     581,   584,   482,   482,     0,  1084,   563,   885,   903,  1088,
    1081,  1079,  1086,   421,     0,   165,   727,   164,     0,   430,
       0,     0,     0,     0,     0,     0,     0,   420,   972,   973,
       0,     0,   455,   838,   840,   838,   859,   840,   840,   465,
       2,   840,   838,   916,   840,   840,   464,   840,   935,   840,
     913,     0,   556,   557,     0,     0,   482,   482,     2,   482,
     437,   485,   495,   549,     0,   578,     0,   821,     2,     0,
       0,   721,   438,   563,   542,   559,   574,     0,   821,     2,
       0,   498,   543,   550,   551,   469,   560,   471,   472,   470,
     565,   575,   579,     0,   593,     0,   791,     2,     2,   819,
     877,   879,   482,     0,     2,     2,  1092,   563,  1095,   838,
     838,     3,     0,   563,     0,     0,   448,   840,   833,   835,
     834,   836,     2,   482,     0,   795,     0,   755,   757,   756,
     758,     0,     0,   751,     0,   741,     0,   750,   761,     0,
     840,   840,     2,   482,  1103,   483,   482,   494,   473,   541,
     474,   566,   475,   573,   570,   591,   840,   592,     0,   702,
     482,   703,  1057,  1058,   482,   704,   706,   580,   586,   663,
     665,   666,   663,   843,     0,   772,   760,     0,   847,    22,
       0,    21,     0,     0,     0,     0,     0,     0,     0,    24,
      26,     4,     8,     5,     6,     7,     0,     0,   482,     2,
       0,   104,   105,   106,   107,    88,    25,    89,    43,    87,
     108,     0,     0,   123,   125,   129,   132,   135,   140,   143,
     145,   147,   149,   151,   153,   156,     0,    27,     0,   587,
       2,   108,   482,   157,   766,   717,   577,   719,   765,     0,
     716,   720,     0,     0,     0,     0,     0,     0,     0,   857,
     883,   840,   893,   901,   905,   911,     2,  1090,   482,  1093,
       2,   101,   482,     3,   701,     0,  1103,     0,   483,   541,
     566,   573,     3,     3,   683,   687,   697,   703,   704,     2,
     886,   904,  1080,     2,     2,    24,     0,     2,   727,    25,
       0,   725,   728,  1101,     0,     0,   734,   723,   722,     0,
       0,   823,     2,     2,     2,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   862,
     919,   942,   840,     0,   461,     2,   858,   866,  1000,   721,
     860,   861,     0,   821,     2,   915,   923,   721,   917,   918,
       0,   934,   936,     0,   451,   482,   482,   547,   483,     0,
     563,   482,  1085,  1089,  1087,   564,   795,     0,   821,   838,
       0,   431,   439,   496,     0,   821,     2,   795,     0,   821,
     770,   544,   545,   561,   576,   582,   585,   580,   586,   604,
     605,     0,   771,   482,   711,     0,   203,   414,   482,     3,
       0,   563,   482,   820,   482,     0,   433,     2,   434,   792,
     453,     0,     0,     0,     2,   482,   838,   482,   795,     0,
       2,     0,   754,   753,   752,   747,   493,     0,   745,   762,
     539,     0,     0,   482,   482,  1059,   483,   479,   480,   481,
    1063,  1054,  1055,  1061,     2,     2,   102,     0,  1019,  1033,
    1103,  1015,   840,   840,  1024,  1031,   709,   482,   571,   705,
     483,   567,   568,   572,     0,   840,  1069,   483,  1074,  1066,
     482,  1071,     0,   672,   664,   671,  1101,     0,   663,     0,
       0,   482,     0,   855,   854,   850,   852,   853,   851,     0,
     845,   848,     0,    23,   482,    95,     0,   482,   482,    90,
     482,    97,     0,    33,    37,    38,    34,    35,    36,   482,
      93,    94,   482,   482,   482,     2,   104,   105,     0,     0,
     183,     0,     0,   607,     0,  1079,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    62,    63,    67,     0,
       0,    67,     0,    91,    92,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     164,     0,   162,   163,     2,   984,   718,   981,   840,   840,
     989,   588,   482,   884,   840,   894,   902,   906,   912,     2,
     887,   889,   891,     2,   907,   909,     0,  1091,  1094,   482,
       0,     0,     2,   102,  1019,   840,  1103,   954,   840,   840,
    1103,   840,   969,   840,   840,     3,   705,     0,     0,  1103,
    1103,   482,   482,     0,     2,   736,     0,  1101,   733,  1102,
       0,   729,     0,     2,   732,   735,   180,   179,     0,     2,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   840,   871,   875,   914,   840,
     928,   932,   940,   840,   950,   863,   920,   943,     0,     0,
       0,   996,     0,   459,   824,     0,   482,   460,   825,   452,
       0,     0,     0,     0,   450,     2,   826,     0,   435,     2,
     795,     0,   821,     2,   827,     0,     0,     0,     0,   619,
     690,   483,     3,     3,   694,   693,   898,     0,     0,   482,
     415,     0,   465,   464,   563,     3,   101,     3,   482,     0,
       3,   796,     2,   749,   482,   482,   743,   742,   743,   540,
     538,   665,   663,   840,   840,  1065,   482,  1070,   483,   482,
    1056,   482,     0,     0,     0,  1034,     0,   840,  1104,  1020,
    1021,   710,  1017,  1018,  1032,  1060,  1064,  1062,   569,   604,
       0,  1068,  1073,   668,   663,     0,   673,     0,   663,   775,
     773,     0,     0,   847,    67,   807,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,     0,
     122,   121,     0,   118,   117,    28,     0,    29,     0,     0,
       0,     0,     3,    67,     0,    52,     0,    53,    60,     0,
      59,    71,     0,    68,    69,    72,    55,     0,    54,    58,
       0,     0,    51,   124,   126,   127,   128,   130,   131,   133,
     134,   138,   139,   136,   137,   141,   142,   144,   146,   148,
     150,   152,     0,     0,   424,     0,     0,    30,     3,   727,
     158,   482,     0,     0,     0,   985,   986,   982,   983,   768,
     767,     2,   888,   890,   892,     2,   908,   910,   482,   482,
    1010,  1009,     2,     0,     0,     0,     0,     0,   840,  1020,
     957,   974,     2,   952,   960,   707,   955,   956,   708,     2,
     967,   977,   970,   971,     0,     3,  1103,   443,     2,  1096,
       2,   698,   699,   677,     3,     3,     3,     3,   721,     0,
     156,     0,     3,     3,     0,   730,     0,   724,     0,   840,
       0,   482,     3,   447,   449,     0,   840,   872,   876,   840,
     929,   933,   941,   840,   951,     2,   864,   867,   869,     2,
     921,   924,   926,     2,   944,   946,   948,   838,     0,   462,
     997,     3,  1001,  1002,     3,   829,     3,   553,   552,   555,
     554,     2,   796,   830,   777,   482,     2,   828,     0,   796,
     831,   619,   619,   619,   482,     0,     0,   712,     0,   418,
       0,     0,   482,     0,   338,     0,     0,     0,     0,     0,
     185,     0,   333,   334,     0,     0,   387,   386,     0,   160,
     160,   393,   580,   586,   200,   482,     0,   186,     0,   211,
     187,   188,   482,   205,   189,   190,   191,   192,     0,   193,
     194,   339,     0,   353,   195,   359,   361,   367,   196,   197,
     198,     0,   199,   207,   563,   482,     0,   209,     0,   412,
       0,     0,     0,     3,     0,   809,   796,   784,   785,     0,
       3,   780,     3,     3,     0,   482,   759,   759,  1101,   663,
     663,  1067,  1072,     2,   101,   482,     3,   578,     3,   483,
       3,   840,  1027,  1030,   482,     3,  1016,  1022,   663,   840,
     840,     0,     0,   651,     0,   667,   663,  1101,     2,   844,
     846,     0,    96,   482,   482,     0,   100,    98,   482,     0,
     112,     0,     0,     0,   116,   120,   119,   184,     0,     0,
       0,   727,   109,   177,     0,     0,    46,    47,    85,     0,
      85,    85,     0,    73,    75,    49,     0,     0,    45,     0,
      48,   155,     0,     0,     0,     0,  1101,     3,   840,   992,
     995,   987,   482,   482,     3,     3,     0,   840,   963,   966,
     840,     0,   840,   840,   958,   975,   482,   482,  1097,     0,
     700,   482,   482,     0,     0,     0,     0,   432,     3,     0,
       0,     0,     0,   726,   731,     3,   822,   182,   181,     3,
       0,     0,     2,   865,   868,   870,     2,   922,   925,   927,
       2,   945,   947,   949,   482,   482,   482,   721,   840,  1008,
    1007,     0,     0,     0,     0,     0,     0,     3,   796,   832,
       0,   482,   482,   482,   482,   482,   482,   482,   602,   632,
       3,     3,   633,   563,   620,     0,     0,   880,     2,     0,
     416,    67,     0,     0,   324,   325,   208,   210,     0,     0,
       0,   482,   482,   320,     0,   318,     0,     0,     0,   727,
       0,     0,     0,     0,     0,   161,     0,     0,   394,     0,
       0,     0,     0,     3,   215,     0,   206,     0,   315,     0,
       0,     0,   338,   338,   344,   343,   338,   355,   354,   338,
     338,     0,   563,   840,     0,   413,  1012,  1011,     0,     2,
       0,   787,     2,   782,     0,   783,     0,   763,   744,   748,
     746,     0,     0,     0,   482,     0,     0,     0,     3,     0,
       2,  1023,  1025,  1026,     0,     0,   101,     0,     3,  1101,
     657,   663,   673,   673,   727,   674,  1101,     0,   776,   482,
     849,  1013,     0,     0,     0,    39,     0,   113,   115,   114,
     111,   110,   727,  1101,     0,    66,    82,     0,    76,    83,
      84,    61,     0,     0,     0,    70,    57,     0,   154,   423,
      31,     0,     0,     2,   988,   990,   991,     3,     3,     0,
       0,   840,     2,   959,   961,   962,     2,   976,   978,     0,
     953,   968,     3,     3,  1098,     3,   685,   684,   688,  1100,
       2,     2,  1099,     0,     3,   837,   737,   738,     0,     0,
     840,   454,   482,   482,   482,     3,     3,     3,   463,   839,
       0,  1003,     0,  1004,  1005,   999,   937,   813,     2,     0,
     815,   602,   602,   602,   633,   640,   607,     0,   646,   633,
       0,   482,   594,   631,   627,     0,     0,     0,     0,   634,
     636,   840,   648,   648,   648,     0,   628,   644,   482,   419,
       0,   328,   329,   326,   327,     0,     0,   338,   225,     0,
       0,   227,   427,   226,   563,   482,   306,   305,   307,     0,
     338,   185,   265,     0,   258,     0,   185,   321,   319,     0,
     313,  1101,   322,     0,     0,     0,   375,   376,   377,   378,
       0,   368,     0,   369,   330,     0,   331,     0,     0,   358,
     482,   216,   204,   317,   316,     0,     0,   347,   357,     0,
     338,   360,     0,   362,   385,     0,   417,   840,   482,   811,
     764,   482,     2,     2,   656,   663,   663,  1075,  1076,  1077,
       0,  1028,   482,     3,     3,     0,  1036,     0,     0,   663,
       0,   670,   669,  1101,     0,   654,     3,     0,  1014,    99,
       0,    32,   482,     0,  1101,     0,     0,    86,     0,    74,
       0,    80,     0,    78,    44,   159,   993,   482,     0,     0,
     881,   899,   482,   482,     0,     0,     0,   482,   482,   740,
       0,   440,   442,     3,     3,     3,     0,     0,     0,     0,
       0,   779,   817,   598,   600,   596,     0,     0,  1043,     0,
     641,  1048,   643,  1040,   840,   840,   626,   647,   630,     0,
     629,     0,     0,     0,   650,     0,   622,   840,   621,   637,
     649,   638,   639,   645,   692,   696,   695,   338,     0,     0,
     246,   338,   228,   563,   311,   309,   312,   308,     0,   310,
       0,   254,     0,   185,     0,   338,   482,   266,     0,   291,
       0,     0,   314,     0,     0,   338,   337,   379,     0,   370,
       2,     0,     0,     0,     0,   340,     0,     0,   338,     0,
     338,   338,   338,   338,   202,   201,   441,   781,     0,     0,
    1101,  1101,  1078,     3,     0,     0,  1035,  1037,   655,     0,
     663,     0,   653,     2,    50,    42,    40,    41,     0,    64,
     178,    77,     0,     0,     3,   882,   900,     3,     3,   964,
     979,   444,     2,   682,     3,   681,   739,     0,     0,     0,
     873,   930,   938,   998,  1006,   624,     0,     0,     0,  1044,
    1045,   840,   625,  1041,  1042,   623,   603,     0,     0,   336,
       0,     0,     0,   239,   338,   217,     0,     0,   338,   248,
     263,   274,   268,   338,   185,   303,     0,   278,     0,     0,
     269,   267,   256,   259,     0,     0,   185,   292,     0,     0,
     220,   335,     2,   482,   332,     0,     0,   395,   338,   345,
       0,    67,   356,   349,     0,   350,   348,   363,   364,   786,
     788,     0,     0,     0,  1038,  1039,   663,  1101,   675,   778,
      65,    81,    79,     0,     0,     0,   482,     0,   874,   931,
     939,   840,  1051,  1053,  1046,     0,   635,   234,   229,   232,
       0,   231,   238,   237,     0,   482,   241,   240,   338,   250,
       0,   247,   338,     0,     0,     0,   255,   260,     0,     0,
     185,   304,   279,     0,     0,   338,     0,   294,   295,   293,
     262,   323,     0,   482,   482,     3,   380,   483,   384,     0,
     388,     0,     0,     0,   396,   397,   223,   341,     0,     0,
       0,     0,   659,   661,  1029,  1101,     0,   994,   965,   980,
     686,     2,  1047,  1049,  1050,   642,     0,   236,     0,   235,
     219,   242,   482,   408,   251,   338,   252,   249,   264,   277,
     275,   271,   283,   281,   282,   280,   261,   276,   272,   273,
     270,   257,     0,     0,     0,     0,   222,   242,     3,   373,
       0,  1043,   381,   382,   383,   395,     0,     0,     0,   395,
       0,   338,   346,   342,   338,     0,   338,     0,     0,   660,
     482,   230,   233,   338,     3,   243,   409,   253,     0,     0,
       0,     0,   302,   300,   297,   301,   298,   299,   296,     3,
     373,     0,     0,  1044,     0,     0,     0,   389,     0,   398,
     224,   351,   338,   365,   338,   658,     3,   212,     0,     0,
     338,   290,   288,   285,   289,   286,   287,   284,     0,     0,
     374,     0,   401,     0,   399,     0,   401,   352,   366,     0,
     214,   213,   218,     0,   221,     0,   371,   402,     0,     0,
     390,     0,  1052,   372,     0,     0,     0,     0,   403,   404,
       0,   400,   391,     0,     0,   392,   405
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1816,  6938,  5845, -1816,    -1,   380,  1471,  -180, -1816,  -324,
   -1816,   386, -1816,  -725, -1816,   806,  -953,  -916, -1816,   173,
    6302,  2039, -1816,  1587, -1816,  1432,   504,   776,   798,   581,
     788,  1378,  1398,  1399,  1400,  1403, -1816,   127,  -157,  8591,
     951, -1816,  1722, -1816, -1816,  -694,  8299, -1178,  2962, -1816,
     205, -1816,   945,   -22, -1816, -1816, -1816,   460,    75, -1816,
   -1722, -1599,   311,    50, -1816, -1816, -1816,   322, -1542, -1816,
   -1478, -1816, -1816, -1816, -1816,  -528, -1152, -1816,   464, -1207,
     463, -1816, -1816, -1816, -1816, -1816,    80, -1173, -1816, -1816,
   -1816,    14,   487,   490,   128, -1816, -1816, -1816, -1816,  -818,
   -1816,    53,   -12, -1816,   132, -1816,    12, -1816, -1816, -1816,
     952,  -861,  -802, -1354, -1816,    32, -1258,    16,  2570,  -785,
    -704, -1816,  -295, -1816, -1816,    64, -1816,  -153,    96,  -254,
    -251,  4125,   849,  -636,   141,    62,   175,  2220,  2286, -1816,
    2142, -1816,   116,  4137, -1816,  2083, -1816,    78, -1816, -1816,
    1141,   212,  4815,  3239,   -32,  1932,  -308, -1816, -1816, -1816,
   -1816, -1816,  -292,  5093,  5442, -1816,  -351,   164, -1816,  -712,
     259, -1816,   183,   784, -1816,   -71,  -301, -1816, -1816, -1816,
    -332,  6147,  -878,  1235,    55,  -626,  -682,  -255,  1475, -1816,
   -1310,  -156,  1157,  1206,   975,  4129,  -131,  -465,  -252,  -184,
    -467,  1374, -1816,  1714,   109,  1276,  1601, -1816, -1816, -1816,
   -1816,   324,  -159,  -172,  -896, -1816,   479, -1816, -1816, -1124,
     489, -1816, -1816, -1816,  2212,  -744,  -437,  -934,     0, -1816,
   -1816, -1816, -1816, -1816, -1816,   129,  -871,  -209, -1815,  -111,
    7822,   -61,  7407, -1816,  1243, -1816,  1417,   -74,  -222,  -219,
    -206,     1,   -66,   -60,   -49,   290,   -18,    -6,    -4,  -202,
     -58,  -200,  -199,  -186,   -29,  -181,  -169,  -146,  -777,  -724,
    -691,  -689,  -722,  -163,  -684, -1816, -1816,  -708,  1446,  1453,
    1460,  2742, -1816,   605,  7794, -1816,  -588,  -599,  -562,  -557,
    -610, -1816, -1712, -1709, -1703, -1695,  -614,  -104,  -294, -1816,
   -1816,   -41,   134,   -95, -1816,  8423,  1832,   618,  -396
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1206,   224,   405,   406,    82,    83,   407,   381,   408,
    1534,  1535,   409,  1012,  1013,  1014,  1322,  1323,  1324,  1546,
     431,   411,   412,   413,   708,   709,   414,   415,   416,   417,
     418,   419,   420,   421,   422,   423,   424,   433,  1111,   710,
    1456,   771,   218,   773,   427,   838,  1207,  1208,  1209,  1210,
    1211,  1212,  1213,  2178,  1214,  1215,  1463,  1659,  2019,  2020,
    1943,  1944,  1945,  2144,  2145,  1216,  1673,  1674,  1675,  1839,
    1840,  1217,  1218,  1219,  1220,  1221,  1222,  1866,  1870,  1480,
    1472,  1223,  1224,  1479,  1473,  1225,  1226,  1227,  1228,  1229,
    1691,  2162,  1692,  1693,  2055,  1230,  1231,  1232,  1459,  2063,
    2064,  2065,  2208,  2220,  2091,  2092,   303,   304,   909,   910,
    1178,    85,    86,    87,    88,    89,    90,   464,    92,    93,
      94,    95,    96,   232,   233,   590,   285,   466,   435,   467,
      99,   313,   101,   102,   155,   346,   347,   106,   107,   170,
     108,   930,   348,   156,   111,   256,   112,   157,   264,   350,
     351,   352,   158,   428,   117,   118,   354,   119,   581,   898,
     896,   897,  1632,   355,   356,   122,   123,  1174,  1424,  1638,
    1639,  1799,  1800,  1425,  1627,  1819,  1640,   124,   668,  1739,
     665,   357,   666,   667,  1285,  1104,   472,   473,   902,   903,
     474,   475,   904,   359,   585,  1236,   437,   438,   219,   492,
     493,   494,   495,   496,   334,  1256,   335,   928,   926,   615,
     336,   375,   337,   338,   439,   126,   176,   177,   127,  1250,
    1251,  1252,  1253,     2,  1161,  1162,   607,  1245,   128,   325,
     326,   266,   277,   564,   129,   222,   130,   316,  1113,   888,
     526,   168,   131,   679,   680,   681,   132,   318,   236,   237,
     238,   319,   134,   135,   136,   137,   138,   139,   140,   241,
     320,   243,   244,   245,   321,   247,   248,   249,   806,   807,
     808,   809,   810,   250,   812,   813,   814,   776,   777,   778,
     779,   527,  1154,  1402,   141,  1747,   640,   641,   642,   643,
     644,   645,  1802,  1803,  1804,  1805,   630,   477,   362,   363,
     364,   440,   210,   143,   144,   145,   366,   830,   646
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      81,   380,   133,    81,   372,   547,  1021,   589,   508,   194,
     358,   509,  1450,   344,   192,   195,  1476,    91,   376,   561,
     152,   827,  1001,   235,   510,   426,   196,  1080,   511,  1254,
     512,   513,  1461,   956,   498,   942,   151,   648,  1237,   242,
     669,  1407,   544,   209,   514,   201,   308,   713,  1233,   515,
     950,  1586,  1587,  1460,  1258,    81,    81,   197,    81,   180,
     133,   516,   361,   104,  1327,    97,   658,  1056,   246,   198,
     661,   199,   943,   994,    81,    91,   719,   944,  1074,   113,
     881,   883,  1081,    81,   517,   207,  1930,  1661,  1926,   625,
    1287,    81,   508,  1334,  1927,   509,    81,    98,   239,    81,
     153,   267,  1928,    81,  2022,   278,   656,   459,   510,   209,
     659,  1075,   511,  1076,   512,   513,   260,   109,  1077,   885,
     272,   104,   524,    97,   529,  2021,   443,  1195,   514,  2028,
     893,   537,   444,   515,  1851,   142,   271,   113,   142,  1105,
    1105,    81,   103,   445,    81,   516,    81,   311,   133,  1169,
    1940,  1941,    81,  1369,  2095,    98,  1368,   519,  1105,    81,
     648,  -789,    58,    91,   194,   619,    81,    58,   517,   506,
     195,   922,  1330,   520,   446,   109,   105,   146,  1246,  1326,
     205,   196,   275,  1662,  1662,  1695,   447,  1370,   448,  1710,
      81,    81,   207,   142,  1844,  1084,  1107,  -790,   596,   598,
     103,  1091,   521,   942,   619,    81,   970,   480,   625,   104,
     159,    97,   197,   114,  1122,   489,   950,  2014,   553,   220,
      81,    58,  2027,   160,   198,   113,   199,  1697,  1406,    81,
      81,  2029,   207,  1942,   105,  1410,  1105,   306,   142,   631,
     943,   519,   166,    98,   951,   944,   205,   297,   194,  1291,
      81,   525,   533,   601,   195,  2087,  2096,   520,    81,    58,
     207,   576,  2023,   109,  1696,   196,   523,   714,    81,    81,
     379,   114,    81,  2021,   565,   262,   662,   873,  1315,    81,
     558,   142,   553,   220,   845,   877,   521,   846,   103,   562,
    -821,   569,  1243,    81,    81,  1698,    81,   165,   457,   915,
     847,  1959,  1354,   811,   848,   525,   849,   850,  2061,   425,
     831,  1437,  1490,    81,    81,  -410,   207,  1060,   648,   597,
     851,  1436,   105,    81,  1709,   852,  -411,  1460,  1712,   937,
      81,    81,    20,  2086,   879,    81,   967,   853,   299,   534,
     884,  1237,   648,   525,  2123,   179,  1661,  1341,  1074,   648,
     920,  1233,   221,   549,  -821,   552,  1306,  1355,   631,   114,
     854,  1277,   977,   798,   592,    58,   962,  1370,   532,   935,
    1964,  1965,  1419,    81,   208,   540,    81,   678,  1019,   845,
     113,  1075,   846,  1076,   149,   181,  -410,   240,  1346,  1420,
     268,   209,   597,   955,   279,   847,   557,  -411,    98,   848,
    1519,   849,   850,  1548,  1399,  1746,   961,   568,  1526,  1930,
    1085,  1926,  2088,  2089,  1088,   851,   637,  1927,   109,   552,
     852,  2027,  1408,  1101,  1102,  1928,  1400,   501,   663,  1553,
    1940,  1941,   853,   664,  1432,  1433,   174,   174,  1281,  1105,
     443,    81,  1662,   480,   865,   604,   444,    58,   563,   525,
      58,  2014,   344,  1166,   201,   854,  1429,   445,  2027,   934,
     866,  1554,   753,  1084,    81,    81,  1868,  1586,  1587,   293,
    1421,    58,   174,   942,  1619,  1430,    81,    81,  2038,  2039,
      58,   208,  1470,   670,   182,    81,   672,   489,   446,   867,
    1284,  1277,  2143,  1842,   612,  1379,   481,   190,  1850,  1869,
     447,   361,   448,   161,   754,    81,   162,   163,   593,   164,
     943,  2134,    58,  1969,   114,   944,   235,    81,  2143,   480,
     262,   208,   174,   613,   614,   174,  1195,   789,   205,   865,
    1061,   525,   242,   298,   525,   443,   190,  1461,    58,  1474,
     174,   444,    81,   631,  -599,   866,  2180,   370,    81,   208,
    1434,  1082,   445,   976,   797,   635,   979,   980,  1460,   981,
    1089,   913,  1475,   566,   635,   213,  1106,  1106,   983,   298,
    -972,   985,   986,   987,   867,  1474,  1045,  -972,   892, -1102,
    1660,  1676,  1336,  1699,    58,  1106,  1651,    81,  1662,    81,
     663,   912,  1135,   648,  1758,   664,   525,  1431,  1475,    58,
      81,   174,    81,   712,   480,  1816,    81,   921,   133,  1419,
    1419,  1419,  1817,    58,  2136,    58,    81,  1477,  1139,   202,
      81,    81,   525,    91,    63,    64,  1420,  1420,  1420,  1195,
     648,  1818,  -652,  1124,    58,  1557,  1759,  1761,  1763,  -652,
    1478,   811,  1262,   262,    -3,  1115,    58,   743,   744,   174,
     174,   956,   227,    81,    58,   251,   592,  1645,  1095,  1147,
     174,  1512,   875,  1106,  1143,  1958,    81,    58,   525,   104,
     275,    97,   113,    77,   579,   174,  1646,   584,   270,  1382,
      58,   293,  1261,   525,   153,   113,   939,   887,  1872,  1148,
      98,   745,   746,  1386,   891,  1390,  2080,   525,   895,   525,
    1360,    58,   215,    98,   174,  1471,  1650,  1421,  1421,  1421,
     109,   174,   174,   216,  1510,   616,   174,  1429,   635,   617,
      81,  1869,    81,   109,    81,   576,  1563,  1807,    81,   217,
     525,    81,   481,   920,  1572,  1811,  1716,   874,   525,  -476,
    1578,   142,  1645,   996,   996,   878,  1808,  1576,   103,  1817,
    1931,   635,  1817,  2035,   174,    74,    81,   174,  1684,   570,
    2081,  1810,   886,  1852,   525,   295,   262,  1662,  1925,  1932,
     251,  1935,   996,   894,   582,   634,  2037,   298,  2070,   635,
     297,  2071,   105,  1834,  1835,  1836,    79,   636,  2050,  1310,
    1498,  2033,   996,  1845,  1653,  1662,  1311,  1280,  1846,   637,
    -477,    81,  2044,    81,   298,  1837,   114,  1902,   481,  1903,
      14,    15,    16,    17,    18,    81,   736,   996,   190,   114,
     996,  -410,    81,   737,   738,   563,  -973,  2113,   489,  -810,
    2115,    81,   720,  -973,  1662,   344,   996,   721,   374,  1660,
      81,    81,    81,   996,  1543,   174,  2149,  1880,  1881,   297,
    1756,   449,   989,  2151,   713,   722,  1367,   174,   174,   312,
     723,   939,  2106,   990,   991,   190,  1106,    81,   332,    58,
      74,   148,  -715,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,   377,   361,   252,   253,  1976,   254,   712,
     774,  1545,  1977,   255,   525,   712,   200,    64,  1326,    74,
    2128,    79,    80,   378,   712,  2129,    81,    81,   489,  2195,
     379,   187,  1796,    74,  2196,  1353,   811,  1809,   161,  1635,
      76,   162,   163,   712,   164,    91,  1636,  1502,  1503,   450,
      79,    80,  1374,   634,   832,   833,   451,   635,   834,   449,
    1248,   525,   425,  1748,    79,    80,   297,   755,   449,   261,
     525,   756,   497,  1110,   781,   452,  1247,   453,   782,    81,
     283,   648,   290,   534,   292,   858,  -478,   525,   149,   454,
     793,  1235,   678,    97,   525,   455,    14,    15,    16,    17,
      18,   907,  1681,   298,   479,  1676,   483,   113,  1834,  1835,
    1836,   484,    14,    15,    16,    17,    18,   174,   575,    64,
     996,   931,   933,   261,   499,    98,   290,   292,    81,   502,
    1837,   955,  1997,   604,  1065,   449,    81,   525,   525,  1843,
    1249,  1168,    74,  1514,  1082,   109,   449,   503,   635,    14,
      15,    16,    17,    18,   959,    58,  1397,   906,   504,   920,
    1491,   907,   634,   142,   505,    81,   635,   174,   489,   522,
     103,    58,   969,    79,   636,   261,   617,  1743,   142,    74,
      14,    15,    16,    17,    18,  1626,  1686,  1687,  1688,  1689,
    1690,    81,   620,   293,   714,  1754,   262,    81,    81,  1797,
    1155,   739,   740,   525,   105,  1834,  1835,  1836,    58,   563,
      79,    80,  1163,   741,   742,   971,  1167,   376,   376,   617,
    1170,  1005,   545,  1007,   523,  1010,  1585,  1837,    81,  1018,
     972,    74,  1022,   262,   973,   220,  1838,   546,  2075,    58,
     556,   114,   261,   623,   290,   292,   995,  1525,  1119,   567,
     996,   634,  1120,  1426,   297,   635,   586,  1047,   525,   683,
    1531,   344,    79,   636,   608,  1608,  1663,   655,    74,   671,
      14,    15,    16,    17,    18,   534,   261,   202,   716,   525,
     682,   631,   261,   664,  2093,   686,  1157,  1536,   774,  1331,
     996,   687,   525,  1834,  1835,  1836,   747,   748,  1159,    79,
      80,  1248,   996,   489,   688,   996,    81,    81,    81,  1740,
     361,   692,  2093,   716,   261,  1837,   174,  1247,    91,   735,
     653,   604,   292,   174,  -186,   525,  1588,   623,   716,    58,
    1325,   489,  1594,  1595,  1326,  1497,  1530,    81,  1751,   782,
    1326,  1123,  1752,  1125,   749,  1827,   998,   999,    91,  1326,
      81,  2146,   750,    81,    81,   267,   278,    81,   696,  1828,
    1024,  1025,  1026,   996,  1235,  1110,    97,   751,    81,   920,
     260,   272,   752,  1854,  1855,  1856,  2068,   996,  1120,   996,
     113,  1249,    14,    15,    16,    17,    18,   271,  1936,  1982,
    1422,  -480,   782,   996,  1235,  2030,    97,   757,    98,   996,
     783,    81,  2132,   784,   965,   785,  1326,  1177,   174,   174,
     113,   261,  2133,  2217,    81,   815,   996,  2214,   109,   142,
      14,    15,    16,    17,    18,  2223,   786,  2164,    98,  2224,
     489,  2168,   787,   275,   996,  1332,   142,   261,    81,   653,
     292,    58,   788,   103,  1031,  1032,  1033,  1034,   109,  1793,
    1794,  1795,  1097,  1098,   696,  1411,  1412,  1413,  1099,  1100,
    1279,  1820,  1820,  1820,   470,   456,   142,    -3,  1642,  -479,
      81,   344,   -18,   103,  1313,  1120,   829,   105,   828,    58,
     476,  1328,  1329,   839,  1643,  -157,  -157,   261,   855,   142,
    1426,  1426,  1426,  1409,   856,  1628,  1426,  -122,  -122,  -122,
    -122,  -122,  -122,  -481,  1470,  1471,  1435,   105,  1099,  1489,
    1551,  1552,   261,  1644,   114,  1556,  1552,   261,   869,   261,
     361,  1560,  1552,  1454,   857,  1663,   859,  1889,   860,   262,
     712,   425,   861,   508,  1071,  1544,   509,  1596,  1544,    74,
     261,    81,   261,   261,   114,    81,  1071,  1610,    81,   510,
     152,  1764,  1120,   511,   871,   512,   513,  1900,  1120,  1797,
     261,  1901,  1552,   525,   563,  1114,   151,   870,   489,   514,
      79,    80,   261,   862,   515,  1911,  1912,    91,    91,  1558,
    -121,  -121,  -121,  -121,  -121,  -121,   516,  1923,   996,  1980,
    1981,   489,   489,  2001,  1552,   261,   305,   653,   292,  2002,
    1552,    81,   863,   565,   628,  1940,  1941,   651,   864,   517,
    2214,  2215,   193,  1549,  1550,   889,   425,   425,   562,   261,
     653,   628,   890,  1665,  1665,   628,   261,  1422,  1422,  1422,
     153,  1624,  1625,  1629,   234,  1027,  1028,  -597,  1863,   113,
     113,  -595,  1914,   489,   268,   279,   183,     6,     7,     8,
       9,    10,    11,    12,    13,  1035,  1036,    98,    98,  1029,
    1030,   489,  2135,  2137,   899,  1248,    81,   908,   142,   923,
    1642,    81,    81,    81,   925,  1642,   929,   109,   109,  1711,
    1713,  1247,  1588,   519,   174,   344,  1643,   174,   174,   174,
     317,  1643,  1821,  1822,  1812,   142,   142,  1741,  1742,   520,
    1499,  1500,   103,   103,   945,   947,   637,   280,   964,   968,
     845,   974,   174,   846,   997,  1644,   975,  1000,   174,  1044,
    1644,  1003,  1049,   628,  1078,  1070,   847,  1071,   521,  1117,
     848,   584,   849,   850,   361,  1126,   105,   105,   174,  1127,
    1128,  1536,  1588,    81,  1129,  1249,   851,   673,    81,  1149,
    1130,   852,  1171,  1131,    81,  1132,    81,  1652,  1654,  1133,
    1134,  1158,  1160,   853,    81,  -793,  1164,   507,   234,  1172,
    1173,  -691,  1238,   114,   114,  1271,   489,   563,  1272,  1239,
    1255,  1273,   174,   142,  1283,  1286,   854,  1284,   317,   489,
    1990,  1288,  1289,  1292,  1293,  1532,  1381,  1296,  1295,  1297,
     260,   272,  1975,  1299,  1298,  1301,  1714,   490,  1302,  1303,
    1308,  1309,   470,  1373,  1333,  1316,  1317,   271,  1338,  1339,
    1340,  1347,   674,  1348,  1349,  1350,  1403,  -679,   476,  1358,
    -678,  1398,  -794,  1427,   489,  1438,    91,  1441,   675,  2056,
     676,   677,    65,    66,    67,    68,    69,    70,    71,    72,
    1428,   602,   317,  1442,  1458,  1451,   724,  1248,   725,   726,
     727,   865,  1452,   275,  1453,  1462,   470,  -714,  1464,   648,
     261,   996,  1485,  1247,  1483,  1486,  1487,   866,  1544,    81,
    1528,    81,  1665,   261,   628,   470,  1493,   728,  1495,  1584,
     729,   730,   566,  1542,  1559,   731,   732,  1571,   113,    19,
    1589,   476,  1590,  2018,  1592,  1591,   867,   261,   628,   905,
      14,    15,    16,    17,    18,   993,    98,  2062,   261,   142,
    2056,   628,  1552,  1597,  1600,  1615,   174,   261,  1616,   174,
    1617,    81,  1620,  1633,    81,  1631,   109,  1249,    48,    49,
      50,    51,    52,    53,    54,    55,   489,  1431,  1634,  1656,
     489,  1471,  1677,  1678,   142,  1680,  1700,  1682,  1702,   262,
    1705,   103,  1694,  1703,   489,  1704,  1706,  2122,  1195,   562,
    1588,  1717,   174,   174,   489,   142,  1467,  1719,  1720,  1722,
    1723,  1724,    91,  1725,  1642,  1726,  1727,   489,  1728,   489,
     489,   489,   489,  1729,  1736,   105,  1501,    81,    81,  1731,
    1643,  1745,   148,   805,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,  1749,  1857,  1750,  1753,   508,   470,
    1757,   509,  1765,  1779,   261,  1527,  1766,  1770,  1665,  1644,
    1771,   449,   114,  1781,   510,   476,   212,  1596,   511,  1791,
     512,   513,  1792,   844,   113,  1806,  1636,  1829,   261,  2141,
    1831,  2018,  2058,  1860,   514,   234,  1862,    81,   221,   515,
     470,  1888,    98,   489,  1882,  2062,  1886,   489,  1887,  2062,
    2062,   516,   489,  1890,  1561,  1468,   476,   825,  1892,   490,
     317,  1899,   109,  1894,  1948,  1905,   317,  1906,  1909,  1910,
    2166,  1920,  1953,   689,   517,  1916,  1921,   489,   476,   476,
     142,  1922,   212,  1954,   148,  2193,  1968,   103,    65,    66,
      67,    68,    69,    70,    71,    72,  1318,   476,   733,   734,
    1319,  1966,  1320,  1972,  1978,  1974,  1979,   317,   563,  1984,
    1994,  1995,  2207,  2058,  1998,  1996,  2207,  1999,   919,   733,
     317,   105,   441,  2000,  -680,   174,  2032,   489,  2008,  2009,
    2010,   489,  2011,    76,  2218,  2012,  1547,   174,  2013,   525,
      84,    91,  -580,   150,   489,  2034,  2040,   519,  2045,   733,
     174,  2059,  2216,  2043,   194,    81,  2051,    81,   114,   601,
     195,  2072,  2073,   520,  1912,  2060,  2074,   425,  2077,    91,
    2078,   196,  2079,  2090,  2099,   476,  2112,   628,  2116,  2125,
     651,  2114,   905,   261,  2126,  2127,  2130,  1665,  2131,   865,
    2139,   554,   521,  2148,   489,   174,  2152,  2150,    84,  2165,
    2161,  2167,  2172,   113,  2174,   866,  2175,  2181,    91,  2192,
    2191,  2194,  2202,  2204,   191,  1665,  2209,  2205,   261,    81,
      81,    98,   207,    84,   261,  2210,  2212,  1037,  2221,   470,
     489,   113,  1555,   489,   867,   489,   231,  1738,  1896,   259,
     992,   109,   489,    84,  1744,  2222,  2225,   905,  1038,    98,
    1039,  1457,  1040,   772,  1665,   554,  1041,  1466,  2203,   142,
    1864,  1755,    81,  2142,   480,  1970,   103,  2159,  1963,   109,
     113,   489,  1871,   489,  2189,   633,  1873,   489,  1858,   489,
     150,  1859,  2118,  2169,  2211,  2117,    84,   142,    98,   150,
    1484,   171,   315,   323,   103,   288,   555,  2016,  2085,  1282,
     105,  1630,   489,  1116,  1257,   343,  1481,   174,   109,   835,
    1877,   174,     3,   425,    81,   425,  1290,  1790,   927,  1073,
    1052,   805,     0,    81,     0,   174,   142,  1053,   105,   432,
     191,   191,     0,   103,  1054,   174,     0,   114,     0,     0,
       0,   150,   462,     0,     0,   259,     0,     0,   174,     0,
     174,   174,   174,   174,   425,     0,     0,   905,   174,   317,
       0,     0,     0,     0,   212,   114,     0,   105,     0,   231,
     231,     0,   261,    19,   905,   905,     0,     0,   476,     0,
       0,     0,   188,     0,     0,   317,     0,     0,  2190,     0,
     315,     0,     0,     0,   633,     0,     0,     0,    84,  1853,
     490,     0,     0,   825,   114,    14,    15,    16,    17,    18,
    1305,     0,   259,     0,     0,     0,    52,    53,    54,    55,
       0,     0,  1023,     0,   174,   281,     0,     0,   174,   282,
       0,   261,   286,   174,   291,     0,   425,   183,     6,     7,
       8,     9,    10,    11,    12,    13,   323,     0,   189,     0,
       0,  1156,   323,   315,   315,     0,     0,     0,   174,   148,
       0,  1891,   150,    65,    66,    67,    68,    69,    70,    71,
      72,  1016,  1898,     0,     0,     0,     0,   441,   441,     0,
       0,     0,   343,   638,   647,     0,   263,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2206,   284,   287,   343,
       0,     0,     0,   343,     0,     0,   628,     0,   174,     0,
    2213,  1017,   174,    58,     0,     0,  1270,     0,   327,   328,
     329,   330,   148,     0,     0,   174,    65,    66,    67,    68,
      69,    70,    71,    72,     0,   470,     0,   432,  2124,     0,
     263,     0,     0,     0,     0,   148,     0,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,   148,     0,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,   432,     0,     0,   775,   174,     0,     0,     0,   957,
    1321,   191,   281,     0,     0,     0,     0,     0,  1321,  1073,
       0,     0,   263,  1351,    76,  1352,   805,   150,  1991,  1992,
       0,   462,     0,   441,     0,   804,     0,   647,     0,   331,
       0,   174,     0,   261,   174,     0,   174,  1321,   476,   476,
     490,     0,     0,   174,     0,     0,  1337,   332,     0,     0,
     148,     0,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,  1344,  1345,   231,     0,     0,   905,   905,
       0,     0,   174,     0,   174,     0,     0,   231,   174,   263,
     174,     0,   905,   905,     0,     0,     0,   281,   282,     0,
     652,  1300,   291,     0,     0,     0,  1304,     0,     0,     0,
    1321,     0,   315,   174,   432,   432,     0,  1312,   315,     0,
     343,     0,     0,   263,     0,  2219,     0,     0,     0,   263,
     905,   905,   905,  1443,  2226,     0,     0,     0,     0,   697,
       0,     0,     0,     0,   441,  2076,   148,     0,   172,   173,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   315,
       0,   263,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,   315,     0,   343,   470,    84,     0,     0,     0,
       0,     0,     0,     0,   204,     0,     0,     0,     0,     0,
       0,     0,   343,   462,   261,   647,   317,     0,     0,     0,
       0,     0,     0,   638,     0,   698,     0,   638,     0,     0,
       0,     0,   261,     0,     0,     0,   343,     0,     0,  1445,
       0,     0,     0,  2138,     0,     0,   647,     0,     0,   343,
       0,     0,     0,     0,     0,   697,     0,   148,     0,     0,
     150,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     204,     0,     0,   432,     0,     0,   150,   150,     0,   432,
       0,     0,     0,     0,     0,    74,   204,     0,   432,     0,
     148,   150,   150,   150,    65,    66,    67,    68,    69,    70,
      71,    72,  1008,     0,   263,  1072,    76,   441,     0,   635,
     204,     0,     0,     0,     0,     0,    79,    80,  1579,   689,
       0,   698,     0,   465,     0,     0,     0,     0,   261,     0,
       0,     0,   490,   281,     0,     0,     0,     0,     0,     0,
    1321,     0,  1009,     0,     0,     0,     0,   462,     0,     0,
       0,     0,     0,     0,     0,  1825,     0,     0,   905,   905,
     905,     0,     0,   775,   775,     0,     0,  1567,  1568,     0,
       0,   432,     0,     0,     0,     0,     0,     0,     0,   204,
       0,  1582,  1583,     0,     0,     0,   263,     0,   462,  1641,
       0,   804,     0,   804,     0,     0,     0,     0,     0,     0,
       0,     0,   733,     0,  1826,     0,     0,   263,     0,     0,
     343,   343,     0,     0,     0,     0,     0,     0,     0,  1605,
    1606,  1607,     0,     0,     0,     0,     0,   263,     0,   343,
       0,   315,     0,     0,     0,     0,     0,     0,  1537,  1538,
    1539,     0,     0,   204,     0,  1540,  1541,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   261,   315,     0,     0,
       0,     0,   263,   204,     0,     0,     0,     0,     0,     0,
     490,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,   476,   476,   263,     0,   905,     0,
       0,     0,     0,   263,     0,     0,     0,     0,   432,     0,
      74,     0,     0,     0,     0,     0,     0,   343,     0,     0,
       0,     0,     0,   150,   432,     0,     0,     0,     0,     0,
     802,    76,     0,   905,   635,   343,     0,  1265,   905,   905,
       0,    79,   803,   536,     0,     0,     0,     0,   638,     0,
       0,     0,     0,     0,   637,   490,     0,     0,     0,   148,
       0,     0,   204,    65,    66,    67,    68,    69,    70,    71,
      72,  1318,   261,   490,     0,  1319,     0,  1320,  1321,  2017,
       0,    58,     0,  1321,  1321,  1321,     0,   462,     0,     0,
       0,     0,   204,     0,     0,     0,     0,     0,     0,     0,
       0,  1641,     0,     0,     0,     0,  1641,     0,    76,     0,
       0,  1760,  1813,   148,  1641,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,   382,     0,     0,
     383,     0,   384,     0,   385,     0,     0,  1783,  1784,  1785,
       0,    74,     0,     0,     0,     0,     0,   441,     0,     0,
       0,   386,     0,     0,   775,     0,     0,   957,     0,     0,
       0,  2120,    76,     0,     0,   525,     0,     0,     0,     0,
       0,   804,    79,    80,   628,   204,   204,     0,   804,   387,
     388,   465,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,  1152,     0,
     343,     0,     0,   476,     0,     0,     0,     0,     0,     0,
     398,     0,     0,    77,   399,   204,     0,   780,     0,     0,
     400,    79,    80,   401,   402,   403,   404,   263,     0,     0,
       0,     0,     0,   791,   465,   628,   794,     0,     0,     0,
     263,     0,     0,     0,   150,     0,     0,  1883,     0,     0,
       0,     0,     0,   150,     0,     0,     0,   204,     0,     0,
       0,   432,     0,     0,   263,     0,     0,     0,     0,  1937,
       0,  1321,  1641,  1321,     0,     0,     0,     0,     0,     0,
     116,   204,  1904,   116,   432,     0,     0,  1907,  1908,     0,
     148,   432,     0,   536,    65,    66,    67,    68,    69,    70,
      71,    72,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,   259,    84,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,   315,     0,
       0,     0,  1865,     0,   150,     0,   317,     0,   116,    76,
       0,     0,   824,     0,   462,     0,     0,   148,     0,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,    58,   116,     0,     0,     0,     0,   465,     0,
       0,     0,     0,   462,     0,     0,    58,   150,     0,   265,
       0,     0,     0,   116,     0,     0,   905,     0,     0,     0,
       0,     0,   204,     0,   148,  1641,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,  1447,     0,   148,   465,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     116,     0,    74,     0,     0,     0,   116,     0,     0,   116,
       0,   465,   465,   265,    58,     0,    74,     0,     0,     0,
     343,   343,   230,    76,   339,   116,   371,     0,     0,     0,
     465,     0,     0,    79,    80,     0,   802,    76,     0,     0,
     635,     0,     0,     0,     0,     0,   148,    79,   803,   436,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,   116,   436,     0,     0,   265,     0,     0,     0,     0,
     150,   150,   150,   150,    74,   150,   150,     0,     0,     0,
       0,  1637,   323,   441,     0,     0,     0,     0,    58,     0,
       0,     0,     0,   317,    75,    76,     0,     0,     0,     0,
     432,   432,     0,     0,     0,    79,    80,     0,   465,     0,
       0,     0,     0,     0,     0,   204,   116,     0,   116,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,   265,     0,     0,     0,   780,   780,     0,     0,
     263,   259,     0,     0,     0,     0,  1063,     0,    74,  1066,
       0,     0,     0,   580,     0,     0,     0,   602,   317,     0,
       0,   116,     0,   462,     0,     0,   265,     0,   230,    76,
       0,     0,   265,     0,     0,   263,     0,     0,   204,    79,
      80,     0,   116,     0,     0,     0,     0,  2067,   150,     0,
     638,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     317,     0,   116,     0,   265,   116,     0,     0,     0,     0,
     536,     0,     0,     0,     0,     0,     0,  1137,     0,   116,
       0,  1141,     0,   116,     0,  1145,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2176,     0,     0,     0,     0,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,   436,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  1637,  1798,     0,     0,    46,  1637,    47,
     432,   436,     0,     0,  1637,   148,  1637,   367,   368,    65,
      66,    67,    68,    69,    70,    71,    72,     0,    58,  1613,
       0,   465,     0,     0,     0,     0,     0,   116,     0,     0,
       0,   436,     0,   323,   150,     0,     0,   265,     0,   263,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
     693,     0,   694,   695,     0,     0,    77,     0,     0,     0,
       0,   369,     0,     0,     0,     0,     0,     0,     0,   432,
       0,     0,     0,     0,   148,     0,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,     0,   343,     0,     0,
     150,     0,     0,     0,     0,     0,     0,     0,   263,     0,
       0,     0,    74,     0,     0,   -17,     0,     0,     0,     0,
       0,     0,     0,     0,   436,   436,     0,     0,     0,   265,
     116,   150,   314,    76,     0,     0,     0,   780,     0,     0,
       0,   148,     0,    79,    80,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,   343,   343,     0,     0,
       0,     0,   116,     0,     0,   204,     0,   116,     0,     0,
     265,   116,     0,   116,     0,   204,  1798,  1798,     0,     0,
       0,     0,     0,     0,   116,     0,   116,     0,     0,  1351,
      76,  1637,     0,     0,  1637,     0,     0,     0,     0,     0,
     371,     0,   116,   436,   204,   265,     0,     0,  1384,     0,
       0,  1388,   323,     0,     0,  1392,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   432,   116,     0,     0,   265,
       0,     0,     0,   580,     0,     0,   265,     0,     0,   116,
       0,   963,     0,     0,     0,     0,     0,     0,     0,     0,
     116,     0,     0,     0,     0,     0,     0,     0,   315,    58,
       0,     0,     0,   436,     0,     0,   116,   116,     0,   436,
       0,   465,   465,    14,    15,    16,    17,    18,   436,     0,
       0,   116,   116,   116,     0,     0,     0,     0,     0,     0,
     263,   148,     0,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,  1798,     0,
       0,     0,     0,     0,     0,     0,     0,  1637,     0,    74,
     148,     0,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,    58,     0,     0,     0,     0,   436,     0,  1635,
      76,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      79,    80,   150,     0,     0,     0,     0,     0,     0,     0,
       0,   436,     0,     0,   148,     0,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,   610,     0,   436,     0,
       0,     0,     0,     0,     0,   343,     0,     0,     0,     0,
       0,     0,    74,     0,  1798,     0,     0,     0,     0,     0,
     116,   116,     0,     0,   150,     0,     0,     0,     0,     0,
       0,     0,  2120,    76,   204,     0,   525,     0,     0,   116,
    1565,     0,     0,    79,    80,     0,     0,     0,     0,  1574,
       0,     0,   150,   150,     0,  2121,   323,     0,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,  1153,     0,     0,     0,   116,     0,     0,     0,   263,
       0,     0,     0,     0,     0,     0,   100,     0,     0,   154,
       0,   150,     0,     0,     0,     0,     0,     0,   110,     0,
     265,     0,     0,     0,     0,     0,  1440,     0,   436,     0,
       0,     0,     0,   265,   932,     0,     0,   116,     0,  2121,
    2121,     0,     0,   116,   436,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,  1267,   436,     0,
     116,     0,     0,     0,   100,   175,   178,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,   204,  2121,     0,     0,     0,     0,     0,   148,   206,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   223,    14,    15,    16,    17,    18,   436,     0,   273,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
     148,   274,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,  1635,    76,     0,     0,
       0,     0,     0,  1636,     0,     0,   307,    79,    80,     0,
       0,   309,   100,     0,   310,     0,     0,     0,     0,     0,
       0,    58,     0,     0,   110,     0,     0,     0,   204,   333,
     116,   345,     0,     0,     0,     0,  1278,     0,     0,     0,
       0,     0,     0,   349,     0,     0,     0,   116,   116,     0,
       0,     0,     0,   148,     0,     0,   442,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   148,   307,   468,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1318,     0,
     469,    74,  1319,   263,  1320,     0,     0,   465,   465,     0,
     500,     0,     0,     0,     0,     0,   518,     0,     0,     0,
     116,    75,    76,     0,     0,     0,     0,  1801,     0,     0,
       0,     0,    79,    80,     0,    76,   543,     0,  1762,     0,
       0,   548,   550,   388,   206,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   559,   560,
       0,     0,     0,     0,   116,     0,     0,   571,     0,   175,
       0,   573,     0,   116,     0,     0,   574,     0,     0,   572,
       0,   436,     0,     0,   175,     0,     0,   591,    14,    15,
      16,    17,    18,   718,  1707,  1708,    77,   399,     0,   110,
     603,     0,     0,     0,   436,     0,     0,     0,     0,     0,
       0,   436,     0,   606,     0,     0,     0,     0,     0,     0,
     609,   611,     0,     0,     0,   618,     0,     0,   626,     0,
       0,   650,     0,   265,   116,     0,     0,     0,     0,     0,
     627,     0,     0,   274,     0,   657,     0,    58,     0,   657,
       0,     0,     0,     0,   116,     0,     0,   627,     0,     0,
       0,   627,     0,   333,   436,     0,   333,   148,  1267,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,   148,
    1522,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   116,   436,     0,     0,     0,   116,     0,  1801,
    1801,     0,     0,     0,     0,     0,     0,    74,     0,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,   314,    76,     0,
       0,     0,     0,     0,     0,     0,   465,    74,    79,    80,
       0,   116,   116,   307,     0,     0,     0,   626,     0,     0,
       0,     0,     0,     0,   223,   116,   116,   230,    76,   627,
     116,   116,     0,     0,     0,     0,   819,   820,    79,    80,
       0,     0,     0,     0,     0,     0,     0,     0,   148,  1830,
     200,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,  1841,   116,   116,   116,     0,     0,     0,     0,
       0,     0,  1614,     0,     0,     0,     0,     0,     0,     0,
     116,   116,   116,   116,   116,   116,   116,     0,     0,     0,
       0,     0,   265,     0,     0,     0,     0,    76,     0,     0,
     824,  1801,     0,     0,     0,     0,   468,  1875,     0,     0,
     436,   436,     0,     0,     0,     0,     0,   148,   469,   575,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   901,     0,
       0,     0,     0,   550,     0,     0,     0,   914,     0,   591,
     349,   265,     0,     0,     0,     0,     0,     0,     0,   274,
     345,   110,   100,     0,     0,     0,     0,     0,     0,  1046,
       0,     0,   469,   436,   110,     0,   333,     0,   657,   938,
       0,     0,     0,  2083,     0,     0,     0,  1801,     0,     0,
     627,   469,     0,   949,     0,     0,     0,     0,   116,     0,
       0,     0,   626,     0,     0,     0,     0,   958,     0,     0,
       0,     0,     0,     0,   627,   657,     0,     0,     0,  1939,
       0,     0,     0,  1949,     0,     0,   966,   627,  1801,     0,
       0,     0,     0,     0,     0,     0,     0,  1962,     0,     0,
       0,     0,     0,     0,     0,     0,   115,  1971,   148,     0,
     577,   578,    65,    66,    67,    68,    69,    70,    71,    72,
    1983,     0,  1985,  1986,  1987,  1988,     0,     0,     0,     0,
       0,   116,   116,   116,     0,     0,     0,     0,     0,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,  1801,  1801,     0,     0,     0,     0,     0,    77,
     436,     0,     0,     0,   115,     0,     0,    74,     0,     0,
       0,     0,     0,   468,     0,     0,     0,   116,     0,     0,
       0,     0,     0,     0,     0,   469,     0,  2120,    76,     0,
    1055,   525,     0,   265,   116,  1801,  2026,     0,    79,    80,
    2031,     0,     0,     0,     0,  2036,     0,     0,     0,   276,
       0,     0,     0,     0,   938,     0,     0,     0,     0,  1079,
       0,     0,     0,     0,     0,     0,   469,     0,     0,   436,
    2066,     0,     0,     0,     0,  1096,   468,   468,     0,     0,
       0,     0,  1108,     0,     0,     0,     0,   116,   349,   349,
     116,     0,   115,     0,     0,   468,     0,     0,     0,     0,
       0,   116,     0,     0,     0,     0,     0,   349,     0,     0,
       0,   353,     0,     0,     0,     0,     0,     0,     0,     0,
    2094,   116,     0,     0,  2097,     0,     0,     0,     0,     0,
       0,   901,     0,     0,     0,     0,   116,  2111,     0,     0,
       0,   116,   116,   349,     0,     0,   116,   116,   471,     0,
     148,     0,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,  1234,     0,     0,  1179,     0,     0,
       0,     0,     0,   468,     0,     0,   110,     0,    74,   154,
       0,     0,     0,     0,     0,   349,     0,  2147,     0,     0,
       0,   657,     0,     0,  1269,     0,   901,     0,   314,    76,
       0,  1275,   265,   627,     0,     0,   274,     0,   349,    79,
      80,     0,     0,     0,     0,   436,     0,     0,     0,     0,
       0,     0,     0,  2170,   120,     0,  2171,   120,  2173,     0,
       0,     0,     0,     0,     0,  2177,     0,     0,     0,     0,
       0,     0,     0,   345,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,   469,     0,     0,    14,    15,
      16,    17,    18,     0,  2197,     0,  2198,     0,     0,     0,
    2201,     0,  2177,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,   629,     0,
       0,   276,   148,     0,     0,  2201,    65,    66,    67,    68,
      69,    70,    71,    72,  1318,   629,   901,   120,  1319,   629,
    1320,     0,     0,     0,     0,     0,     0,    58,   349,     0,
       0,     0,     0,   901,   901,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   349,   349,     0,     0,     0,
       0,    76,   116,     0,     0,     0,     0,     0,     0,   148,
       0,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,   120,     0,     0,     0,     0,     0,
     120,     0,     0,   120,     0,   116,   468,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   349,     0,
       0,     0,     0,     0,   116,     0,     0,  1635,    76,     0,
       0,     0,     0,     0,     0,     0,     0,   629,    79,    80,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
     154,     0,   116,   116,     0,   120,   265,     0,     0,  1423,
       0,     0,     0,     0,     0,     0,     0,  1234,     0,     0,
       0,     0,     0,     0,     0,     0,  1444,  1446,  1448,   110,
     759,   760,   761,   762,   763,   764,   765,   766,   767,   768,
     769,   116,     0,     0,   215,     0,     0,  1234,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1469,     0,   110,
     120,     0,   120,     0,     0,     0,     0,   120,     0,     0,
    1482,   770,     0,     0,     0,     0,   471,  1179,     0,     0,
       0,     0,   274,     0,     0,     0,     0,     0,   148,   116,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     626,     0,     0,     0,     0,   120,     0,     0,   353,   548,
       0,     0,   627,     0,     0,     0,     0,   276,     0,   115,
       0,  1523,     0,     0,     0,     0,   120,     0,   901,   345,
     471,     0,   115,     0,     0,     0,     0,     0,     0,    77,
     349,   469,     0,     0,     0,     0,     0,     0,   629,   471,
       0,     0,     0,   121,     0,     0,   121,   148,     0,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   629,     0,     0,     0,     0,   901,   901,     0,
       0,     0,     0,     0,     0,   629,     0,     0,     0,   349,
     349,   901,   901,     0,     0,     0,   468,   468,     0,     0,
       0,   120,     0,   349,   349,   479,     0,     0,   349,   349,
     148,   121,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,   901,
     901,   901,     0,     0,     0,   120,   121,     0,     0,     0,
       0,   349,   349,   349,     0,     0,  1423,  1423,  1423,   154,
     550,     0,     0,     0,     0,     0,   121,     0,   483,     0,
       0,   120,     0,     0,   289,  1647,     0,     0,  1649,     0,
       0,     0,     0,     0,     0,     0,  1664,  1664,     0,     0,
       0,     0,     0,   471,     0,     0,     0,     0,   110,   110,
       0,     0,     0,   121,     0,     0,     0,     0,     0,   121,
       0,     0,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   345,
       0,     0,   121,     0,     0,     0,   353,   353,   120,   120,
       0,   469,     0,     0,   121,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   154,   353,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,   120,     0,   120,     0,     0,
       0,   353,     0,     0,     0,     0,     0,     0,     0,   121,
     120,   121,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,   901,   901,   901,
       0,     0,     0,   353,     0,     0,     0,     0,     0,   349,
     349,   349,     0,     0,   121,     0,     0,     0,     0,     0,
       0,   629,     0,     0,   276,     0,   353,     0,     0,     0,
       0,     0,  1815,     0,   120,   121,     0,     0,     0,     0,
       0,     0,     0,   901,  1823,     0,     0,   120,     0,     0,
     120,   120,     0,   120,     0,   349,     0,     0,     0,     0,
    1833,     0,   120,     0,     0,   120,   120,   120,     0,     0,
       0,     0,   274,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1664,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   110,     0,     0,
     121,     0,     0,   345,     0,     0,   154,     0,     0,     0,
       0,     0,     0,     0,     0,   349,     0,   901,     0,     0,
       0,     0,     0,     0,     0,     0,   353,     0,     0,   349,
       0,     0,     0,     0,   121,   120,     0,     0,     0,     0,
       0,     0,     0,   353,   353,     0,     0,     0,     0,     0,
       0,     0,   901,     0,     0,     0,     0,   901,   901,     0,
     121,     0,   468,   468,   349,     0,     0,     0,     0,   349,
     349,     0,     0,     0,   349,   349,     0,     0,     0,     0,
       0,     0,     0,  1929,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   214,     0,     0,   353,     0,     0,   225,
     226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1664,     0,   296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,     0,     0,   121,   121,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,   120,     0,     0,     0,     0,  1989,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
     121,     0,     0,     0,   121,     0,   121,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,   121,
     276,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -483,  -483,
     629,  -483,    46,     0,    47,     0,  -483,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2057,     0,
       0,     0,     0,    58,     0,     0,     0,     0,   353,   471,
     627,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,   121,
     121,   468,   121,     0,     0,     0,     0,    63,    64,     0,
       0,   121,     0,   349,   121,   121,   121,     0,   125,     0,
    1664,   125,     0,     0,     0,     0,   599,   353,   353,     0,
       0,     0,   110,    74,     0,     0,     0,     0,     0,     0,
       0,   353,   353,     0,     0,     0,   353,   353,  1664,  2057,
       0,     0,     0,     0,     0,     0,    77,   322,     0,     0,
     110,   627,     0,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,     0,     0,   353,
     353,   353,     0,     0,     0,     0,     0,  1664,     0,     0,
       0,     0,     0,     0,   121,     0,     0,     0,     0,   110,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2163,     0,     0,     0,
       0,   125,     0,     0,     0,     0,   115,   115,   120,     0,
       0,     0,     0,     0,     0,   901,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   120,     0,   349,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   125,     0,
       0,     0,     0,     0,   125,     0,     0,   125,   120,     0,
       0,     0,     0,     0,     0,   120,   800,     0,   801,     0,
       0,     0,     0,     0,     0,     0,     0,   817,   818,   471,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   125,
       0,   121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,   125,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,   353,   353,   353,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   911,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   353,     0,     0,     0,     0,     0,     0,
     125,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     276,     0,     0,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,   120,   120,   120,   120,   120,
     120,     0,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   353,   120,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   125,     0,   353,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,   353,     0,     0,     0,     0,   353,   353,     0,
       0,     0,   353,   353,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   125,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,   120,     0,   121,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,   121,     0,     0,     0,     0,     0,
    1094,   115,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,   685,     0,     0,     0,   410,   691,
       0,     0,   125,   125,     0,     0,     0,   121,   700,   701,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   410,   125,     0,     0,     0,   125,
     121,   125,     0,     0,     0,     0,     0,  1175,  1176,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   120,     0,
    1240,  1241,  1242,     0,   410,  1244,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   629,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,   125,     0,
       0,   353,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,     0,   125,   125,     0,   125,     0,     0,
     115,     0,     0,     0,     0,   120,   125,  1314,     0,   125,
     125,   125,     0,   121,   121,   121,   121,   121,   121,   121,
       0,     0,     0,     0,     0,     0,     0,     0,   115,   629,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,   121,     0,     0,     0,     0,     0,
       0,     0,     0,  1335,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,     0,     1,   120,
    1359,   147,     0,     0,     0,     0,     0,     0,     0,  1363,
    1364,  1365,  1366,     0,     0,   353,     0,  1371,  1372,     0,
       0,     0,     0,     0,     0,     0,     0,  1380,     0,     0,
       0,   121,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1401,     0,     0,  1404,
       0,  1405,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   203,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,   410,   410,
     410,   410,   410,   410,   410,   410,   410,   410,   410,   410,
     410,   410,   410,   410,   410,   410,   125,  1465,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   125,   125,   121,     0,   302,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1488,     0,
       0,     0,     0,     0,     0,  1492,     0,  1494,  1496,     0,
       0,     0,     0,     0,     0,     0,     0,   121,     0,  1505,
       0,  1506,     0,  1507,     0,  1509,     0,   410,   120,     0,
    1517,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   302,     0,
       0,     0,  1562,     0,     0,   120,     0,     0,     0,  1569,
    1570,     0,     0,     0,   121,     0,   551,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   302,     0,     0,     0,
       0,     0,     0,  1593,     0,     0,     0,   302,     0,     0,
    1598,     0,     0,     0,  1599,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   583,   587,     0,     0,     0,
       0,     0,   594,   595,     0,     0,     0,     0,     0,     0,
       0,     0,  1618,     0,     0,     0,     0,     0,     0,     0,
     605,     0,     0,     0,     0,     0,   225,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     624,     0,     0,     0,     0,     0,   410,     0,   121,     0,
       0,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,     0,     0,     0,     0,     0,  1701,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,     0,     0,     0,     0,     0,   717,     0,     0,
       0,     0,     0,     0,   410,     0,     0,     0,     0,     0,
       0,     0,   125,  1730,     0,     0,     0,     0,     0,   125,
       0,  1735,     0,  1737,     0,     0,     0,     0,   758,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   796,     0,     0,     0,   799,     0,
       0,     0,   125,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1768,  1769,     0,     0,     0,   821,     0,     0,
       0,   822,   823,     0,     0,   826,     0,  1774,  1775,     0,
    1776,     0,     0,     0,     0,     0,     0,     0,     0,  1780,
     840,   841,   842,   843,     0,   125,     0,     0,     0,     0,
    1786,  1787,  1788,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,   872,     0,     0,     0,   121,     0,     0,
       0,     0,   876,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,   302,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,     0,     0,     0,   121,   918,     0,     0,     0,     0,
       0,   169,   583,   169,     0,     0,     0,     0,   924,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   125,   125,
     125,   125,   125,   125,   125,     0,     0,   410,     0,     0,
       0,     0,   941,   946,     0,   373,     0,     0,  1884,  1885,
       0,     0,     0,     0,     0,     0,   410,     0,   125,   125,
       0,  1893,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,   410,   410,   410,     0,     0,     0,     0,
     410,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1917,  1918,
    1919,     0,     0,     0,   410,     0,     0,   169,     0,     0,
       0,   169,     0,   988,   169,   169,     0,     0,   169,     0,
       0,   169,   169,     0,   169,     0,   169,     0,     0,   183,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,   410,   410,    19,     0,    20,   125,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   252,   253,     0,   254,    46,     0,    47,     0,
     255,     0,  1051,    49,    50,    51,    52,    53,    54,    55,
       0,   169,     0,     0,   169,     0,     0,  1068,  1993,     0,
       0,  1069,     0,     0,     0,     0,     0,     0,     0,     0,
     941,     0,     0,     0,     0,     0,     0,   169,   169,  2003,
       0,     0,  2004,  2005,     0,     0,     0,     0,     0,  2007,
       0,     0,  1109,   169,     0,     0,     0,     0,     0,     0,
       0,  1118,     0,     0,     0,  1274,     0,  1121,   125,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -458,
       0,     0,   125,     0,     0,     0,     0,     0,     0,   382,
       0,     0,   383,     1,   384,     0,   385,  1165,     0,     0,
       0,     1,  -458,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,   386,     0,     0,     0,   125,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       1,     0,     0,     0,     0,     0,     0,     0,   125,     0,
     167,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,   125,
    2119,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,  1294,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,   169,
       0,     0,   400,   461,    80,   401,   402,   403,   404,     0,
       0,     0,     0,   294,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   300,     0,   301,     0,
     360,     0,     0,  2160,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2179,
       0,     0,     0,   125,     0,     0,   458,   360,     0,  1342,
       0,     0,     0,  1343,  2188,     0,     0,   410,   373,     0,
     941,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1356,  2199,     0,     0,     0,     0,     0,  1357,   528,     0,
       0,     0,     0,     0,     0,   528,  1361,     0,  1362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
     169,     0,     0,     0,     0,     0,     0,     0,     0,   530,
     531,     0,   169,   535,     0,     0,   538,   539,     0,   541,
       0,   542,     0,  1394,     0,     0,     0,  1395,     0,     0,
       0,  1396,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,   528,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,   639,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   660,     0,
       0,     0,   621,   622,     0,     0,     0,     0,     0,     0,
       0,     0,   125,     0,     0,     0,     0,     0,   654,     0,
       0,     0,     0,     0,     0,   169,   169,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
     125,  1504,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,   169,   169,     0,   169,     0,
     169,   169,     0,     0,     0,     0,  1529,     0,     0,   528,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
       0,     0,   410,     0,     0,   528,   792,     0,   528,   795,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
     639,     0,   169,     0,     0,     0,   169,     0,     0,     0,
     169,     0,     0,   790,     0,     0,     0,     0,     0,     0,
       0,     0,   410,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   528,     0,     0,     0,   528,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1602,     0,     0,     0,  1603,     0,     0,     0,  1604,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     169,   169,     0,     0,   868,   360,     0,     0,     0,     0,
       0,     0,     0,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1648,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   410,     0,
     410,     0,     0,     0,     0,     0,   528,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   936,   360,     0,
       0,     0,     0,     0,     0,     0,     0,  1718,   639,   410,
    1721,     0,   639,     0,     0,     0,     0,     0,     0,   954,
       0,   360,     0,     0,     0,     0,     0,     0,  1732,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   952,   953,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   960,     0,     0,
       0,     0,     0,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1767,     0,     0,     0,     0,     0,   211,     0,     0,
    1772,   410,     0,     0,  1773,     0,     0,     0,     0,     0,
       0,     0,     0,   269,     0,     0,   169,     0,  1777,  1778,
       0,     0,     0,   169,     0,     0,   169,     0,     0,     0,
     169,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,  1721,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   528,   528,
       0,     0,     0,   211,     0,     0,     0,   324,   528,  1064,
       0,   528,  1067,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,   360,     0,     0,   639,     0,   639,   639,
    1057,  1058,     0,     0,     0,   639,  1062,     0,     0,     0,
       0,     0,     0,   211,     0,   360,   360,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   478,  1083,     0,   482,
    1086,  1087,     0,  1090,   360,  1092,  1093,     0,   528,     0,
       0,     0,   528,     0,     0,     0,     0,     0,     0,   528,
    1138,     0,     0,   528,  1142,     0,     0,   528,  1146,     0,
    1878,  1879,     0,     0,     0,  1150,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1136,   169,     0,
       0,  1140,   211,     0,     0,  1144,   169,   169,     0,     0,
       0,     0,     0,     0,     0,     0,   269,   711,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,   528,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   482,     0,     0,     0,
       0,     0,     0,   639,     0,   169,   211,     0,     0,     0,
       0,     0,     0,     0,   169,  1259,  1260,   169,     0,   169,
     169,     0,     0,     0,     0,     0,   632,     0,   649,  1276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   434,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   463,     0,     0,     0,  1973,     0,
       0,     0,     0,     0,     0,   169,     0,   491,     0,   491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   715,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1721,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   880,   882,     0,     0,     0,   528,
    2006,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   639,   639,     0,     0,
       0,     0,     0,   639,     0,     0,     0,     0,     0,     0,
    2025,     0,     0,     0,     0,   632,     0,     0,     0,     0,
     169,   816,     0,     0,     0,     0,     0,     0,     0,     0,
    1276,     0,     0,   600,     0,     0,     0,  2053,     0,     0,
    2054,     0,     0,     0,     0,   360,     0,     0,     0,     0,
     528,  1385,     0,   528,  1389,     0,     0,   528,  1393,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1376,     0,     0,     0,     0,     0,     0,  1383,     0,
       0,  1387,     0,     0,     0,  1391,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   211,   211,
       0,     0,     0,     0,   478,     0,     0,     0,   169,     0,
       0,     0,     0,   711,     0,     0,     0,     0,     0,   711,
       0,     0,     0,     0,     0,     0,     0,     0,   711,     0,
       0,     0,     0,     0,     0,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   711,     0,  2140,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,   478,   169,   940,
       0,     0,     0,  1043,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,     0,   639,  1513,     0,     0,     0,
     632,     0,     0,     0,     0,     0,     0,     0,     0,   491,
       0,     0,     0,     0,     0,   491,     0,     0,   360,     0,
     837,     0,     0,  1511,   211,     0,     0,     0,     0,     0,
       0,  1520,  1521,     0,     0,     0,     0,   715,     0,     0,
     715,   715,     0,   715,     0,     0,     0,     0,     0,     0,
       0,     0,   715,     0,   169,   715,   715,   715,     0,     0,
       0,     0,   528,  1566,     0,     0,     0,     0,     0,     0,
       0,   528,  1575,     0,   639,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,   360,     0,     0,     0,
    1564,     0,     0,     0,     0,     0,     0,     0,     0,  1573,
       0,     0,  1577,     0,  1580,  1581,     0,     0,     0,     0,
       0,   478,     0,     0,     0,     0,   917,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   211,     0,     0,     0,     0,
       0,   169,   169,     0,     0,   463,     0,     0,     0,   373,
    1609,     0,   478,     0,   169,     0,     0,     0,   948,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   478,   478,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   478,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   982,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,     0,
       0,     0,     0,     0,     0,  1715,     0,   837,  1002,     0,
       0,  1004,     0,  1006,     0,     0,     0,     0,     0,  1015,
       0,  1020,  1015,     0,     0,   639,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,   478,     0,     0,     0,     0,     0,     0,   211,  1048,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   816,  1050,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1059,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   463,     0,     0,
    1048,     0,     0,  1577,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,  1112,   169,   528,
     491,     0,  1782,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   528,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   257,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,  1151,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,   711,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,   434,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   169,     0,  1266,
    1268,     0,     0,     0,     0,     0,     0,   463,     0,  1876,
       0,     0,     0,     0,   478,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1015,     0,     0,     0,     0,
       0,   360,   360,     0,     0,     0,     0,    74,     0,  1048,
       0,     0,     0,     0,     0,     0,     0,  1307,     0,     0,
       0,   528,   528,     0,  1015,     0,     0,   715,     0,     0,
      77,   258,     0,     0,     0,     0,     0,   528,    79,    80,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1933,  1934,   715,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1938,
     491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   269,     0,     0,
       0,  1515,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,   211,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   632,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   382,     0,   491,   383,  1375,
     384,  1378,   385,     0,     0,     0,     0,   365,     0,     0,
       0,   715,     0,   528,     0,     0,     0,     0,    58,   386,
       0,   528,     0,     0,     0,     0,     0,  1655,     0,     0,
    1658,  1672,     0,     0,     0,     0,  1679,     0,     0,     0,
    1683,     0,  1685,  2015,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,   478,   478,     0,     0,    74,     0,
    1455,  1455,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,   528,  2084,     0,   398,   528,
       0,    77,   399,     0,     0,     0,     0,     0,   400,  1516,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,     0,     0,  2082,   715,   715,   715,     0,     0,   715,
     715,     0,     0,     0,     0,     0,   482,     0,     0,     0,
     528,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1508,     0,     0,     0,     0,     0,  1518,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,   463,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   491,     0,     0,   269,     0,     0,     0,  1789,
       0,     0,     0,     0,   528,   528,     0,  1015,     0,     0,
     837,     0,     0,     0,     0,   387,   388,   365,   485,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,   528,     0,  1832,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1601,     0,  1847,  1849,   398,    76,     0,   486,
     487,     0,     0,     0,   488,     0,   400,    79,    80,   401,
     402,   403,   404,  1611,  1612,     0,     0,     0,     0,  1658,
       0,     0,     0,     0,     0,  1867,   382,     0,     0,   383,
       0,   384,     0,   385,     0,     0,     0,     0,     0,     0,
       0,     0,  1015,     0,     0,     0,     0,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     491,     0,     0,   837,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,   387,   388,
       0,   389,   390,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   391,   392,   379,     0,   393,   394,   395,
       0,   396,   397,     0,     0,     0,     0,   269,     0,    74,
       0,     0,     0,     0,     0,  1002,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1733,  1734,     0,     0,   398,
    1310,     0,    77,   399,     0,   491,     0,  1311,     0,   400,
      79,    80,   401,   402,   403,   404,     0,     0,  1947,     0,
       0,     0,     0,   491,     0,   837,     0,  1950,     0,  1952,
       0,   365,  1957,  1961,     0,  1672,     0,     0,     0,     0,
    1967,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   715,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     478,   478,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   434,     0,     0,     0,     0,  1814,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   269,     0,  2042,     0,
       0,     0,     0,  2047,  2049,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   257,     0,  2069,     0,     0,  1861,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -483,  -483,     0,  -483,    46,
       0,    47,  2098,  -483,  2101,     0,     0,  2103,  2105,     0,
       0,  1895,  2108,  2110,  1897,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1924,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   715,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,  2154,  2156,  2158,  2200,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   478,
       0,     0,  1439,    77,   322,     0,     0,     0,     0,     0,
       0,    79,    80,     0,     0,     0,     0,     0,  2183,  2185,
    2187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,     0,     0,   715,     0,     0,
     482,     0,     0,     0,     0,  1181,     0,   386,    -2,     0,
    1183,  -244,  -244,  1184,  1185,  1186,  1187,  1188,  1189,  1190,
    1191,  1192,  1193,  1194,  1195,  -338,  -338,  1196,  1197,  1198,
    1199,  1200,     0,  1201,     0,   387,   388,     0,   485,   390,
    1202,  1203,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,  1204,   393,   394,   395,  2200,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,  1439,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -244,  1205,     0,     0,    77,
     399,     0,  1015,     0,   298,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,   382,     0,     0,   383,     0,
     384,  -185,   385,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1181,     0,   386,
      -2,     0,  1183,  -245,  -245,  1184,  1185,  1186,  1187,  1188,
    1189,  1190,  1191,  1192,  1193,  1194,  1195,  -338,  -338,  1196,
    1197,  1198,  1199,  1200,     0,  1201,     0,   387,   388,     0,
     485,   390,  1202,  1203,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,  1204,   393,   394,   395,  1874,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,  1439,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -245,  1205,     0,
       0,    77,   399,     0,     0,     0,   298,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,   382,     0,     0,
     383,     0,   384,  -185,   385,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1181,
       0,   386,    -2,     0,  1183,     0,     0,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  -338,
    -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,   387,
     388,     0,   485,   390,  1202,  1203,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,  1204,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1205,     0,     0,    77,   399,     0,     0,     0,   298,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,     0,  -185,     4,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1180,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   382,     0,    46,   383,    47,   384,     0,   385,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1181,    58,  1182,    -2,     0,  1183,
       0,     0,  1184,  1185,  1186,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  1195,  -338,  -338,  1196,  1197,  1198,  1199,
    1200,     0,  1201,     0,   387,   388,    61,   485,   390,  1202,
    1203,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,  1204,   393,   394,   395,     0,   396,   397,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,  1205,     0,     0,    77,   430,
       0,     0,     0,   298,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,     0,     0,
    -185,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1180,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1181,
      58,  1182,    -2,     0,  1183,     0,     0,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  -338,
    -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,   387,
     388,    61,   485,   390,  1202,  1203,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,  1204,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1205,     0,     0,    77,   430,     0,     0,     0,   298,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,     0,  -185,     4,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   382,     0,    46,   383,    47,   384,     0,   385,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   386,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,   388,    61,   389,   390,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,     0,   393,   394,   395,     0,   396,   397,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1666,
    1667,  1668,     0,     0,     0,   398,  1669,  1670,    77,   430,
       0,     0,     0,     0,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,     0,     0,
    1671,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     388,    61,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1666,  1667,  1668,     0,     0,     0,
     398,  1669,     0,    77,   430,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,     0,  1671,     4,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   382,     0,    46,   383,    47,   384,     0,   385,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   386,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,   388,    61,   389,   390,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,     0,   393,   394,   395,     0,   396,   397,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   398,     0,  1657,    77,   430,
       0,     0,     0,     0,     0,   400,    79,    80,   401,   402,
     403,   404,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,   388,    61,   389,   390,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,     0,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   398,     0,     0,    77,   430,     0,     0,     0,     0,
       0,   400,    79,    80,   401,   402,   403,   404,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     460,     0,     0,     0,     0,     0,   400,   461,    80,   401,
     402,   403,   404,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,   388,     0,   389,   390,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,     0,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   398,     0,     0,    77,  1263,     0,     0,     0,     0,
       0,   400,  1264,    80,   401,   402,   403,   404,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,   388,     0,   389,   390,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,     0,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   398,     0,     0,    77,   460,     0,     0,     0,     0,
       0,   400,    79,    80,   401,   402,   403,   404,  2024,     0,
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
      -2,    -2,  2052,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,     0,    -2,     0,     0,     0,     0,    -2,    -2,
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
      77,   258,     0,     0,     0,  -812,     0,     0,    79,    80,
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
       0,     0,  -407,     0,     0,     0,    77,    78,     0,  1414,
       0,  1415,     0,     0,    79,    80,  1416,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1418,     0,     0,
       0,    77,   978,     0,  1414,     0,  1415,     0,     0,    79,
      80,  1416,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1621,     0,     0,     0,    77,   978,     0,  1414,
       0,  1415,     0,     0,    79,    80,  1416,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1622,     0,     0,
       0,    77,   978,     0,  1414,     0,  1415,     0,     0,    79,
      80,  1416,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1623,     0,     0,     0,    77,   978,     0,     0,
       0,     0,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -483,
    -483,     0,  -483,    46,     0,    47,     0,  -483,     0,     0,
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
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
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
       0,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,  1103,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -676,    77,
     342,     0,     0,     0,     0,     0,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   340,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   341,
      77,   342,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
    1913,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   342,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   340,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,  1915,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,   342,     0,     0,     0,     0,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
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
       0,     0,     0,    77,   322,     0,     0,     0,     0,     0,
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
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,   342,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    58,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,    63,    64,     0,     0,
       0,     0,  1439,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,    77,   258,     0,     0,     0,
       0,     0,     0,    79,    80,  1181,     0,   386,    -2,     0,
    1183,  1940,  1941,  1184,  1185,  1186,  1187,  1188,  1189,  1190,
    1191,  1192,  1193,  1194,  1195,     0,     0,  1196,  1197,  1198,
    1199,  1200,     0,  1201,     0,   387,   388,     0,   485,   390,
    1202,  1203,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,  1204,   393,   394,   395,  1439,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1205,     0,   382,    77,
     399,   383,     0,   384,   298,   385,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
    1181,  -185,   386,    -2,     0,  1183,     0,     0,  1184,  1185,
    1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,
       0,     0,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,
     387,   388,     0,   485,   390,  1202,  1203,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,  1204,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1205,     0,     0,    77,   399,     0,     0,     0,   298,
       0,   400,    79,    80,   401,   402,   403,   404,     0,     0,
       0,     0,     0,     0,     0,     0,  -185,    14,    15,    16,
      17,    18,    19,   702,    20,   703,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   704,
       0,     0,     0,     0,  1195,     0,  -338,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1205,     0,     0,    77,
     705,     0,     0,     0,   298,     0,   400,    79,    80,   706,
     707,   403,   404,    14,    15,    16,    17,    18,    19,   702,
      20,   703,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   704,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   705,     0,     0,     0,
     298,     0,   400,    79,    80,   706,   707,   403,   404,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
     429,    77,   430,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   382,     0,    46,   383,    47,   384,     0,   385,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   386,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,     0,   396,   397,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   398,     0,     0,    77,   705,     0,
       0,     0,   298,     0,   400,    79,    80,   401,   402,   403,
     404,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     398,     0,     0,    77,   430,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     460,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,   340,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,   588,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   257,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    63,    64,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,    77,
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
       0,     0,     0,  -814,     0,     0,    79,    80,    14,    15,
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
       0,    47,     0,     0,     0,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   900,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,  -689,    77,    20,     0,    21,    22,    23,    24,
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
     322,     0,     0,     0,     0,     0,     0,    79,    80,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   340,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1824,
       0,     0,     0,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
      77,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   340,    49,    50,    51,
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
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   984,    77,   978,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1533,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   978,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    77,   305,     0,
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
      78,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   456,     0,     0,     0,     0,     0,     0,    79,
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
       0,     0,     0,    77,   342,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    77,   305,     0,     0,     0,
       0,     0,     0,    79,    80,    14,    15,    16,    17,    18,
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
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   456,     0,
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
     322,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   978,     0,     0,     0,     0,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,    58,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,    63,    64,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   978,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    77,     0,     0,    14,    15,
      16,    17,    18,    79,    80,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,    58,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,    63,    64,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,     0,    63,    64,     0,     0,     0,     0,    79,    80,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,     0,     0,     0,
       0,    77,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,   461,    80,   401,   402,   403,   404,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,   388,     0,   389,   390,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,     0,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   398,     0,     0,    77,   399,     0,     0,     0,     0,
       0,   400,    79,    80,   401,   402,   403,   404,    14,    15,
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
      72,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      77,    47,     0,     0,     0,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    63,    64,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,   382,     0,     0,   383,     0,
     384,     0,   385,    77,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,   382,     0,     0,   383,     0,   384,    74,   385,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,  1666,  1667,  1668,     0,   386,     0,   398,  1848,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,   388,     0,   389,   390,  1955,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,     0,   393,   394,   395,     0,   396,   397,   382,
       0,     0,   383,     0,   384,    74,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1666,
    1667,  1668,     0,   386,     0,   398,  1956,     0,    77,   399,
       0,     0,     0,     0,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
     488,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,   836,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,     0,   298,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  1011,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,  1042,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  1377,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,  1449,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,     0,  1524,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,     0,  1946,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  1951,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  1960,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2046,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2048,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2100,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2102,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2104,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2107,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2109,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2153,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2155,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2157,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2182,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2184,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2186,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   684,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   690,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     699,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,     0,     0,    77,   399,     0,
       0,     0,     0,     0,   400,   916,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,     0,     0,   400,   461,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,  2041,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   184,
       0,   185,   186,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   693,     0,   694,   695,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -482,  -482,     0,  -482,    46,     0,    47,     0,  -482,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,    58,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,   181,     1,     4,   167,   256,   731,   302,   230,    75,
     166,   230,  1190,   166,    75,    75,  1223,     1,   177,   271,
       4,   488,   716,    97,   230,   182,    75,   804,   230,   925,
     230,   230,  1205,   647,   218,   634,     4,   345,   909,    97,
     372,  1165,   251,    84,   230,    77,   141,   398,   909,   230,
     638,  1361,  1362,  1205,   932,    56,    57,    75,    59,    59,
      59,   230,   166,     1,  1017,     1,   360,   775,    97,    75,
     364,    75,   634,   709,    75,    59,   400,   634,   802,     1,
     545,   546,   804,    84,   230,    84,  1798,  1441,  1797,   343,
     968,    92,   314,  1046,  1797,   314,    97,     1,    97,   100,
       4,   100,  1797,   104,     1,   104,   360,   202,   314,   150,
     364,   802,   314,   802,   314,   314,   100,     1,   802,   556,
     104,    59,   233,    59,   235,  1940,   192,    90,   314,    75,
     567,   242,   192,   314,  1676,     1,   104,    59,     4,   821,
     822,   142,     1,   192,   145,   314,   147,   147,   147,   893,
      77,    78,   153,   134,    75,    59,  1109,   231,   840,   160,
     468,     0,    72,   147,   230,   337,   167,    72,   314,   230,
     230,   608,   151,   231,   192,    59,     1,     0,   922,   158,
      84,   230,   104,  1441,  1442,    98,   192,   168,   192,   152,
     191,   192,   191,    59,  1672,   805,   822,     0,   309,   310,
      59,   811,   231,   802,   376,   206,   671,   206,   462,   147,
     118,   147,   230,     1,   840,   216,   804,  1929,   259,    89,
     221,    72,  1944,   152,   230,   147,   230,    84,  1162,   230,
     231,   177,   231,   160,    59,  1169,   918,   141,   104,   343,
     802,   315,   152,   147,   640,   802,   150,   152,   314,   974,
     251,   156,   240,   314,   314,     1,   177,   315,   259,    72,
     259,   293,   159,   147,   177,   314,    99,   398,   269,   270,
     118,    59,   273,  2088,   273,   100,    10,   529,  1003,   280,
     268,   147,   323,    89,   506,   537,   315,   506,   147,   273,
     160,   279,   918,   294,   295,   152,   297,   152,   202,   594,
     506,  1843,  1079,   466,   506,   156,   506,   506,   156,   182,
     494,  1182,  1246,   314,   315,    89,   315,   782,   626,   152,
     506,  1182,   147,   324,  1476,   506,    89,  1479,  1480,   623,
     331,   332,    20,   134,   543,   336,   668,   506,   133,   152,
     549,  1212,   650,   156,  2056,   152,  1700,  1055,  1072,   657,
     605,  1212,   158,   257,   160,   259,   992,  1079,   462,   147,
     506,   949,   686,   458,   302,    72,   660,   168,   239,   623,
    1848,  1849,  1174,   374,    84,   246,   377,   378,   729,   601,
     302,  1072,   601,  1072,     4,   152,   160,    97,  1072,  1174,
     100,   432,   152,   647,   104,   601,   267,   160,   302,   601,
    1278,   601,   601,  1319,   146,  1529,   660,   278,  1286,  2121,
     806,  2120,   158,   159,   810,   601,   176,  2120,   302,   323,
     601,  2143,  1166,   819,   820,  2120,   168,   222,   162,   123,
      77,    78,   601,   167,    61,    62,    56,    57,    10,  1121,
     506,   442,  1700,   442,   518,   152,   506,    72,   273,   156,
      72,  2163,   605,   890,   486,   601,   158,   506,  2180,   622,
     518,   155,   133,  1073,   465,   466,    75,  1777,  1778,   155,
    1174,    72,    92,  1072,  1408,   177,   477,   478,  1956,  1957,
      72,   191,    91,   374,   152,   486,   377,   488,   506,   518,
     176,  1079,  2091,  1671,   134,  1121,   206,   152,  1676,   108,
     506,   605,   506,    58,   175,   506,    61,    62,   303,    64,
    1072,    75,    72,   160,   302,  1072,   590,   518,  2117,   518,
     345,   231,   142,   163,   164,   145,    90,   152,   432,   603,
     152,   156,   590,   160,   156,   601,   152,  1710,    72,   110,
     160,   601,   543,   647,   160,   603,  2145,   167,   549,   259,
     177,   152,   601,   684,   458,   156,   687,   688,  1710,   690,
     152,   590,   133,   273,   156,   177,   821,   822,   699,   160,
     160,   702,   703,   704,   603,   110,   756,   167,   566,   151,
    1441,  1442,  1049,   154,    72,   840,   177,   588,  1846,   590,
     162,   590,   152,   901,  1547,   167,   156,   152,   133,    72,
     601,   221,   603,   398,   603,   151,   607,   607,   607,  1411,
    1412,  1413,   158,    72,    75,    72,   617,   109,   152,   157,
     621,   622,   156,   607,   106,   107,  1411,  1412,  1413,    90,
     938,   177,   160,   842,    72,  1329,  1552,  1553,  1554,   167,
     132,   804,   936,   468,   158,   829,    72,   128,   129,   269,
     270,  1265,   177,   654,    72,     3,   594,   158,   815,   868,
     280,  1271,   533,   918,   152,  1843,   667,    72,   156,   607,
     592,   607,   594,   155,   294,   295,   177,   297,    69,   152,
      72,   155,   936,   156,   588,   607,   624,   558,    75,   869,
     594,   172,   173,   152,   565,   152,  2006,   156,   569,   156,
    1096,    72,   149,   607,   324,    92,  1431,  1411,  1412,  1413,
     594,   331,   332,   160,   152,   154,   336,   158,   156,   158,
     721,   108,   723,   607,   725,   757,   152,   158,   729,   176,
     156,   732,   442,   988,   152,  1631,   177,   532,   156,     3,
    1350,   607,   158,   158,   158,   540,   177,   152,   607,   158,
     158,   156,   158,   168,   374,   132,   757,   377,  1452,   280,
     152,   177,   557,   177,   156,   158,   591,  2025,   177,   177,
       3,   177,   158,   568,   295,   152,  1954,   160,  1985,   156,
     152,  1988,   607,   146,   147,   148,   163,   164,  1966,   153,
    1255,   177,   158,   154,   177,  2053,   160,   960,   159,   176,
       3,   802,   168,   804,   160,   168,   594,  1760,   518,  1762,
      13,    14,    15,    16,    17,   816,   163,   158,   152,   607,
     158,   160,   823,   170,   171,   650,   160,   168,   829,   160,
     168,   832,   156,   167,  2092,   988,   158,   161,   152,  1700,
     841,   842,   843,   158,  1311,   465,   168,  1725,  1726,   152,
    1544,   154,   154,   168,  1205,   156,  1108,   477,   478,   176,
     161,   799,  2040,   165,   166,   152,  1121,   868,   174,    72,
     132,   104,   159,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   152,   988,    47,    48,   154,    50,   684,
     152,   151,   159,    55,   156,   690,   106,   107,   158,   132,
     154,   163,   164,   152,   699,   159,   907,   908,   909,   154,
     118,    62,  1624,   132,   159,  1078,  1079,  1629,    58,   152,
     153,    61,    62,   718,    64,   909,   159,  1259,  1260,   154,
     163,   164,  1116,   152,   155,   156,   154,   156,   159,   154,
     924,   156,   815,  1531,   163,   164,   152,   154,   154,   100,
     156,   158,    22,   826,   154,   154,   924,   154,   158,   960,
     111,  1269,   113,   152,   115,   154,     3,   156,   588,   154,
     152,   909,   973,   909,   156,   154,    13,    14,    15,    16,
      17,   158,  1449,   160,   152,  1846,   152,   909,   146,   147,
     148,   158,    13,    14,    15,    16,    17,   617,   106,   107,
     158,   621,   622,   154,   152,   909,   157,   158,  1009,   152,
     168,  1265,  1890,   152,   152,   154,  1017,   156,   156,   177,
     924,   892,   132,  1274,   152,   909,   154,   158,   156,    13,
      14,    15,    16,    17,   654,    72,  1147,   154,   158,  1294,
    1249,   158,   152,   909,   158,  1046,   156,   667,  1049,   158,
     909,    72,   154,   163,   164,   206,   158,  1524,   924,   132,
      13,    14,    15,    16,    17,  1416,   111,   112,   113,   114,
     115,  1072,   154,   155,  1205,  1542,   901,  1078,  1079,   152,
     875,   165,   166,   156,   909,   146,   147,   148,    72,   914,
     163,   164,   887,   126,   127,   154,   891,  1256,  1257,   158,
     895,   721,   152,   723,    99,   725,  1361,   168,  1109,   729,
     154,   132,   732,   938,   158,    89,   177,   152,  1996,    72,
     160,   909,   273,   157,   275,   276,   154,  1284,   154,   160,
     158,   152,   158,  1174,   152,   156,   151,   757,   156,   118,
    1293,  1294,   163,   164,   160,  1397,  1441,   154,   132,   176,
      13,    14,    15,    16,    17,   152,   307,   157,   158,   156,
     154,  1265,   313,   167,  2025,   152,   154,  1298,   152,  1042,
     158,   152,   156,   146,   147,   148,   130,   131,   154,   163,
     164,  1165,   158,  1184,   152,   158,  1187,  1188,  1189,  1521,
    1294,   152,  2053,   158,   345,   168,   816,  1165,  1182,   169,
     351,   152,   353,   823,   177,   156,  1362,   157,   158,    72,
     154,  1212,  1369,  1370,   158,   154,   154,  1218,   154,   158,
     158,   841,   158,   843,   164,   154,   163,   164,  1212,   158,
    1231,  2092,   162,  1234,  1235,  1234,  1235,  1238,   389,   154,
     736,   737,   738,   158,  1182,  1118,  1182,   174,  1249,  1504,
    1234,  1235,   132,   154,   154,   154,  1981,   158,   158,   158,
    1182,  1165,    13,    14,    15,    16,    17,  1235,   154,   154,
    1174,   134,   158,   158,  1212,   154,  1212,   155,  1182,   158,
     154,  1282,   154,   154,   666,   154,   158,   907,   908,   909,
    1212,   442,   154,   154,  1295,   134,   158,   158,  1182,  1165,
      13,    14,    15,    16,    17,   154,   154,  2125,  1212,   158,
    1311,  2129,   154,  1235,   158,   159,  1182,   468,  1319,   470,
     471,    72,   154,  1182,   743,   744,   745,   746,  1212,  1621,
    1622,  1623,   157,   158,   485,  1171,  1172,  1173,   157,   158,
     960,  1642,  1643,  1644,   203,   156,  1212,   157,  1422,   134,
    1351,  1504,   159,  1212,   157,   158,   158,  1182,   159,    72,
     203,   157,   158,   152,  1422,   157,   158,   518,   154,  1235,
    1411,  1412,  1413,  1168,   154,  1416,  1417,    13,    14,    15,
      16,    17,    18,   134,    91,    92,  1181,  1212,   157,   158,
     157,   158,   543,  1422,  1182,   157,   158,   548,   152,   550,
    1504,   157,   158,  1198,   154,  1700,   154,  1739,   154,  1234,
    1205,  1284,   154,  1635,   157,   158,  1635,   157,   158,   132,
     571,  1422,   573,   574,  1212,  1426,   157,   158,  1429,  1635,
    1414,   157,   158,  1635,   157,  1635,  1635,   157,   158,   152,
     591,   157,   158,   156,  1269,   827,  1414,   176,  1449,  1635,
     163,   164,   603,   154,  1635,   157,   158,  1441,  1442,  1332,
      13,    14,    15,    16,    17,    18,  1635,   157,   158,   158,
     159,  1472,  1473,   157,   158,   626,   156,   628,   629,   157,
     158,  1482,   154,  1482,   343,    77,    78,   346,   154,  1635,
     158,   159,    75,  1320,  1321,   160,  1369,  1370,  1482,   650,
     651,   360,   160,  1441,  1442,   364,   657,  1411,  1412,  1413,
    1414,  1415,  1416,  1417,    97,   739,   740,   160,  1698,  1441,
    1442,   160,  1777,  1524,  1234,  1235,     4,     5,     6,     7,
       8,     9,    10,    11,    12,   747,   748,  1441,  1442,   741,
     742,  1542,  2070,  2071,   160,  1529,  1547,    70,  1414,   157,
    1624,  1552,  1553,  1554,   152,  1629,    78,  1441,  1442,  1479,
    1480,  1529,  1718,  1637,  1184,  1718,  1624,  1187,  1188,  1189,
     153,  1629,  1643,  1644,  1635,  1441,  1442,  1522,  1523,  1637,
    1256,  1257,  1441,  1442,   157,    18,   176,    65,   158,   160,
    1812,   152,  1212,  1812,   154,  1624,   177,   154,  1218,   177,
    1629,   160,   160,   462,    18,   157,  1812,   157,  1637,   151,
    1812,  1231,  1812,  1812,  1718,   154,  1441,  1442,  1238,   154,
     154,  1752,  1778,  1624,   154,  1529,  1812,    13,  1629,    22,
     154,  1812,   160,   154,  1635,   154,  1637,  1432,  1433,   154,
     154,   154,   154,  1812,  1645,   151,   151,   230,   231,   160,
     160,   154,    70,  1441,  1442,   154,  1657,  1482,   154,   177,
     176,   154,  1282,  1529,   151,   160,  1812,   176,   251,  1670,
    1879,   160,   154,   154,   158,  1295,   176,   154,   158,   154,
    1664,  1665,  1862,   154,   158,   154,  1481,   216,   154,   154,
     154,   154,   551,   151,   154,   157,   157,  1665,   154,   154,
     154,   154,    88,   154,   154,   154,   158,   154,   551,   157,
     154,   154,   151,   154,  1715,   152,  1700,   152,   104,  1973,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     158,   314,   315,   152,    14,   152,   121,  1721,   123,   124,
     125,  1815,   152,  1665,   152,    74,   605,   159,   177,  2057,
     901,   158,   177,  1721,   159,   157,   157,  1815,   158,  1760,
     151,  1762,  1700,   914,   623,   624,   177,   152,   177,   157,
     155,   156,  1482,   160,   177,   160,   161,   154,  1700,    18,
     154,   624,   158,  1940,   154,   158,  1815,   938,   647,   583,
      13,    14,    15,    16,    17,    18,  1700,  1977,   949,  1665,
    2054,   660,   158,   157,   154,   157,  1426,   958,   154,  1429,
     151,  1812,   151,   177,  1815,   152,  1700,  1721,    57,    58,
      59,    60,    61,    62,    63,    64,  1827,   152,   177,    80,
    1831,    92,   177,   177,  1700,   177,   152,   177,   151,  1664,
     152,  1700,   177,   177,  1845,   177,   152,  2056,    90,  1833,
    2006,   154,  1472,  1473,  1855,  1721,    78,   151,   151,   158,
     158,   151,  1846,   160,  1938,   160,   157,  1868,   157,  1870,
    1871,  1872,  1873,   157,   157,  1700,  1258,  1878,  1879,   154,
    1938,   151,   104,   466,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   154,  1690,   159,   159,  2120,   758,
     121,  2120,   151,   157,  1055,  1287,   154,   154,  1846,  1938,
     154,   154,  1700,   154,  2120,   758,    84,   157,  2120,   151,
    2120,  2120,   151,   506,  1846,   177,   159,   152,  1079,  2086,
     154,  2088,  1973,   152,  2120,   518,   152,  1938,   158,  2120,
     799,   151,  1846,  1944,   157,  2125,   157,  1948,   157,  2129,
    2130,  2120,  1953,   160,  1336,   177,   799,   486,   151,   488,
     543,   151,  1846,   154,    75,   154,   549,   154,   154,   154,
    2127,   154,    75,   386,  2120,   157,   154,  1978,   821,   822,
    1846,   154,   150,   177,   104,  2165,   151,  1846,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   840,   411,   412,
     120,   177,   122,   152,   154,   177,   154,   590,  1833,   152,
     157,   157,  2192,  2054,   151,   160,  2196,   151,   601,   432,
     603,  1846,   190,   151,   154,  1645,    75,  2028,   154,   154,
     154,  2032,   154,   153,  2214,   154,   156,  1657,   154,   156,
       1,  2025,   155,     4,  2045,   168,   177,  2121,    75,   462,
    1670,   159,  2209,   168,  2120,  2056,   177,  2058,  1846,  2120,
    2120,   151,   151,  2121,   158,   177,   154,  1940,   154,  2053,
     154,  2120,   154,   151,   153,   918,   168,   936,   151,   159,
     939,   168,   876,  1234,   104,   152,   158,  2025,    75,  2163,
     151,   259,  2121,   168,  2095,  1715,   153,   168,    59,   157,
     177,   177,    75,  2025,    75,  2163,   151,   153,  2092,   159,
     154,   154,   151,   151,    75,  2053,   152,   154,  1269,  2120,
    2121,  2025,  2121,    84,  1275,   177,   154,   749,   154,   988,
    2131,  2053,  1326,  2134,  2163,  2136,    97,  1519,  1752,   100,
     708,  2025,  2143,   104,  1526,   177,   177,   941,   750,  2053,
     751,  1200,   752,   431,  2092,   323,   753,  1212,  2180,  2025,
    1700,  1543,  2163,  2088,  2163,  1854,  2025,  2117,  1846,  2053,
    2092,  2172,  1709,  2174,  2160,   343,  1712,  2178,  1691,  2180,
     141,  1691,  2054,  2130,  2196,  2053,   147,  2053,  2092,   150,
    1238,    49,   153,   154,  2053,   112,   264,  1938,  2015,   964,
    2025,  1417,  2203,   829,   928,   166,  1231,  1827,  2092,   495,
    1721,  1831,     0,  2086,  2215,  2088,   973,  1612,   617,   802,
     774,   804,    -1,  2224,    -1,  1845,  2092,   774,  2053,   190,
     191,   192,    -1,  2092,   774,  1855,    -1,  2025,    -1,    -1,
      -1,   202,   203,    -1,    -1,   206,    -1,    -1,  1868,    -1,
    1870,  1871,  1872,  1873,  2127,    -1,    -1,  1051,  1878,   842,
      -1,    -1,    -1,    -1,   432,  2053,    -1,  2092,    -1,   230,
     231,    -1,  1423,    18,  1068,  1069,    -1,    -1,  1121,    -1,
      -1,    -1,    62,    -1,    -1,   868,    -1,    -1,  2161,    -1,
     251,    -1,    -1,    -1,   462,    -1,    -1,    -1,   259,  1681,
     829,    -1,    -1,   832,  2092,    13,    14,    15,    16,    17,
      18,    -1,   273,    -1,    -1,    -1,    61,    62,    63,    64,
      -1,    -1,   735,    -1,  1944,   105,    -1,    -1,  1948,   109,
      -1,  1482,   112,  1953,   114,    -1,  2209,     4,     5,     6,
       7,     8,     9,    10,    11,    12,   307,    -1,    62,    -1,
      -1,   876,   313,   314,   315,    -1,    -1,    -1,  1978,   104,
      -1,  1743,   323,   108,   109,   110,   111,   112,   113,   114,
     115,   116,  1754,    -1,    -1,    -1,    -1,   545,   546,    -1,
      -1,    -1,   343,   344,   345,    -1,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2191,   111,   112,   360,
      -1,    -1,    -1,   364,    -1,    -1,  1265,    -1,  2028,    -1,
    2205,   156,  2032,    72,    -1,    -1,   941,    -1,    65,    66,
      67,    68,   104,    -1,    -1,  2045,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1294,    -1,   398,  2058,    -1,
     154,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,   432,    -1,    -1,   435,  2095,    -1,    -1,    -1,   647,
    1009,   442,   262,    -1,    -1,    -1,    -1,    -1,  1017,  1072,
      -1,    -1,   206,   152,   153,  1078,  1079,   458,  1880,  1881,
      -1,   462,    -1,   671,    -1,   466,    -1,   468,    -1,   156,
      -1,  2131,    -1,  1664,  2134,    -1,  2136,  1046,  1361,  1362,
    1049,    -1,    -1,  2143,    -1,    -1,  1051,   174,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,  1068,  1069,   506,    -1,    -1,  1342,  1343,
      -1,    -1,  2172,    -1,  2174,    -1,    -1,   518,  2178,   273,
    2180,    -1,  1356,  1357,    -1,    -1,    -1,   347,   348,    -1,
     350,   984,   352,    -1,    -1,    -1,   989,    -1,    -1,    -1,
    1109,    -1,   543,  2203,   545,   546,    -1,  1000,   549,    -1,
     551,    -1,    -1,   307,    -1,  2215,    -1,    -1,    -1,   313,
    1394,  1395,  1396,   177,  2224,    -1,    -1,    -1,    -1,   389,
      -1,    -1,    -1,    -1,   782,  1997,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   590,
      -1,   345,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     601,    -1,   603,    -1,   605,  1504,   607,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   623,   624,  1815,   626,  1249,    -1,    -1,    -1,
      -1,    -1,    -1,   634,    -1,   389,    -1,   638,    -1,    -1,
      -1,    -1,  1833,    -1,    -1,    -1,   647,    -1,    -1,   177,
      -1,    -1,    -1,  2075,    -1,    -1,   657,    -1,    -1,   660,
      -1,    -1,    -1,    -1,    -1,   485,    -1,   104,    -1,    -1,
     671,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     150,    -1,    -1,   684,    -1,    -1,   687,   688,    -1,   690,
      -1,    -1,    -1,    -1,    -1,   132,   166,    -1,   699,    -1,
     104,   702,   703,   704,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   468,   152,   153,   925,    -1,   156,
     190,    -1,    -1,    -1,    -1,    -1,   163,   164,  1351,  1182,
      -1,   485,    -1,   203,    -1,    -1,    -1,    -1,  1929,    -1,
      -1,    -1,  1311,   563,    -1,    -1,    -1,    -1,    -1,    -1,
    1319,    -1,   156,    -1,    -1,    -1,    -1,   758,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1648,    -1,    -1,  1602,  1603,
    1604,    -1,    -1,   774,   775,    -1,    -1,  1342,  1343,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   259,
      -1,  1356,  1357,    -1,    -1,    -1,   550,    -1,   799,  1422,
      -1,   802,    -1,   804,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1265,    -1,  1648,    -1,    -1,   571,    -1,    -1,
     821,   822,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1394,
    1395,  1396,    -1,    -1,    -1,    -1,    -1,   591,    -1,   840,
      -1,   842,    -1,    -1,    -1,    -1,    -1,    -1,  1301,  1302,
    1303,    -1,    -1,   323,    -1,  1308,  1309,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2057,   868,    -1,    -1,
      -1,    -1,   626,   343,    -1,    -1,    -1,    -1,    -1,    -1,
    1449,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1777,  1778,   650,    -1,  1732,    -1,
      -1,    -1,    -1,   657,    -1,    -1,    -1,    -1,   909,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,   918,    -1,    -1,
      -1,    -1,    -1,   924,   925,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,  1767,   156,   936,    -1,   938,  1772,  1773,
      -1,   163,   164,   241,    -1,    -1,    -1,    -1,   949,    -1,
      -1,    -1,    -1,    -1,   176,  1524,    -1,    -1,    -1,   104,
      -1,    -1,   432,   108,   109,   110,   111,   112,   113,   114,
     115,   116,  2163,  1542,    -1,   120,    -1,   122,  1547,     1,
      -1,    72,    -1,  1552,  1553,  1554,    -1,   988,    -1,    -1,
      -1,    -1,   462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1624,    -1,    -1,    -1,    -1,  1629,    -1,   153,    -1,
      -1,   156,  1635,   104,  1637,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,  1602,  1603,  1604,
      -1,   132,    -1,    -1,    -1,    -1,    -1,  1255,    -1,    -1,
      -1,    73,    -1,    -1,  1055,    -1,    -1,  1265,    -1,    -1,
      -1,   152,   153,    -1,    -1,   156,    -1,    -1,    -1,    -1,
      -1,  1072,   163,   164,  1973,   545,   546,    -1,  1079,   101,
     102,   551,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   872,    -1,
    1121,    -1,    -1,  2006,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,   605,    -1,   435,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,   901,    -1,    -1,
      -1,    -1,    -1,   451,   624,  2054,   454,    -1,    -1,    -1,
     914,    -1,    -1,    -1,  1165,    -1,    -1,  1732,    -1,    -1,
      -1,    -1,    -1,  1174,    -1,    -1,    -1,   647,    -1,    -1,
      -1,  1182,    -1,    -1,   938,    -1,    -1,    -1,    -1,  1812,
      -1,  1760,  1815,  1762,    -1,    -1,    -1,    -1,    -1,    -1,
       1,   671,  1767,     4,  1205,    -1,    -1,  1772,  1773,    -1,
     104,  1212,    -1,   511,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,  1234,  1235,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,  1249,    -1,
      -1,    -1,  1705,    -1,  1255,    -1,  1879,    -1,    59,   153,
      -1,    -1,   156,    -1,  1265,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    72,    84,    -1,    -1,    -1,    -1,   758,    -1,
      -1,    -1,    -1,  1294,    -1,    -1,    72,  1298,    -1,   100,
      -1,    -1,    -1,   104,    -1,    -1,  2140,    -1,    -1,    -1,
      -1,    -1,   782,    -1,   104,  1938,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   163,    -1,   104,   799,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     141,    -1,   132,    -1,    -1,    -1,   147,    -1,    -1,   150,
      -1,   821,   822,   154,    72,    -1,   132,    -1,    -1,    -1,
    1361,  1362,   152,   153,   165,   166,   167,    -1,    -1,    -1,
     840,    -1,    -1,   163,   164,    -1,   152,   153,    -1,    -1,
     156,    -1,    -1,    -1,    -1,    -1,   104,   163,   164,   190,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,   202,   203,    -1,    -1,   206,    -1,    -1,    -1,    -1,
    1411,  1412,  1413,  1414,   132,  1416,  1417,    -1,    -1,    -1,
      -1,  1422,  1423,  1631,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,  2056,   152,   153,    -1,    -1,    -1,    -1,
    1441,  1442,    -1,    -1,    -1,   163,   164,    -1,   918,    -1,
      -1,    -1,    -1,    -1,    -1,   925,   257,    -1,   259,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   273,    -1,    -1,    -1,   774,   775,    -1,    -1,
    1234,  1482,    -1,    -1,    -1,    -1,   784,    -1,   132,   787,
      -1,    -1,    -1,   294,    -1,    -1,    -1,  2120,  2121,    -1,
      -1,   302,    -1,  1504,    -1,    -1,   307,    -1,   152,   153,
      -1,    -1,   313,    -1,    -1,  1269,    -1,    -1,   988,   163,
     164,    -1,   323,    -1,    -1,    -1,    -1,  1980,  1529,    -1,
    1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2163,    -1,   343,    -1,   345,   346,    -1,    -1,    -1,    -1,
     848,    -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,   360,
      -1,   859,    -1,   364,    -1,   863,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2140,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,   398,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,  1624,  1625,    -1,    -1,    51,  1629,    53,
    1631,   432,    -1,    -1,  1635,   104,  1637,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    72,  1403,
      -1,  1121,    -1,    -1,    -1,    -1,    -1,   458,    -1,    -1,
      -1,   462,    -1,  1664,  1665,    -1,    -1,   468,    -1,  1423,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,    -1,    -1,   155,    -1,    -1,    -1,
      -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1700,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,  1718,    -1,    -1,
    1721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1482,    -1,
      -1,    -1,   132,    -1,    -1,   159,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   545,   546,    -1,    -1,    -1,   550,
     551,  1752,   152,   153,    -1,    -1,    -1,  1055,    -1,    -1,
      -1,   104,    -1,   163,   164,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,  1777,  1778,    -1,    -1,
      -1,    -1,   583,    -1,    -1,  1255,    -1,   588,    -1,    -1,
     591,   592,    -1,   594,    -1,  1265,  1797,  1798,    -1,    -1,
      -1,    -1,    -1,    -1,   605,    -1,   607,    -1,    -1,   152,
     153,  1812,    -1,    -1,  1815,    -1,    -1,    -1,    -1,    -1,
     621,    -1,   623,   624,  1294,   626,    -1,    -1,  1126,    -1,
      -1,  1129,  1833,    -1,    -1,  1133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1846,   647,    -1,    -1,   650,
      -1,    -1,    -1,   654,    -1,    -1,   657,    -1,    -1,   660,
      -1,   662,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     671,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1879,    72,
      -1,    -1,    -1,   684,    -1,    -1,   687,   688,    -1,   690,
      -1,  1361,  1362,    13,    14,    15,    16,    17,   699,    -1,
      -1,   702,   703,   704,    -1,    -1,    -1,    -1,    -1,    -1,
    1664,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,  1929,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1938,    -1,   132,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    72,    -1,    -1,    -1,    -1,   758,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,  1973,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   782,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   160,    -1,   799,    -1,
      -1,    -1,    -1,    -1,    -1,  2006,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,  2015,    -1,    -1,    -1,    -1,    -1,
     821,   822,    -1,    -1,  2025,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,  1504,    -1,   156,    -1,    -1,   840,
    1338,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,  1347,
      -1,    -1,  2053,  2054,    -1,  2056,  2057,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   872,    -1,    -1,    -1,   876,    -1,    -1,    -1,  1833,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,
      -1,  2092,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
     901,    -1,    -1,    -1,    -1,    -1,  1184,    -1,   909,    -1,
      -1,    -1,    -1,   914,   160,    -1,    -1,   918,    -1,  2120,
    2121,    -1,    -1,   924,   925,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   936,    -1,   938,   939,    -1,
     941,    -1,    -1,    -1,    59,    56,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,
      -1,  1631,  2163,    -1,    -1,    -1,    -1,    -1,   104,    84,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    92,    13,    14,    15,    16,    17,   988,    -1,   104,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
     104,   104,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,    -1,    -1,   141,   163,   164,    -1,
      -1,   142,   147,    -1,   145,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,   147,    -1,    -1,    -1,  1718,   160,
    1051,   166,    -1,    -1,    -1,    -1,   160,    -1,    -1,    -1,
      -1,    -1,    -1,   166,    -1,    -1,    -1,  1068,  1069,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   191,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   104,   202,   203,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     203,   132,   120,  2057,   122,    -1,    -1,  1777,  1778,    -1,
     221,    -1,    -1,    -1,    -1,    -1,   231,    -1,    -1,    -1,
    1121,   152,   153,    -1,    -1,    -1,    -1,  1625,    -1,    -1,
      -1,    -1,   163,   164,    -1,   153,   251,    -1,   156,    -1,
      -1,   256,   257,   102,   259,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   269,   270,
      -1,    -1,    -1,    -1,  1165,    -1,    -1,   282,    -1,   280,
      -1,   286,    -1,  1174,    -1,    -1,   291,    -1,    -1,   282,
      -1,  1182,    -1,    -1,   295,    -1,    -1,   302,    13,    14,
      15,    16,    17,   152,  1472,  1473,   155,   156,    -1,   302,
     315,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,    -1,   324,    -1,    -1,    -1,    -1,    -1,    -1,
     331,   332,    -1,    -1,    -1,   336,    -1,    -1,   343,    -1,
      -1,   346,    -1,  1234,  1235,    -1,    -1,    -1,    -1,    -1,
     343,    -1,    -1,   346,    -1,   360,    -1,    72,    -1,   364,
      -1,    -1,    -1,    -1,  1255,    -1,    -1,   360,    -1,    -1,
      -1,   364,    -1,   374,  1265,    -1,   377,   104,  1269,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   104,
    1281,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  1293,  1294,    -1,    -1,    -1,  1298,    -1,  1797,
    1798,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2006,   132,   163,   164,
      -1,  1342,  1343,   458,    -1,    -1,    -1,   462,    -1,    -1,
      -1,    -1,    -1,    -1,   465,  1356,  1357,   152,   153,   462,
    1361,  1362,    -1,    -1,    -1,    -1,   477,   478,   163,   164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,  1657,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,  1670,  1394,  1395,  1396,    -1,    -1,    -1,    -1,
      -1,    -1,  1403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,    -1,    -1,
      -1,    -1,  1423,    -1,    -1,    -1,    -1,   153,    -1,    -1,
     156,  1929,    -1,    -1,    -1,    -1,   551,  1715,    -1,    -1,
    1441,  1442,    -1,    -1,    -1,    -1,    -1,   104,   551,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   583,    -1,
      -1,    -1,    -1,   588,    -1,    -1,    -1,   592,    -1,   594,
     583,  1482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   592,
     605,   594,   607,    -1,    -1,    -1,    -1,    -1,    -1,   156,
      -1,    -1,   605,  1504,   607,    -1,   617,    -1,   623,   624,
      -1,    -1,    -1,  2011,    -1,    -1,    -1,  2015,    -1,    -1,
     623,   624,    -1,   638,    -1,    -1,    -1,    -1,  1529,    -1,
      -1,    -1,   647,    -1,    -1,    -1,    -1,   652,    -1,    -1,
      -1,    -1,    -1,    -1,   647,   660,    -1,    -1,    -1,  1827,
      -1,    -1,    -1,  1831,    -1,    -1,   667,   660,  2056,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1845,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,  1855,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1868,    -1,  1870,  1871,  1872,  1873,    -1,    -1,    -1,    -1,
      -1,  1602,  1603,  1604,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  2120,  2121,    -1,    -1,    -1,    -1,    -1,   155,
    1631,    -1,    -1,    -1,    59,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,   758,    -1,    -1,    -1,  1648,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   758,    -1,   152,   153,    -1,
     775,   156,    -1,  1664,  1665,  2163,  1944,    -1,   163,   164,
    1948,    -1,    -1,    -1,    -1,  1953,    -1,    -1,    -1,   104,
      -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,    -1,   804,
      -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,  1700,
    1978,    -1,    -1,    -1,    -1,   816,   821,   822,    -1,    -1,
      -1,    -1,   823,    -1,    -1,    -1,    -1,  1718,   821,   822,
    1721,    -1,   147,    -1,    -1,   840,    -1,    -1,    -1,    -1,
      -1,  1732,    -1,    -1,    -1,    -1,    -1,   840,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2028,  1752,    -1,    -1,  2032,    -1,    -1,    -1,    -1,    -1,
      -1,   876,    -1,    -1,    -1,    -1,  1767,  2045,    -1,    -1,
      -1,  1772,  1773,   876,    -1,    -1,  1777,  1778,   203,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,   909,    -1,    -1,   908,    -1,    -1,
      -1,    -1,    -1,   918,    -1,    -1,   909,    -1,   132,   924,
      -1,    -1,    -1,    -1,    -1,   918,    -1,  2095,    -1,    -1,
      -1,   936,    -1,    -1,   939,    -1,   941,    -1,   152,   153,
      -1,   946,  1833,   936,    -1,    -1,   939,    -1,   941,   163,
     164,    -1,    -1,    -1,    -1,  1846,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2131,     1,    -1,  2134,     4,  2136,    -1,
      -1,    -1,    -1,    -1,    -1,  2143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   988,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   988,    -1,    -1,    13,    14,
      15,    16,    17,    -1,  2172,    -1,  2174,    -1,    -1,    -1,
    2178,    -1,  2180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,   343,    -1,
      -1,   346,   104,    -1,    -1,  2203,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   360,  1051,    84,   120,   364,
     122,    -1,    -1,    -1,    -1,    -1,    -1,    72,  1051,    -1,
      -1,    -1,    -1,  1068,  1069,    -1,    -1,   104,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1068,  1069,    -1,    -1,    -1,
      -1,   153,  1973,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
     147,    -1,    -1,   150,    -1,  2006,  1121,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1121,    -1,
      -1,    -1,    -1,    -1,  2025,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   462,   163,   164,
      -1,    -1,    -1,   190,    -1,    -1,    -1,    -1,    -1,    -1,
    1165,    -1,  2053,  2054,    -1,   202,  2057,    -1,    -1,  1174,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1187,  1188,  1189,  1182,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,  2092,    -1,    -1,   149,    -1,    -1,  1212,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1218,    -1,  1212,
     257,    -1,   259,    -1,    -1,    -1,    -1,   264,    -1,    -1,
    1235,   176,    -1,    -1,    -1,    -1,   551,  1238,    -1,    -1,
      -1,    -1,  1235,    -1,    -1,    -1,    -1,    -1,   104,  2140,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1265,    -1,    -1,    -1,    -1,   302,    -1,    -1,   583,  1274,
      -1,    -1,  1265,    -1,    -1,    -1,    -1,   592,    -1,   594,
      -1,  1282,    -1,    -1,    -1,    -1,   323,    -1,  1293,  1294,
     605,    -1,   607,    -1,    -1,    -1,    -1,    -1,    -1,   155,
    1293,  1294,    -1,    -1,    -1,    -1,    -1,    -1,   623,   624,
      -1,    -1,    -1,     1,    -1,    -1,     4,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   647,    -1,    -1,    -1,    -1,  1342,  1343,    -1,
      -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,    -1,  1342,
    1343,  1356,  1357,    -1,    -1,    -1,  1361,  1362,    -1,    -1,
      -1,   398,    -1,  1356,  1357,   152,    -1,    -1,  1361,  1362,
     104,    59,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1394,
    1395,  1396,    -1,    -1,    -1,   432,    84,    -1,    -1,    -1,
      -1,  1394,  1395,  1396,    -1,    -1,  1411,  1412,  1413,  1414,
    1415,    -1,    -1,    -1,    -1,    -1,   104,    -1,   152,    -1,
      -1,   458,    -1,    -1,   112,  1426,    -1,    -1,  1429,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1441,  1442,    -1,    -1,
      -1,    -1,    -1,   758,    -1,    -1,    -1,    -1,  1441,  1442,
      -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,   147,
      -1,    -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1504,
      -1,    -1,   190,    -1,    -1,    -1,   821,   822,   545,   546,
      -1,  1504,    -1,    -1,   202,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1529,   840,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   588,    -1,    -1,    -1,   592,    -1,   594,    -1,    -1,
      -1,   876,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,
     607,   259,    -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   909,    -1,    -1,  1602,  1603,  1604,
      -1,    -1,    -1,   918,    -1,    -1,    -1,    -1,    -1,  1602,
    1603,  1604,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,   936,    -1,    -1,   939,    -1,   941,    -1,    -1,    -1,
      -1,    -1,  1637,    -1,   671,   323,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1648,  1645,    -1,    -1,   684,    -1,    -1,
     687,   688,    -1,   690,    -1,  1648,    -1,    -1,    -1,    -1,
    1665,    -1,   699,    -1,    -1,   702,   703,   704,    -1,    -1,
      -1,    -1,  1665,   988,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1700,    -1,    -1,
     398,    -1,    -1,  1718,    -1,    -1,  1721,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1718,    -1,  1732,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1051,    -1,    -1,  1732,
      -1,    -1,    -1,    -1,   432,   782,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1068,  1069,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1767,    -1,    -1,    -1,    -1,  1772,  1773,    -1,
     458,    -1,  1777,  1778,  1767,    -1,    -1,    -1,    -1,  1772,
    1773,    -1,    -1,    -1,  1777,  1778,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1798,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,  1121,    -1,    -1,    94,
      95,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1846,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1846,    -1,    -1,    -1,   545,   546,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,   909,    -1,    -1,    -1,    -1,  1878,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   924,   925,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1212,    -1,    -1,
     588,    -1,    -1,    -1,   592,    -1,   594,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,   607,
    1235,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
    1265,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,  1293,  1294,
    1973,    -1,    -1,   671,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,   687,
     688,  2006,   690,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,   699,    -1,  2006,   702,   703,   704,    -1,     1,    -1,
    2025,     4,    -1,    -1,    -1,    -1,   311,  1342,  1343,    -1,
      -1,    -1,  2025,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1356,  1357,    -1,    -1,    -1,  1361,  1362,  2053,  2054,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
    2053,  2054,    -1,    -1,   163,   164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,  1394,
    1395,  1396,    -1,    -1,    -1,    -1,    -1,  2092,    -1,    -1,
      -1,    -1,    -1,    -1,   782,    -1,    -1,    -1,    -1,  2092,
      -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2121,    -1,    -1,    -1,
      -1,   104,    -1,    -1,    -1,    -1,  1441,  1442,  1165,    -1,
      -1,    -1,    -1,    -1,    -1,  2140,    -1,  1174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1182,    -1,  2140,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,    -1,    -1,    -1,   147,    -1,    -1,   150,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,  1212,   461,    -1,   463,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   472,   473,  1504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1235,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1255,   202,
      -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   924,   925,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   257,    -1,   259,    -1,    -1,    -1,
      -1,   264,    -1,    -1,    -1,    -1,    -1,  1602,  1603,  1604,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   589,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1648,    -1,    -1,    -1,    -1,    -1,    -1,
     323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1665,    -1,    -1,    -1,   182,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1411,  1412,  1413,  1414,  1415,  1416,
    1417,    -1,    -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1718,  1441,  1442,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   398,    -1,  1732,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   432,
      -1,    -1,  1767,    -1,    -1,    -1,    -1,  1772,  1773,    -1,
      -1,    -1,  1777,  1778,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   458,    -1,  1165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,    -1,
      -1,    -1,  1529,    -1,  1182,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,    -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,
     815,  1846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1235,    -1,    -1,
      -1,    -1,    -1,    -1,   382,    -1,    -1,    -1,   386,   387,
      -1,    -1,   545,   546,    -1,    -1,    -1,  1255,   396,   397,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   411,   412,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1631,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   432,   588,    -1,    -1,    -1,   592,
    1298,   594,    -1,    -1,    -1,    -1,    -1,   902,   903,    -1,
      -1,    -1,    -1,    -1,   607,    -1,    -1,    -1,  1665,    -1,
     915,   916,   917,    -1,   462,   920,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,    -1,
      -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1721,    -1,    -1,    -1,   671,    -1,
      -1,  2006,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   684,    -1,    -1,   687,   688,    -1,   690,    -1,    -1,
    2025,    -1,    -1,    -1,    -1,  1752,   699,  1002,    -1,   702,
     703,   704,    -1,  1411,  1412,  1413,  1414,  1415,  1416,  1417,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2053,  2054,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1441,  1442,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1048,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2092,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   782,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,  1846,
    1095,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1104,
    1105,  1106,  1107,    -1,    -1,  2140,    -1,  1112,  1113,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1122,    -1,    -1,
      -1,  1529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1151,    -1,    -1,  1154,
      -1,  1156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,   736,   737,
     738,   739,   740,   741,   742,   743,   744,   745,   746,   747,
     748,   749,   750,   751,   752,   753,   909,  1212,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   924,   925,  1631,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1243,    -1,
      -1,    -1,    -1,    -1,    -1,  1250,    -1,  1252,  1253,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1665,    -1,  1264,
      -1,  1266,    -1,  1268,    -1,  1270,    -1,   815,  2025,    -1,
    1275,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1700,    -1,    -1,    -1,  2053,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1721,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   240,    -1,
      -1,    -1,  1337,    -1,    -1,  2092,    -1,    -1,    -1,  1344,
    1345,    -1,    -1,    -1,  1752,    -1,   258,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,
      -1,    -1,    -1,  1368,    -1,    -1,    -1,   279,    -1,    -1,
    1375,    -1,    -1,    -1,  1379,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   297,   298,    -1,    -1,    -1,
      -1,    -1,   304,   305,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     322,    -1,    -1,    -1,    -1,    -1,  1421,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     342,    -1,    -1,    -1,    -1,    -1,   984,    -1,  1846,    -1,
      -1,   989,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1000,    -1,    -1,    -1,    -1,    -1,  1463,    -1,
      -1,    -1,  1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1182,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   399,    -1,    -1,
      -1,    -1,    -1,    -1,  1042,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1205,  1508,    -1,    -1,    -1,    -1,    -1,  1212,
      -1,  1516,    -1,  1518,    -1,    -1,    -1,    -1,   430,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,   460,    -1,
      -1,    -1,  1255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1567,  1568,    -1,    -1,    -1,   479,    -1,    -1,
      -1,   483,   484,    -1,    -1,   487,    -1,  1582,  1583,    -1,
    1585,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1594,
     502,   503,   504,   505,    -1,  1298,    -1,    -1,    -1,    -1,
    1605,  1606,  1607,    -1,    -1,    48,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   525,    -1,    -1,    -1,  2025,    -1,    -1,
      -1,    -1,   534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    -1,  1182,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2053,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   566,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,
      -1,    -1,    -1,    -1,  2092,   597,    -1,    -1,    -1,    -1,
      -1,   134,   604,   136,    -1,    -1,    -1,    -1,   610,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1411,  1412,
    1413,  1414,  1415,  1416,  1417,    -1,    -1,  1265,    -1,    -1,
      -1,    -1,   634,   635,    -1,   168,    -1,    -1,  1733,  1734,
      -1,    -1,    -1,    -1,    -1,    -1,  1284,    -1,  1441,  1442,
      -1,  1746,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,
      -1,    -1,    -1,  1301,  1302,  1303,    -1,    -1,    -1,    -1,
    1308,  1309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1783,  1784,
    1785,    -1,    -1,    -1,  1332,    -1,    -1,   230,    -1,    -1,
      -1,   234,    -1,   705,   237,   238,    -1,    -1,   241,    -1,
      -1,   244,   245,    -1,   247,    -1,   249,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      -1,  1369,  1370,    18,    -1,    20,  1529,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,   774,    58,    59,    60,    61,    62,    63,    64,
      -1,   314,    -1,    -1,   317,    -1,    -1,   789,  1883,    -1,
      -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,    -1,    -1,    -1,   340,   341,  1904,
      -1,    -1,  1907,  1908,    -1,    -1,    -1,    -1,    -1,  1914,
      -1,    -1,   824,   356,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   833,    -1,    -1,    -1,     5,    -1,   839,  1631,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    -1,  1665,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,   885,    54,    -1,    56,   889,    -1,    -1,
      -1,   893,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,  1700,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   451,    -1,
     922,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1721,    -1,
      48,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,  1752,
    2055,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   506,    -1,    -1,   978,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,   522,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,   121,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,   136,    -1,
     166,    -1,    -1,  2118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2144,
      -1,    -1,    -1,  1846,    -1,    -1,   202,   203,    -1,  1061,
      -1,    -1,    -1,  1065,  2159,    -1,    -1,  1705,   601,    -1,
    1072,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1082,  2176,    -1,    -1,    -1,    -1,    -1,  1089,   234,    -1,
      -1,    -1,    -1,    -1,    -1,   241,  1098,    -1,  1100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   642,
     643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   237,
     238,    -1,   655,   241,    -1,    -1,   244,   245,    -1,   247,
      -1,   249,    -1,  1135,    -1,    -1,    -1,  1139,    -1,    -1,
      -1,  1143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1161,
      -1,    -1,    -1,    -1,  1166,    -1,    -1,    -1,    -1,    -1,
      -1,   317,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,   344,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   364,    -1,
      -1,    -1,   340,   341,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2025,    -1,    -1,    -1,    -1,    -1,   356,    -1,
      -1,    -1,    -1,    -1,    -1,   778,   779,    -1,    -1,    -1,
      -1,   784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2053,  1263,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,   808,   809,    -1,   811,    -1,
     813,   814,    -1,    -1,    -1,    -1,  1288,    -1,    -1,   435,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2092,
      -1,    -1,  1940,    -1,    -1,   451,   452,    -1,   454,   455,
      -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,    -1,
     466,    -1,   855,    -1,    -1,    -1,   859,    -1,    -1,    -1,
     863,    -1,    -1,   451,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1980,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   507,    -1,    -1,    -1,   511,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1382,    -1,    -1,    -1,  1386,    -1,    -1,    -1,  1390,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     933,   934,    -1,    -1,   522,   551,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   947,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2086,    -1,
    2088,    -1,    -1,    -1,    -1,    -1,   602,    -1,    -1,   605,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   623,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1489,   634,  2127,
    1492,    -1,   638,    -1,    -1,    -1,    -1,    -1,    -1,   645,
      -1,   647,    -1,    -1,    -1,    -1,    -1,    -1,  1510,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2161,   642,   643,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   655,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1563,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
    1572,  2209,    -1,    -1,  1576,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,  1119,    -1,  1590,  1591,
      -1,    -1,    -1,  1126,    -1,    -1,  1129,    -1,    -1,    -1,
    1133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   758,    -1,    -1,    -1,  1618,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   774,   775,
      -1,    -1,    -1,   150,    -1,    -1,    -1,   154,   784,   785,
      -1,   787,   788,    -1,    -1,    -1,    -1,    -1,    -1,   166,
      -1,    -1,    -1,   799,    -1,    -1,   802,    -1,   804,   805,
     778,   779,    -1,    -1,    -1,   811,   784,    -1,    -1,    -1,
      -1,    -1,    -1,   190,    -1,   821,   822,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   203,   805,    -1,   206,
     808,   809,    -1,   811,   840,   813,   814,    -1,   844,    -1,
      -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,    -1,   855,
     856,    -1,    -1,   859,   860,    -1,    -1,   863,   864,    -1,
    1722,  1723,    -1,    -1,    -1,   871,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,  1271,    -1,
      -1,   859,   259,    -1,    -1,   863,  1279,  1280,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   273,   398,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   918,   919,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   313,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,  1338,   323,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1347,   933,   934,  1350,    -1,  1352,
    1353,    -1,    -1,    -1,    -1,    -1,   343,    -1,   345,   947,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,   988,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   203,    -1,    -1,    -1,  1860,    -1,
      -1,    -1,    -1,    -1,    -1,  1398,    -1,   216,    -1,   218,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   398,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1893,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   545,   546,    -1,    -1,    -1,  1055,
    1912,    -1,    -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1072,  1073,    -1,    -1,
      -1,    -1,    -1,  1079,    -1,    -1,    -1,    -1,    -1,    -1,
    1942,    -1,    -1,    -1,    -1,   462,    -1,    -1,    -1,    -1,
    1483,   468,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1078,    -1,    -1,   312,    -1,    -1,    -1,  1969,    -1,    -1,
    1972,    -1,    -1,    -1,    -1,  1121,    -1,    -1,    -1,    -1,
    1126,  1127,    -1,  1129,  1130,    -1,    -1,  1133,  1134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1119,    -1,    -1,    -1,    -1,    -1,    -1,  1126,    -1,
      -1,  1129,    -1,    -1,    -1,  1133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,   546,
      -1,    -1,    -1,    -1,   551,    -1,    -1,    -1,  1571,    -1,
      -1,    -1,    -1,   684,    -1,    -1,    -1,    -1,    -1,   690,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,  2081,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   605,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1635,    -1,    -1,    -1,    -1,   624,  1641,   626,
      -1,    -1,    -1,   754,    -1,    -1,    -1,    -1,    -1,  1265,
      -1,    -1,    -1,    -1,    -1,  1271,  1272,    -1,    -1,    -1,
     647,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   488,
      -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,  1294,    -1,
     499,    -1,    -1,  1271,   671,    -1,    -1,    -1,    -1,    -1,
      -1,  1279,  1280,    -1,    -1,    -1,    -1,   684,    -1,    -1,
     687,   688,    -1,   690,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   699,    -1,  1717,   702,   703,   704,    -1,    -1,
      -1,    -1,  1338,  1339,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1347,  1348,    -1,  1350,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1361,  1362,    -1,    -1,    -1,
    1338,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1347,
      -1,    -1,  1350,    -1,  1352,  1353,    -1,    -1,    -1,    -1,
      -1,   758,    -1,    -1,    -1,    -1,   595,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   782,    -1,    -1,    -1,    -1,
      -1,  1804,  1805,    -1,    -1,   624,    -1,    -1,    -1,  1812,
    1398,    -1,   799,    -1,  1817,    -1,    -1,    -1,   637,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   821,   822,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   840,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   692,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1504,    -1,
      -1,    -1,    -1,    -1,    -1,  1483,    -1,   716,   717,    -1,
      -1,   720,    -1,   722,    -1,    -1,    -1,    -1,    -1,   728,
      -1,   730,   731,    -1,    -1,  1531,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1931,    -1,
      -1,   918,    -1,    -1,    -1,    -1,    -1,    -1,   925,   758,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   938,   771,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   796,    -1,    -1,
     799,    -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   988,    -1,    -1,    -1,    -1,    -1,   826,  2011,  1625,
     829,    -1,  1600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1641,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   872,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1718,    -1,    -1,    -1,   925,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2120,    -1,   938,
     939,    -1,    -1,    -1,    -1,    -1,    -1,   946,    -1,  1717,
      -1,    -1,    -1,    -1,  1121,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   974,    -1,    -1,    -1,    -1,
      -1,  1777,  1778,    -1,    -1,    -1,    -1,   132,    -1,   988,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   996,    -1,    -1,
      -1,  1797,  1798,    -1,  1003,    -1,    -1,  1174,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,  1813,   163,   164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1804,  1805,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1817,
    1049,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1234,    -1,    -1,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,  1255,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1265,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,  1116,    52,  1118,
      54,  1120,    56,    -1,    -1,    -1,    -1,  1294,    -1,    -1,
      -1,  1298,    -1,  1929,    -1,    -1,    -1,    -1,    72,    73,
      -1,  1937,    -1,    -1,    -1,    -1,    -1,  1438,    -1,    -1,
    1441,  1442,    -1,    -1,    -1,    -1,  1447,    -1,    -1,    -1,
    1451,    -1,  1453,  1931,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,  1361,  1362,    -1,    -1,   132,    -1,
    1199,  1200,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2006,    -1,    -1,    -1,    -1,  2011,  2012,    -1,   152,  2015,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2011,  1411,  1412,  1413,    -1,    -1,  1416,
    1417,    -1,    -1,    -1,    -1,    -1,  1423,    -1,    -1,    -1,
    2056,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1269,    -1,    -1,    -1,    -1,    -1,  1275,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,  1294,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,  1311,    -1,    -1,  1482,    -1,    -1,    -1,  1610,
      -1,    -1,    -1,    -1,  2120,  2121,    -1,  1326,    -1,    -1,
    1329,    -1,    -1,    -1,    -1,   101,   102,  1504,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,  2163,    -1,  1660,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1381,    -1,  1675,  1676,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,  1402,  1403,    -1,    -1,    -1,    -1,  1700,
      -1,    -1,    -1,    -1,    -1,  1706,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1431,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1449,    -1,    -1,  1452,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1631,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,  1664,    -1,   132,
      -1,    -1,    -1,    -1,    -1,  1504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1514,  1515,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,  1524,    -1,   160,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,  1829,    -1,
      -1,    -1,    -1,  1542,    -1,  1544,    -1,  1838,    -1,  1840,
      -1,  1718,  1843,  1844,    -1,  1846,    -1,    -1,    -1,    -1,
    1851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1777,  1778,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1631,    -1,    -1,    -1,    -1,  1636,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1833,    -1,  1959,    -1,
      -1,    -1,    -1,  1964,  1965,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,  1984,    -1,    -1,  1695,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,  2033,    55,  2035,    -1,    -1,  2038,  2039,    -1,
      -1,  1750,  2043,  2044,  1753,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1790,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1973,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,  2113,  2114,  2115,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2006,
      -1,    -1,    18,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,  2149,  2150,
    2151,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,  2054,    -1,    -1,
    2057,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    -1,    99,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,     1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,
     156,    -1,  1981,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,   177,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    -1,    99,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,     1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,   177,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
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
      -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
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
      69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,
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
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    72,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,   106,   107,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   177,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    90,    -1,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    13,
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
     154,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
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
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
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
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
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
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,   106,   107,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,   155,
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
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,    -1,
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
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
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
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,    -1,   106,   107,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   155,    -1,    -1,    13,    14,
      15,    16,    17,   163,   164,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    72,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,   106,   107,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,    -1,   106,   107,    -1,    -1,    -1,    -1,   163,   164,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
     155,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,   106,   107,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,   155,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    49,    -1,    -1,    52,    -1,    54,   132,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   146,   147,   148,    -1,    73,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    49,
      -1,    -1,    52,    -1,    54,   132,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
     147,   148,    -1,    73,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,   106,   107,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    72,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72
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
     183,   331,   451,   420,   152,   389,   390,   152,   152,   118,
     185,   186,    49,    52,    54,    56,    73,   101,   102,   104,
     105,   116,   117,   120,   121,   122,   124,   125,   152,   156,
     162,   165,   166,   167,   168,   181,   182,   185,   187,   190,
     198,   199,   200,   201,   204,   205,   206,   207,   208,   209,
     210,   211,   212,   213,   214,   215,   216,   222,   331,   154,
     156,   198,   199,   215,   217,   306,   331,   374,   375,   392,
     479,   484,   309,   430,   431,   432,   434,   435,   436,   154,
     154,   154,   154,   154,   154,   154,   156,   306,   462,   481,
     156,   163,   199,   217,   295,   296,   305,   307,   309,   321,
     328,   330,   364,   365,   368,   369,   370,   475,   483,   152,
     429,   433,   483,   152,   158,   104,   155,   156,   160,   182,
     184,   217,   377,   378,   379,   380,   381,    22,   377,   152,
     373,   228,   152,   158,   158,   158,   419,   424,   426,   427,
     428,   437,   439,   440,   441,   443,   444,   445,   309,   425,
     438,   442,   158,    99,   417,   156,   418,   459,   462,   417,
     418,   418,   413,   284,   152,   418,   459,   417,   418,   418,
     413,   418,   418,   309,   415,   152,   152,   308,   309,   306,
     309,   179,   306,   479,   484,   333,   160,   413,   284,   373,
     373,   376,   295,   314,   411,   429,   433,   160,   413,   284,
     394,   309,   321,   309,   309,   106,   332,   106,   107,   183,
     331,   336,   394,   179,   183,   372,   151,   179,     3,   300,
     303,   309,   313,   228,   179,   179,   417,   152,   417,   180,
     217,   419,   424,   309,   152,   179,   373,   404,   160,   373,
     160,   373,   134,   163,   164,   387,   154,   158,   373,   391,
     154,   418,   418,   157,   179,   307,   309,   321,   328,   330,
     474,   475,   483,   484,   152,   156,   164,   176,   199,   462,
     464,   465,   466,   467,   468,   469,   486,   199,   334,   483,
     309,   328,   315,   310,   418,   154,   307,   309,   476,   307,
     462,   476,    10,   162,   167,   358,   360,   361,   356,   358,
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
     179,   179,   179,   179,   424,   426,   427,   428,   437,   439,
     440,   441,   443,   444,   445,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   425,   438,   442,   418,   152,
     176,   157,   179,   376,   228,   413,   179,   376,   228,   415,
     224,   375,   224,   375,   415,   404,   228,   413,   417,   160,
     160,   413,   284,   404,   228,   413,   338,   339,   337,   160,
     134,   309,   366,   367,   370,   371,   154,   158,    70,   286,
     287,   180,   429,   442,   309,   300,   163,   217,   179,   424,
     365,   406,   404,   157,   179,   152,   386,   384,   385,    78,
     319,   183,   160,   183,   451,   307,   462,   476,   309,   313,
     483,   179,   465,   466,   467,   157,   179,    18,   217,   309,
     464,   486,   418,   418,   462,   307,   474,   484,   309,   183,
     418,   307,   476,   331,   158,   485,   373,   358,   160,   154,
     375,   154,   154,   158,   152,   177,   374,   187,   156,   374,
     374,   374,   217,   374,   154,   374,   374,   374,   179,   154,
     165,   166,   203,    18,   311,   154,   158,   154,   163,   164,
     154,   223,   217,   160,   217,   183,   217,   183,   116,   156,
     183,   153,   191,   192,   193,   217,   116,   156,   183,   344,
     217,   191,   183,   201,   204,   204,   204,   205,   205,   206,
     206,   207,   207,   207,   207,   208,   208,   209,   210,   211,
     212,   213,   159,   224,   177,   185,   156,   183,   217,   160,
     217,   179,   456,   457,   458,   309,   455,   418,   418,   217,
     375,   152,   418,   459,   462,   152,   459,   462,   179,   179,
     157,   157,   152,   424,   447,   448,   449,   452,    18,   309,
     446,   450,   152,   418,   468,   486,   418,   418,   486,   152,
     418,   468,   418,   418,   180,   216,   373,   157,   158,   157,
     158,   486,   486,   134,   363,   364,   365,   363,   373,   179,
     215,   216,   217,   416,   485,   377,   379,   151,   179,   154,
     158,   179,   363,   183,   415,   183,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   152,   418,   459,   462,   152,
     418,   459,   462,   152,   418,   459,   462,   415,   185,    22,
     462,   217,   316,   331,   460,   228,   366,   154,   154,   154,
     154,   402,   403,   228,   151,   179,   404,   228,   413,   403,
     228,   160,   160,   160,   345,   180,   180,   183,   288,   373,
      18,    71,    73,    76,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    93,    94,    95,    96,
      97,    99,   106,   107,   119,   152,   179,   224,   225,   226,
     227,   228,   229,   230,   232,   233,   243,   249,   250,   251,
     252,   253,   254,   259,   260,   263,   264,   265,   266,   267,
     273,   274,   275,   289,   309,   313,   373,   414,    70,   177,
     180,   180,   180,   363,   180,   405,   403,   293,   295,   306,
     397,   398,   399,   400,   392,   176,   383,   383,   360,   418,
     418,   307,   476,   156,   163,   199,   217,   331,   217,   309,
     366,   154,   154,   154,     5,   309,   418,   464,   160,   183,
     451,    10,   361,   151,   176,   362,   160,   360,   160,   154,
     422,   191,   154,   158,   179,   158,   154,   154,   158,   154,
     201,   154,   154,   154,   201,    18,   311,   217,   154,   154,
     153,   160,   201,   157,   180,   191,   157,   157,   116,   120,
     122,   184,   194,   195,   196,   154,   158,   194,   157,   158,
     151,   215,   159,   154,   194,   180,   378,   366,   154,   154,
     154,   455,   179,   179,   366,   366,   452,   154,   154,   154,
     154,   152,   424,   451,   446,   450,   179,   179,   157,   180,
     486,   179,   179,   180,   180,   180,   180,   376,   194,   134,
     168,   180,   180,   151,   377,   217,   418,   153,   217,   363,
     180,   176,   152,   418,   459,   462,   152,   418,   459,   462,
     152,   418,   459,   462,   179,   179,   179,   417,   154,   146,
     168,   180,   461,   158,   180,   180,   405,   397,   403,   228,
     405,   345,   345,   345,     3,     5,    10,    73,   151,   290,
     297,   298,   306,   309,   346,   351,   479,   154,   158,   158,
     177,   152,    61,    62,   177,   228,   289,   414,   152,    18,
     226,   152,   152,   177,   373,   177,   373,   163,   373,   160,
     225,   152,   152,   152,   228,   217,   218,   218,    14,   276,
     254,   265,    74,   234,   177,   180,   230,    78,   177,   373,
      91,    92,   258,   262,   110,   133,   257,   109,   132,   261,
     257,   372,   309,   159,   288,   177,   157,   157,   180,   158,
     405,   415,   180,   177,   180,   177,   180,   154,   375,   389,
     389,   485,   358,   358,   179,   180,   180,   180,   217,   180,
     152,   418,   468,   462,   308,     5,   163,   180,   217,   360,
     418,   418,   331,   373,   160,   216,   360,   485,   151,   179,
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
     151,   179,   158,   158,   151,   160,   160,   157,   157,   157,
     180,   154,   179,   217,   217,   180,   157,   180,   485,   357,
     358,   362,   362,   378,   485,   151,   397,   463,   464,   154,
     159,   154,   158,   159,   378,   485,   223,   121,   194,   195,
     156,   195,   156,   195,   157,   151,   154,   179,   180,   180,
     154,   154,   179,   179,   180,   180,   180,   179,   179,   157,
     180,   154,   418,   366,   366,   366,   180,   180,   180,   224,
     461,   151,   151,   340,   340,   340,   347,   152,   199,   349,
     350,   459,   470,   471,   472,   473,   177,   158,   177,   347,
     177,   392,   419,   424,   217,   309,   151,   158,   177,   353,
     354,   353,   353,   373,   134,   370,   371,   154,   154,   152,
     226,   154,   224,   309,   146,   147,   148,   168,   177,   247,
     248,   226,   225,   177,   248,   154,   159,   224,   153,   224,
     225,   246,   177,   485,   154,   154,   154,   228,   270,   271,
     152,   217,   152,   185,   235,   201,   255,   224,    75,   108,
     256,   258,    75,   256,     1,   226,   418,   398,   179,   179,
     360,   360,   157,   366,   180,   180,   157,   157,   151,   358,
     160,   485,   151,   180,   154,   217,   189,   217,   485,   151,
     157,   157,   194,   194,   366,   154,   154,   366,   366,   154,
     154,   157,   158,   134,   365,   134,   157,   180,   180,   180,
     154,   154,   154,   157,   217,   177,   471,   472,   473,   309,
     470,   158,   177,   418,   418,   177,   154,   424,   418,   226,
      77,    78,   160,   238,   239,   240,   154,   224,    75,   226,
     224,   153,   224,    75,   177,   106,   153,   224,   225,   246,
     153,   224,   226,   245,   248,   248,   177,   224,   151,   160,
     240,   226,   152,   179,   177,   185,   154,   159,   154,   154,
     158,   159,   154,   226,   152,   226,   226,   226,   226,   373,
     415,   485,   485,   180,   157,   157,   160,   360,   151,   151,
     151,   157,   157,   180,   180,   180,   179,   180,   154,   154,
     154,   154,   154,   154,   470,   418,   348,     1,   216,   236,
     237,   416,     1,   159,     1,   179,   226,   238,    75,   177,
     154,   226,    75,   177,   168,   168,   226,   225,   248,   248,
     177,   106,   224,   168,   168,    75,   153,   224,   153,   224,
     225,   177,     1,   179,   179,   272,   307,   309,   479,   159,
     177,   156,   185,   277,   278,   279,   226,   201,   191,   224,
     257,   257,   151,   151,   154,   360,   485,   154,   154,   154,
     368,   152,   418,   459,   462,   350,   134,     1,   158,   159,
     151,   282,   283,   289,   226,    75,   177,   226,   224,   153,
     153,   224,   153,   224,   153,   224,   225,   153,   224,   153,
     224,   226,   168,   168,   168,   168,   151,   282,   272,   180,
     152,   199,   415,   470,   183,   159,   104,   152,   154,   159,
     158,    75,   154,   154,    75,   253,    75,   253,   485,   151,
     179,   216,   236,   239,   241,   242,   289,   226,   168,   168,
     168,   168,   153,   153,   224,   153,   224,   153,   224,   241,
     180,   177,   269,   309,   277,   157,   216,   177,   277,   279,
     226,   226,    75,   226,    75,   151,   366,   226,   231,   180,
     239,   153,   153,   224,   153,   224,   153,   224,   180,   269,
     215,   154,   159,   185,   154,   154,   159,   226,   226,   180,
       1,   226,   151,   231,   151,   154,   228,   185,   280,   152,
     177,   280,   154,   228,   158,   159,   216,   154,   185,   183,
     281,   154,   177,   154,   158,   177,   183
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
     354,   355,   356,   355,   355,   355,   355,   357,   355,   355,
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
       2,     6,     0,     9,     8,     9,     8,     0,    13,    11,
      12,    11,     1,     0,     1,     3,     3,     3,     2,     5,
       5,     1,     1,     0,     2,     5,     0,     1,     1,     1,
       5,     5,     5,     1,     5,     5,     9,     1,     5,     0,
       1,     1,     5,     1,     1,     5,     5,     1,     3,     3,
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
       5,     3,     4,     8,     9,     3,     4,     2,     1,     2,
       6,     8,     9,     3,     4,     2,     3,     4,     5,     4,
       5,     4,     5,     3,     4,     1,     1,     1,     4,     8,
       9,     3,     4,     2,     3,     3,     4,     4,     5,     4,
       5,     3,     4,     1,     3,     2,     1,     2,     2,     2,
       3,     4,     5,     2,     4,     5,     4,     5,     3,     4,
       8,     9,     3,     4,     2,     1,     2,     6,     8,     9,
       3,     4,     2,     3,     4,     5,     4,     5,     4,     5,
       3,     4,     2,     4,     1,     2,     2,     2,     3,     4,
       2,     4,     4,     3,     6,     8,     3,     2,     4,     1,
       2,     2,     1,     1,     2,     3,     4,     2,     4,     6,
       8,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     5,     8,     3,     2,     3,     7,     5,
       1,     1,     1,     3,     3,     3,     5,     1,     1,     5,
       5,     6,     6,     0,     1,     1,     3,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     5,     8,
       3,     1,     2,     1,     2,     6,     5,     6,     7,     7,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     8,     3,     1,     1,     2,     1,     1,     2,
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
#line 7915 "Parser/parser.cc"
    break;

  case 3:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7921 "Parser/parser.cc"
    break;

  case 4:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 7927 "Parser/parser.cc"
    break;

  case 5:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7933 "Parser/parser.cc"
    break;

  case 6:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7939 "Parser/parser.cc"
    break;

  case 7:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 8:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 20:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7957 "Parser/parser.cc"
    break;

  case 21:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 22:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7969 "Parser/parser.cc"
    break;

  case 23:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7979 "Parser/parser.cc"
    break;

  case 24:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 7985 "Parser/parser.cc"
    break;

  case 25:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 7991 "Parser/parser.cc"
    break;

  case 26:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 7997 "Parser/parser.cc"
    break;

  case 28:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8003 "Parser/parser.cc"
    break;

  case 29:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8009 "Parser/parser.cc"
    break;

  case 30:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8015 "Parser/parser.cc"
    break;

  case 31:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8021 "Parser/parser.cc"
    break;

  case 32:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8031 "Parser/parser.cc"
    break;

  case 33:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.expr) = nullptr; }
#line 8037 "Parser/parser.cc"
    break;

  case 34:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8043 "Parser/parser.cc"
    break;

  case 35:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8049 "Parser/parser.cc"
    break;

  case 36:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8055 "Parser/parser.cc"
    break;

  case 37:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8061 "Parser/parser.cc"
    break;

  case 38:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8067 "Parser/parser.cc"
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
#line 8079 "Parser/parser.cc"
    break;

  case 41:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8088 "Parser/parser.cc"
    break;

  case 42:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8094 "Parser/parser.cc"
    break;

  case 44:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8100 "Parser/parser.cc"
    break;

  case 45:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8106 "Parser/parser.cc"
    break;

  case 46:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8112 "Parser/parser.cc"
    break;

  case 47:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8118 "Parser/parser.cc"
    break;

  case 48:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8128 "Parser/parser.cc"
    break;

  case 49:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8134 "Parser/parser.cc"
    break;

  case 50:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8141 "Parser/parser.cc"
    break;

  case 51:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8147 "Parser/parser.cc"
    break;

  case 52:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8153 "Parser/parser.cc"
    break;

  case 53:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8159 "Parser/parser.cc"
    break;

  case 54:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8165 "Parser/parser.cc"
    break;

  case 55:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8171 "Parser/parser.cc"
    break;

  case 56:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8177 "Parser/parser.cc"
    break;

  case 57:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8183 "Parser/parser.cc"
    break;

  case 58:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8189 "Parser/parser.cc"
    break;

  case 59:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8195 "Parser/parser.cc"
    break;

  case 60:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8201 "Parser/parser.cc"
    break;

  case 61:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 62:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8213 "Parser/parser.cc"
    break;

  case 63:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8219 "Parser/parser.cc"
    break;

  case 64:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8225 "Parser/parser.cc"
    break;

  case 65:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8231 "Parser/parser.cc"
    break;

  case 66:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8241 "Parser/parser.cc"
    break;

  case 67:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8247 "Parser/parser.cc"
    break;

  case 70:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8253 "Parser/parser.cc"
    break;

  case 71:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8259 "Parser/parser.cc"
    break;

  case 74:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8265 "Parser/parser.cc"
    break;

  case 76:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8271 "Parser/parser.cc"
    break;

  case 77:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8277 "Parser/parser.cc"
    break;

  case 78:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8283 "Parser/parser.cc"
    break;

  case 79:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8289 "Parser/parser.cc"
    break;

  case 80:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8295 "Parser/parser.cc"
    break;

  case 81:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8301 "Parser/parser.cc"
    break;

  case 82:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8307 "Parser/parser.cc"
    break;

  case 83:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8313 "Parser/parser.cc"
    break;

  case 84:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8321 "Parser/parser.cc"
    break;

  case 85:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8327 "Parser/parser.cc"
    break;

  case 86:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8336 "Parser/parser.cc"
    break;

  case 89:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8342 "Parser/parser.cc"
    break;

  case 90:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8348 "Parser/parser.cc"
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
#line 8368 "Parser/parser.cc"
    break;

  case 92:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8374 "Parser/parser.cc"
    break;

  case 93:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8380 "Parser/parser.cc"
    break;

  case 94:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8386 "Parser/parser.cc"
    break;

  case 95:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8392 "Parser/parser.cc"
    break;

  case 96:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8398 "Parser/parser.cc"
    break;

  case 97:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8404 "Parser/parser.cc"
    break;

  case 98:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8410 "Parser/parser.cc"
    break;

  case 99:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8416 "Parser/parser.cc"
    break;

  case 100:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8425 "Parser/parser.cc"
    break;

  case 101:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8431 "Parser/parser.cc"
    break;

  case 102:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8437 "Parser/parser.cc"
    break;

  case 103:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8443 "Parser/parser.cc"
    break;

  case 104:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8449 "Parser/parser.cc"
    break;

  case 105:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8455 "Parser/parser.cc"
    break;

  case 106:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8461 "Parser/parser.cc"
    break;

  case 107:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8467 "Parser/parser.cc"
    break;

  case 109:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8473 "Parser/parser.cc"
    break;

  case 110:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8479 "Parser/parser.cc"
    break;

  case 111:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8485 "Parser/parser.cc"
    break;

  case 112:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8491 "Parser/parser.cc"
    break;

  case 113:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8497 "Parser/parser.cc"
    break;

  case 114:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr), ast::CastExpr::Return ) ); }
#line 8503 "Parser/parser.cc"
    break;

  case 115:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8509 "Parser/parser.cc"
    break;

  case 116:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8515 "Parser/parser.cc"
    break;

  case 124:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8521 "Parser/parser.cc"
    break;

  case 126:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8527 "Parser/parser.cc"
    break;

  case 127:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8533 "Parser/parser.cc"
    break;

  case 128:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8539 "Parser/parser.cc"
    break;

  case 130:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8545 "Parser/parser.cc"
    break;

  case 131:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8551 "Parser/parser.cc"
    break;

  case 133:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8557 "Parser/parser.cc"
    break;

  case 134:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8563 "Parser/parser.cc"
    break;

  case 136:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8569 "Parser/parser.cc"
    break;

  case 137:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8575 "Parser/parser.cc"
    break;

  case 138:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8581 "Parser/parser.cc"
    break;

  case 139:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8587 "Parser/parser.cc"
    break;

  case 141:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8593 "Parser/parser.cc"
    break;

  case 142:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 144:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 146:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8611 "Parser/parser.cc"
    break;

  case 148:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8617 "Parser/parser.cc"
    break;

  case 150:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8623 "Parser/parser.cc"
    break;

  case 152:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8629 "Parser/parser.cc"
    break;

  case 154:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8635 "Parser/parser.cc"
    break;

  case 155:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), (yyvsp[-3].expr)->clone(), (yyvsp[0].expr) ) ); }
#line 8641 "Parser/parser.cc"
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
#line 8653 "Parser/parser.cc"
    break;

  case 159:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8659 "Parser/parser.cc"
    break;

  case 160:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8665 "Parser/parser.cc"
    break;

  case 164:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8671 "Parser/parser.cc"
    break;

  case 165:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8677 "Parser/parser.cc"
    break;

  case 166:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8683 "Parser/parser.cc"
    break;

  case 167:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8689 "Parser/parser.cc"
    break;

  case 168:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8695 "Parser/parser.cc"
    break;

  case 169:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8701 "Parser/parser.cc"
    break;

  case 170:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8707 "Parser/parser.cc"
    break;

  case 171:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8713 "Parser/parser.cc"
    break;

  case 172:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8719 "Parser/parser.cc"
    break;

  case 173:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8725 "Parser/parser.cc"
    break;

  case 174:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8731 "Parser/parser.cc"
    break;

  case 175:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8737 "Parser/parser.cc"
    break;

  case 176:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8743 "Parser/parser.cc"
    break;

  case 177:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8749 "Parser/parser.cc"
    break;

  case 178:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8755 "Parser/parser.cc"
    break;

  case 180:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8761 "Parser/parser.cc"
    break;

  case 181:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8767 "Parser/parser.cc"
    break;

  case 182:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8773 "Parser/parser.cc"
    break;

  case 184:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8779 "Parser/parser.cc"
    break;

  case 185:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8785 "Parser/parser.cc"
    break;

  case 198:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8791 "Parser/parser.cc"
    break;

  case 200:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8797 "Parser/parser.cc"
    break;

  case 201:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8803 "Parser/parser.cc"
    break;

  case 202:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntx error, label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.stmt) = nullptr;
		}
#line 8814 "Parser/parser.cc"
    break;

  case 203:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 204:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 206:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8832 "Parser/parser.cc"
    break;

  case 207:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8838 "Parser/parser.cc"
    break;

  case 208:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8844 "Parser/parser.cc"
    break;

  case 209:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8850 "Parser/parser.cc"
    break;

  case 210:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8856 "Parser/parser.cc"
    break;

  case 213:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8862 "Parser/parser.cc"
    break;

  case 214:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8868 "Parser/parser.cc"
    break;

  case 215:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 8874 "Parser/parser.cc"
    break;

  case 216:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8880 "Parser/parser.cc"
    break;

  case 217:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8886 "Parser/parser.cc"
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
#line 8900 "Parser/parser.cc"
    break;

  case 219:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "synatx error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8906 "Parser/parser.cc"
    break;

  case 220:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8912 "Parser/parser.cc"
    break;

  case 221:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8921 "Parser/parser.cc"
    break;

  case 222:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, declarations can only appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8927 "Parser/parser.cc"
    break;

  case 223:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 8933 "Parser/parser.cc"
    break;

  case 224:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 8939 "Parser/parser.cc"
    break;

  case 225:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 8945 "Parser/parser.cc"
    break;

  case 226:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8951 "Parser/parser.cc"
    break;

  case 227:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8957 "Parser/parser.cc"
    break;

  case 228:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 8963 "Parser/parser.cc"
    break;

  case 229:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 8969 "Parser/parser.cc"
    break;

  case 230:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8975 "Parser/parser.cc"
    break;

  case 232:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 8981 "Parser/parser.cc"
    break;

  case 233:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 8987 "Parser/parser.cc"
    break;

  case 234:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, case list missing after case." ); (yyval.clause) = nullptr; }
#line 8993 "Parser/parser.cc"
    break;

  case 235:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 8999 "Parser/parser.cc"
    break;

  case 236:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after case list." ); (yyval.clause) = nullptr; }
#line 9005 "Parser/parser.cc"
    break;

  case 237:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9011 "Parser/parser.cc"
    break;

  case 238:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, colon missing after default." ); (yyval.clause) = nullptr; }
#line 9017 "Parser/parser.cc"
    break;

  case 240:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9023 "Parser/parser.cc"
    break;

  case 241:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9029 "Parser/parser.cc"
    break;

  case 242:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9035 "Parser/parser.cc"
    break;

  case 244:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9041 "Parser/parser.cc"
    break;

  case 245:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9047 "Parser/parser.cc"
    break;

  case 246:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9053 "Parser/parser.cc"
    break;

  case 247:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9062 "Parser/parser.cc"
    break;

  case 248:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9068 "Parser/parser.cc"
    break;

  case 249:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9074 "Parser/parser.cc"
    break;

  case 250:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9080 "Parser/parser.cc"
    break;

  case 251:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9089 "Parser/parser.cc"
    break;

  case 252:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9095 "Parser/parser.cc"
    break;

  case 253:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 254:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 255:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9116 "Parser/parser.cc"
    break;

  case 256:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 257:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9128 "Parser/parser.cc"
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
#line 9147 "Parser/parser.cc"
    break;

  case 260:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 261:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9162 "Parser/parser.cc"
    break;

  case 262:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 263:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9174 "Parser/parser.cc"
    break;

  case 264:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9180 "Parser/parser.cc"
    break;

  case 265:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9186 "Parser/parser.cc"
    break;

  case 266:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9192 "Parser/parser.cc"
    break;

  case 267:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9198 "Parser/parser.cc"
    break;

  case 268:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9207 "Parser/parser.cc"
    break;

  case 269:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9216 "Parser/parser.cc"
    break;

  case 270:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9222 "Parser/parser.cc"
    break;

  case 271:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9231 "Parser/parser.cc"
    break;

  case 272:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9240 "Parser/parser.cc"
    break;

  case 273:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9246 "Parser/parser.cc"
    break;

  case 274:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9252 "Parser/parser.cc"
    break;

  case 275:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9258 "Parser/parser.cc"
    break;

  case 276:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9264 "Parser/parser.cc"
    break;

  case 277:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9270 "Parser/parser.cc"
    break;

  case 278:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9276 "Parser/parser.cc"
    break;

  case 279:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9282 "Parser/parser.cc"
    break;

  case 280:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9288 "Parser/parser.cc"
    break;

  case 281:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9297 "Parser/parser.cc"
    break;

  case 282:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9307 "Parser/parser.cc"
    break;

  case 283:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9313 "Parser/parser.cc"
    break;

  case 284:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9319 "Parser/parser.cc"
    break;

  case 285:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9328 "Parser/parser.cc"
    break;

  case 286:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9338 "Parser/parser.cc"
    break;

  case 287:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9344 "Parser/parser.cc"
    break;

  case 288:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9353 "Parser/parser.cc"
    break;

  case 289:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9363 "Parser/parser.cc"
    break;

  case 290:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9369 "Parser/parser.cc"
    break;

  case 291:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9375 "Parser/parser.cc"
    break;

  case 292:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9381 "Parser/parser.cc"
    break;

  case 293:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9387 "Parser/parser.cc"
    break;

  case 294:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9396 "Parser/parser.cc"
    break;

  case 295:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9406 "Parser/parser.cc"
    break;

  case 296:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9412 "Parser/parser.cc"
    break;

  case 297:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9421 "Parser/parser.cc"
    break;

  case 298:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9431 "Parser/parser.cc"
    break;

  case 299:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9437 "Parser/parser.cc"
    break;

  case 300:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9446 "Parser/parser.cc"
    break;

  case 301:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "syntax error, equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9456 "Parser/parser.cc"
    break;

  case 302:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9462 "Parser/parser.cc"
    break;

  case 303:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9471 "Parser/parser.cc"
    break;

  case 304:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) {
				SemanticError( yylloc, "syntax error, all enumeration ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr;
			}
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9482 "Parser/parser.cc"
    break;

  case 305:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9488 "Parser/parser.cc"
    break;

  case 306:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9494 "Parser/parser.cc"
    break;

  case 307:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9500 "Parser/parser.cc"
    break;

  case 308:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9506 "Parser/parser.cc"
    break;

  case 309:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9512 "Parser/parser.cc"
    break;

  case 311:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9518 "Parser/parser.cc"
    break;

  case 312:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9524 "Parser/parser.cc"
    break;

  case 313:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9530 "Parser/parser.cc"
    break;

  case 314:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9536 "Parser/parser.cc"
    break;

  case 315:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9542 "Parser/parser.cc"
    break;

  case 316:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9548 "Parser/parser.cc"
    break;

  case 317:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9554 "Parser/parser.cc"
    break;

  case 318:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9560 "Parser/parser.cc"
    break;

  case 319:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9566 "Parser/parser.cc"
    break;

  case 320:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9572 "Parser/parser.cc"
    break;

  case 321:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9578 "Parser/parser.cc"
    break;

  case 322:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 323:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9590 "Parser/parser.cc"
    break;

  case 324:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 325:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 326:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9608 "Parser/parser.cc"
    break;

  case 327:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 328:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 329:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 330:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 331:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9638 "Parser/parser.cc"
    break;

  case 332:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9644 "Parser/parser.cc"
    break;

  case 335:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 336:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "syntax error, mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9659 "Parser/parser.cc"
    break;

  case 337:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9665 "Parser/parser.cc"
    break;

  case 338:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9671 "Parser/parser.cc"
    break;

  case 341:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9677 "Parser/parser.cc"
    break;

  case 342:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9683 "Parser/parser.cc"
    break;

  case 345:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9689 "Parser/parser.cc"
    break;

  case 346:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9695 "Parser/parser.cc"
    break;

  case 347:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9701 "Parser/parser.cc"
    break;

  case 348:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9707 "Parser/parser.cc"
    break;

  case 349:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9713 "Parser/parser.cc"
    break;

  case 350:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9719 "Parser/parser.cc"
    break;

  case 351:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9725 "Parser/parser.cc"
    break;

  case 352:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9731 "Parser/parser.cc"
    break;

  case 353:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9737 "Parser/parser.cc"
    break;

  case 356:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9743 "Parser/parser.cc"
    break;

  case 357:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = build_waituntil_clause( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9749 "Parser/parser.cc"
    break;

  case 358:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[-1].wucn); }
#line 9755 "Parser/parser.cc"
    break;

  case 359:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9761 "Parser/parser.cc"
    break;

  case 360:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::AND, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9767 "Parser/parser.cc"
    break;

  case 361:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = (yyvsp[0].wucn); }
#line 9773 "Parser/parser.cc"
    break;

  case 362:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, (yyvsp[-2].wucn), (yyvsp[0].wucn) ); }
#line 9779 "Parser/parser.cc"
    break;

  case 363:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9785 "Parser/parser.cc"
    break;

  case 364:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-4].wucn), build_waituntil_timeout( yylloc, (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9791 "Parser/parser.cc"
    break;

  case 365:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "syntax error, else clause must be conditional after timeout or timeout never triggered." ); (yyval.wucn) = nullptr; }
#line 9797 "Parser/parser.cc"
    break;

  case 366:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wucn) = new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::LEFT_OR, (yyvsp[-8].wucn),
                new ast::WaitUntilStmt::ClauseNode( ast::WaitUntilStmt::ClauseNode::Op::OR, 
                    build_waituntil_timeout( yylloc, (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), 
                    build_waituntil_else( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9806 "Parser/parser.cc"
    break;

  case 367:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
            (yyval.stmt) = new StatementNode( build_waituntil_stmt( yylloc, (yyvsp[0].wucn) ) );
            // $$ = new StatementNode( build_compound( yylloc, nullptr ) );
        }
#line 9815 "Parser/parser.cc"
    break;

  case 368:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9821 "Parser/parser.cc"
    break;

  case 369:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9827 "Parser/parser.cc"
    break;

  case 370:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9833 "Parser/parser.cc"
    break;

  case 371:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 372:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 373:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9851 "Parser/parser.cc"
    break;

  case 374:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9857 "Parser/parser.cc"
    break;

  case 375:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9863 "Parser/parser.cc"
    break;

  case 376:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9869 "Parser/parser.cc"
    break;

  case 377:
#line 1756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9875 "Parser/parser.cc"
    break;

  case 378:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9881 "Parser/parser.cc"
    break;

  case 379:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 9887 "Parser/parser.cc"
    break;

  case 381:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 382:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9899 "Parser/parser.cc"
    break;

  case 383:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9905 "Parser/parser.cc"
    break;

  case 388:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 9911 "Parser/parser.cc"
    break;

  case 389:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9917 "Parser/parser.cc"
    break;

  case 390:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9923 "Parser/parser.cc"
    break;

  case 391:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9929 "Parser/parser.cc"
    break;

  case 392:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 9935 "Parser/parser.cc"
    break;

  case 393:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 9941 "Parser/parser.cc"
    break;

  case 394:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 9947 "Parser/parser.cc"
    break;

  case 395:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9953 "Parser/parser.cc"
    break;

  case 398:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 9959 "Parser/parser.cc"
    break;

  case 399:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 9965 "Parser/parser.cc"
    break;

  case 400:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 9974 "Parser/parser.cc"
    break;

  case 401:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9980 "Parser/parser.cc"
    break;

  case 402:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 9986 "Parser/parser.cc"
    break;

  case 403:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 9992 "Parser/parser.cc"
    break;

  case 404:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10001 "Parser/parser.cc"
    break;

  case 405:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10010 "Parser/parser.cc"
    break;

  case 406:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10016 "Parser/parser.cc"
    break;

  case 409:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10022 "Parser/parser.cc"
    break;

  case 410:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10028 "Parser/parser.cc"
    break;

  case 412:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10034 "Parser/parser.cc"
    break;

  case 413:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 10040 "Parser/parser.cc"
    break;

  case 420:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10051 "Parser/parser.cc"
    break;

  case 423:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10057 "Parser/parser.cc"
    break;

  case 424:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10063 "Parser/parser.cc"
    break;

  case 428:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10069 "Parser/parser.cc"
    break;

  case 430:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10075 "Parser/parser.cc"
    break;

  case 431:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10081 "Parser/parser.cc"
    break;

  case 432:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10087 "Parser/parser.cc"
    break;

  case 433:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10093 "Parser/parser.cc"
    break;

  case 434:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10099 "Parser/parser.cc"
    break;

  case 435:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10105 "Parser/parser.cc"
    break;

  case 437:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10111 "Parser/parser.cc"
    break;

  case 438:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10117 "Parser/parser.cc"
    break;

  case 439:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10123 "Parser/parser.cc"
    break;

  case 440:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10134 "Parser/parser.cc"
    break;

  case 441:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10140 "Parser/parser.cc"
    break;

  case 442:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10146 "Parser/parser.cc"
    break;

  case 443:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10152 "Parser/parser.cc"
    break;

  case 444:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10158 "Parser/parser.cc"
    break;

  case 445:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10167 "Parser/parser.cc"
    break;

  case 446:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10176 "Parser/parser.cc"
    break;

  case 447:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10185 "Parser/parser.cc"
    break;

  case 448:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10196 "Parser/parser.cc"
    break;

  case 449:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10205 "Parser/parser.cc"
    break;

  case 450:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10211 "Parser/parser.cc"
    break;

  case 451:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10217 "Parser/parser.cc"
    break;

  case 452:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10223 "Parser/parser.cc"
    break;

  case 453:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10231 "Parser/parser.cc"
    break;

  case 454:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10239 "Parser/parser.cc"
    break;

  case 455:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10245 "Parser/parser.cc"
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
#line 10260 "Parser/parser.cc"
    break;

  case 459:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10266 "Parser/parser.cc"
    break;

  case 460:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 461:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10278 "Parser/parser.cc"
    break;

  case 462:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10284 "Parser/parser.cc"
    break;

  case 463:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 469:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 10301 "Parser/parser.cc"
    break;

  case 482:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10307 "Parser/parser.cc"
    break;

  case 485:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10313 "Parser/parser.cc"
    break;

  case 488:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10319 "Parser/parser.cc"
    break;

  case 489:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10325 "Parser/parser.cc"
    break;

  case 490:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10331 "Parser/parser.cc"
    break;

  case 491:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10337 "Parser/parser.cc"
    break;

  case 492:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10343 "Parser/parser.cc"
    break;

  case 493:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10349 "Parser/parser.cc"
    break;

  case 495:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10355 "Parser/parser.cc"
    break;

  case 496:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10361 "Parser/parser.cc"
    break;

  case 498:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 499:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10373 "Parser/parser.cc"
    break;

  case 500:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10379 "Parser/parser.cc"
    break;

  case 501:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10385 "Parser/parser.cc"
    break;

  case 502:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10391 "Parser/parser.cc"
    break;

  case 503:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10397 "Parser/parser.cc"
    break;

  case 504:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10403 "Parser/parser.cc"
    break;

  case 505:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10409 "Parser/parser.cc"
    break;

  case 506:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10415 "Parser/parser.cc"
    break;

  case 507:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10421 "Parser/parser.cc"
    break;

  case 508:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10427 "Parser/parser.cc"
    break;

  case 509:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10433 "Parser/parser.cc"
    break;

  case 510:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10439 "Parser/parser.cc"
    break;

  case 511:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10445 "Parser/parser.cc"
    break;

  case 512:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10451 "Parser/parser.cc"
    break;

  case 513:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10457 "Parser/parser.cc"
    break;

  case 514:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10463 "Parser/parser.cc"
    break;

  case 515:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10469 "Parser/parser.cc"
    break;

  case 516:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10475 "Parser/parser.cc"
    break;

  case 517:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10481 "Parser/parser.cc"
    break;

  case 518:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10487 "Parser/parser.cc"
    break;

  case 519:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10493 "Parser/parser.cc"
    break;

  case 520:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10499 "Parser/parser.cc"
    break;

  case 521:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10505 "Parser/parser.cc"
    break;

  case 522:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10511 "Parser/parser.cc"
    break;

  case 523:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10517 "Parser/parser.cc"
    break;

  case 524:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10523 "Parser/parser.cc"
    break;

  case 525:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10529 "Parser/parser.cc"
    break;

  case 526:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10535 "Parser/parser.cc"
    break;

  case 527:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10541 "Parser/parser.cc"
    break;

  case 528:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10547 "Parser/parser.cc"
    break;

  case 529:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10553 "Parser/parser.cc"
    break;

  case 530:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10559 "Parser/parser.cc"
    break;

  case 531:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10565 "Parser/parser.cc"
    break;

  case 532:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10571 "Parser/parser.cc"
    break;

  case 533:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10577 "Parser/parser.cc"
    break;

  case 534:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10583 "Parser/parser.cc"
    break;

  case 536:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10589 "Parser/parser.cc"
    break;

  case 538:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10595 "Parser/parser.cc"
    break;

  case 539:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10601 "Parser/parser.cc"
    break;

  case 540:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10607 "Parser/parser.cc"
    break;

  case 542:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10613 "Parser/parser.cc"
    break;

  case 543:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 544:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 545:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 547:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 549:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 550:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 551:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10655 "Parser/parser.cc"
    break;

  case 552:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10661 "Parser/parser.cc"
    break;

  case 553:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10667 "Parser/parser.cc"
    break;

  case 554:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10673 "Parser/parser.cc"
    break;

  case 555:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10679 "Parser/parser.cc"
    break;

  case 556:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10685 "Parser/parser.cc"
    break;

  case 557:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10691 "Parser/parser.cc"
    break;

  case 558:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10702 "Parser/parser.cc"
    break;

  case 559:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 560:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10714 "Parser/parser.cc"
    break;

  case 561:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 562:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10731 "Parser/parser.cc"
    break;

  case 563:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10737 "Parser/parser.cc"
    break;

  case 564:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10743 "Parser/parser.cc"
    break;

  case 565:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10752 "Parser/parser.cc"
    break;

  case 567:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 568:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 569:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10770 "Parser/parser.cc"
    break;

  case 571:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10776 "Parser/parser.cc"
    break;

  case 572:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10782 "Parser/parser.cc"
    break;

  case 574:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 575:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 576:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 578:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 579:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 580:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 581:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 582:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 584:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 585:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 586:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10848 "Parser/parser.cc"
    break;

  case 587:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10854 "Parser/parser.cc"
    break;

  case 588:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 589:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10871 "Parser/parser.cc"
    break;

  case 593:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10877 "Parser/parser.cc"
    break;

  case 594:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 595:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10892 "Parser/parser.cc"
    break;

  case 596:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10900 "Parser/parser.cc"
    break;

  case 597:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10909 "Parser/parser.cc"
    break;

  case 598:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10918 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10927 "Parser/parser.cc"
    break;

  case 600:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10936 "Parser/parser.cc"
    break;

  case 602:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10942 "Parser/parser.cc"
    break;

  case 603:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 10948 "Parser/parser.cc"
    break;

  case 604:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10958 "Parser/parser.cc"
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
#line 10977 "Parser/parser.cc"
    break;

  case 608:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 10983 "Parser/parser.cc"
    break;

  case 609:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 10989 "Parser/parser.cc"
    break;

  case 610:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 10995 "Parser/parser.cc"
    break;

  case 611:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11001 "Parser/parser.cc"
    break;

  case 612:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11007 "Parser/parser.cc"
    break;

  case 613:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11013 "Parser/parser.cc"
    break;

  case 614:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11022 "Parser/parser.cc"
    break;

  case 615:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11028 "Parser/parser.cc"
    break;

  case 616:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11037 "Parser/parser.cc"
    break;

  case 617:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11043 "Parser/parser.cc"
    break;

  case 618:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11052 "Parser/parser.cc"
    break;

  case 619:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11058 "Parser/parser.cc"
    break;

  case 620:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11064 "Parser/parser.cc"
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
#line 11077 "Parser/parser.cc"
    break;

  case 622:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "syntax error, expecting ';' at end of previous declaration." ) );
			(yyval.decl) = nullptr;
		}
#line 11086 "Parser/parser.cc"
    break;

  case 623:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11092 "Parser/parser.cc"
    break;

  case 624:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11098 "Parser/parser.cc"
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
#line 11111 "Parser/parser.cc"
    break;

  case 626:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11117 "Parser/parser.cc"
    break;

  case 629:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11123 "Parser/parser.cc"
    break;

  case 630:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11129 "Parser/parser.cc"
    break;

  case 633:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11135 "Parser/parser.cc"
    break;

  case 635:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11141 "Parser/parser.cc"
    break;

  case 636:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11147 "Parser/parser.cc"
    break;

  case 637:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11153 "Parser/parser.cc"
    break;

  case 638:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11159 "Parser/parser.cc"
    break;

  case 639:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11165 "Parser/parser.cc"
    break;

  case 640:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11171 "Parser/parser.cc"
    break;

  case 642:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11177 "Parser/parser.cc"
    break;

  case 644:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11183 "Parser/parser.cc"
    break;

  case 645:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11189 "Parser/parser.cc"
    break;

  case 647:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11195 "Parser/parser.cc"
    break;

  case 648:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11201 "Parser/parser.cc"
    break;

  case 650:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11207 "Parser/parser.cc"
    break;

  case 651:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 652:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 11219 "Parser/parser.cc"
    break;

  case 653:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 654:
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11231 "Parser/parser.cc"
    break;

  case 655:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11242 "Parser/parser.cc"
    break;

  case 656:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11250 "Parser/parser.cc"
    break;

  case 657:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) {
				SemanticError( yylloc, "syntax error, storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." );
			}
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 11261 "Parser/parser.cc"
    break;

  case 658:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11269 "Parser/parser.cc"
    break;

  case 659:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11277 "Parser/parser.cc"
    break;

  case 660:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11285 "Parser/parser.cc"
    break;

  case 661:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11293 "Parser/parser.cc"
    break;

  case 663:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11299 "Parser/parser.cc"
    break;

  case 664:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11305 "Parser/parser.cc"
    break;

  case 665:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 666:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 667:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 668:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11329 "Parser/parser.cc"
    break;

  case 669:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 670:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 672:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11347 "Parser/parser.cc"
    break;

  case 673:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11353 "Parser/parser.cc"
    break;

  case 674:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 675:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11365 "Parser/parser.cc"
    break;

  case 676:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11371 "Parser/parser.cc"
    break;

  case 677:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11377 "Parser/parser.cc"
    break;

  case 680:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 681:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11389 "Parser/parser.cc"
    break;

  case 682:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11395 "Parser/parser.cc"
    break;

  case 684:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 685:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 686:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 688:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 689:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11425 "Parser/parser.cc"
    break;

  case 690:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11431 "Parser/parser.cc"
    break;

  case 692:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11437 "Parser/parser.cc"
    break;

  case 695:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 696:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 698:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 699:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 700:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 705:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 707:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11479 "Parser/parser.cc"
    break;

  case 708:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11485 "Parser/parser.cc"
    break;

  case 709:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11491 "Parser/parser.cc"
    break;

  case 710:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11497 "Parser/parser.cc"
    break;

  case 711:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 712:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 718:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 721:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11521 "Parser/parser.cc"
    break;

  case 722:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11527 "Parser/parser.cc"
    break;

  case 723:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11533 "Parser/parser.cc"
    break;

  case 724:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11539 "Parser/parser.cc"
    break;

  case 725:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 726:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11551 "Parser/parser.cc"
    break;

  case 727:
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11557 "Parser/parser.cc"
    break;

  case 729:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11563 "Parser/parser.cc"
    break;

  case 730:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11569 "Parser/parser.cc"
    break;

  case 731:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11575 "Parser/parser.cc"
    break;

  case 733:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11581 "Parser/parser.cc"
    break;

  case 735:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11587 "Parser/parser.cc"
    break;

  case 736:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11593 "Parser/parser.cc"
    break;

  case 737:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11599 "Parser/parser.cc"
    break;

  case 738:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11605 "Parser/parser.cc"
    break;

  case 739:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11611 "Parser/parser.cc"
    break;

  case 740:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11617 "Parser/parser.cc"
    break;

  case 742:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 743:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11629 "Parser/parser.cc"
    break;

  case 744:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11635 "Parser/parser.cc"
    break;

  case 745:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11646 "Parser/parser.cc"
    break;

  case 746:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11652 "Parser/parser.cc"
    break;

  case 747:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11658 "Parser/parser.cc"
    break;

  case 748:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11664 "Parser/parser.cc"
    break;

  case 749:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11673 "Parser/parser.cc"
    break;

  case 750:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 751:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11685 "Parser/parser.cc"
    break;

  case 752:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11691 "Parser/parser.cc"
    break;

  case 753:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11697 "Parser/parser.cc"
    break;

  case 754:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11703 "Parser/parser.cc"
    break;

  case 755:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11709 "Parser/parser.cc"
    break;

  case 756:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11715 "Parser/parser.cc"
    break;

  case 757:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11721 "Parser/parser.cc"
    break;

  case 758:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11727 "Parser/parser.cc"
    break;

  case 759:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11733 "Parser/parser.cc"
    break;

  case 762:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11739 "Parser/parser.cc"
    break;

  case 763:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11745 "Parser/parser.cc"
    break;

  case 764:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11751 "Parser/parser.cc"
    break;

  case 765:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 767:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11763 "Parser/parser.cc"
    break;

  case 768:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11769 "Parser/parser.cc"
    break;

  case 769:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11775 "Parser/parser.cc"
    break;

  case 770:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 771:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 772:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 773:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 774:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11808 "Parser/parser.cc"
    break;

  case 775:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11817 "Parser/parser.cc"
    break;

  case 776:
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11826 "Parser/parser.cc"
    break;

  case 777:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11832 "Parser/parser.cc"
    break;

  case 778:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11841 "Parser/parser.cc"
    break;

  case 779:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11847 "Parser/parser.cc"
    break;

  case 781:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11853 "Parser/parser.cc"
    break;

  case 786:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11859 "Parser/parser.cc"
    break;

  case 787:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11865 "Parser/parser.cc"
    break;

  case 788:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 790:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11877 "Parser/parser.cc"
    break;

  case 791:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11883 "Parser/parser.cc"
    break;

  case 792:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11889 "Parser/parser.cc"
    break;

  case 793:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11895 "Parser/parser.cc"
    break;

  case 795:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11901 "Parser/parser.cc"
    break;

  case 796:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11907 "Parser/parser.cc"
    break;

  case 797:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 798:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11930 "Parser/parser.cc"
    break;

  case 799:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11936 "Parser/parser.cc"
    break;

  case 800:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11942 "Parser/parser.cc"
    break;

  case 801:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11948 "Parser/parser.cc"
    break;

  case 802:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11954 "Parser/parser.cc"
    break;

  case 803:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11960 "Parser/parser.cc"
    break;

  case 804:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11966 "Parser/parser.cc"
    break;

  case 806:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11975 "Parser/parser.cc"
    break;

  case 807:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 808:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11990 "Parser/parser.cc"
    break;

  case 809:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12000 "Parser/parser.cc"
    break;

  case 810:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12009 "Parser/parser.cc"
    break;

  case 811:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12019 "Parser/parser.cc"
    break;

  case 812:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12030 "Parser/parser.cc"
    break;

  case 813:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12040 "Parser/parser.cc"
    break;

  case 814:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12051 "Parser/parser.cc"
    break;

  case 815:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12061 "Parser/parser.cc"
    break;

  case 816:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) {
				SemanticError( yylloc, "syntax error, CV qualifiers cannot be distributed; only storage-class and forall qualifiers." );
			}
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12072 "Parser/parser.cc"
    break;

  case 817:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12082 "Parser/parser.cc"
    break;

  case 819:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12088 "Parser/parser.cc"
    break;

  case 820:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12094 "Parser/parser.cc"
    break;

  case 821:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12100 "Parser/parser.cc"
    break;

  case 822:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "syntax error, attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12112 "Parser/parser.cc"
    break;

  case 823:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12123 "Parser/parser.cc"
    break;

  case 824:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12132 "Parser/parser.cc"
    break;

  case 825:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12141 "Parser/parser.cc"
    break;

  case 826:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12147 "Parser/parser.cc"
    break;

  case 827:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12153 "Parser/parser.cc"
    break;

  case 828:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12159 "Parser/parser.cc"
    break;

  case 829:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12168 "Parser/parser.cc"
    break;

  case 830:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12174 "Parser/parser.cc"
    break;

  case 831:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12180 "Parser/parser.cc"
    break;

  case 832:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12186 "Parser/parser.cc"
    break;

  case 837:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12192 "Parser/parser.cc"
    break;

  case 838:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12198 "Parser/parser.cc"
    break;

  case 839:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12208 "Parser/parser.cc"
    break;

  case 840:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12214 "Parser/parser.cc"
    break;

  case 843:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12220 "Parser/parser.cc"
    break;

  case 844:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12226 "Parser/parser.cc"
    break;

  case 846:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12232 "Parser/parser.cc"
    break;

  case 847:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12238 "Parser/parser.cc"
    break;

  case 848:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12244 "Parser/parser.cc"
    break;

  case 849:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12250 "Parser/parser.cc"
    break;

  case 854:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12256 "Parser/parser.cc"
    break;

  case 855:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12262 "Parser/parser.cc"
    break;

  case 856:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12268 "Parser/parser.cc"
    break;

  case 857:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12274 "Parser/parser.cc"
    break;

  case 858:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12280 "Parser/parser.cc"
    break;

  case 860:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12286 "Parser/parser.cc"
    break;

  case 861:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12292 "Parser/parser.cc"
    break;

  case 862:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12298 "Parser/parser.cc"
    break;

  case 863:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12304 "Parser/parser.cc"
    break;

  case 864:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12310 "Parser/parser.cc"
    break;

  case 865:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12316 "Parser/parser.cc"
    break;

  case 866:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12322 "Parser/parser.cc"
    break;

  case 867:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12328 "Parser/parser.cc"
    break;

  case 868:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12334 "Parser/parser.cc"
    break;

  case 869:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12340 "Parser/parser.cc"
    break;

  case 870:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12346 "Parser/parser.cc"
    break;

  case 871:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12352 "Parser/parser.cc"
    break;

  case 872:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12358 "Parser/parser.cc"
    break;

  case 873:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12364 "Parser/parser.cc"
    break;

  case 874:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12370 "Parser/parser.cc"
    break;

  case 875:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12376 "Parser/parser.cc"
    break;

  case 876:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12382 "Parser/parser.cc"
    break;

  case 877:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12388 "Parser/parser.cc"
    break;

  case 879:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12394 "Parser/parser.cc"
    break;

  case 880:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12400 "Parser/parser.cc"
    break;

  case 881:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12406 "Parser/parser.cc"
    break;

  case 882:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12412 "Parser/parser.cc"
    break;

  case 883:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12418 "Parser/parser.cc"
    break;

  case 884:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12424 "Parser/parser.cc"
    break;

  case 885:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12430 "Parser/parser.cc"
    break;

  case 886:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12436 "Parser/parser.cc"
    break;

  case 887:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12442 "Parser/parser.cc"
    break;

  case 888:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12448 "Parser/parser.cc"
    break;

  case 889:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12454 "Parser/parser.cc"
    break;

  case 890:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12460 "Parser/parser.cc"
    break;

  case 891:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12466 "Parser/parser.cc"
    break;

  case 892:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12472 "Parser/parser.cc"
    break;

  case 893:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12478 "Parser/parser.cc"
    break;

  case 894:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12484 "Parser/parser.cc"
    break;

  case 898:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12490 "Parser/parser.cc"
    break;

  case 899:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12496 "Parser/parser.cc"
    break;

  case 900:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12502 "Parser/parser.cc"
    break;

  case 901:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12508 "Parser/parser.cc"
    break;

  case 902:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12514 "Parser/parser.cc"
    break;

  case 903:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12520 "Parser/parser.cc"
    break;

  case 904:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12526 "Parser/parser.cc"
    break;

  case 905:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12532 "Parser/parser.cc"
    break;

  case 906:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12538 "Parser/parser.cc"
    break;

  case 907:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12544 "Parser/parser.cc"
    break;

  case 908:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12550 "Parser/parser.cc"
    break;

  case 909:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12556 "Parser/parser.cc"
    break;

  case 910:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12562 "Parser/parser.cc"
    break;

  case 911:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12568 "Parser/parser.cc"
    break;

  case 912:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 913:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12583 "Parser/parser.cc"
    break;

  case 914:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12589 "Parser/parser.cc"
    break;

  case 915:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12595 "Parser/parser.cc"
    break;

  case 917:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12601 "Parser/parser.cc"
    break;

  case 918:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12607 "Parser/parser.cc"
    break;

  case 919:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12613 "Parser/parser.cc"
    break;

  case 920:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12619 "Parser/parser.cc"
    break;

  case 921:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12625 "Parser/parser.cc"
    break;

  case 922:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12631 "Parser/parser.cc"
    break;

  case 923:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12637 "Parser/parser.cc"
    break;

  case 924:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12643 "Parser/parser.cc"
    break;

  case 925:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12649 "Parser/parser.cc"
    break;

  case 926:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12655 "Parser/parser.cc"
    break;

  case 927:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12661 "Parser/parser.cc"
    break;

  case 928:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12667 "Parser/parser.cc"
    break;

  case 929:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12673 "Parser/parser.cc"
    break;

  case 930:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12679 "Parser/parser.cc"
    break;

  case 931:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12685 "Parser/parser.cc"
    break;

  case 932:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12691 "Parser/parser.cc"
    break;

  case 933:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 934:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 936:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 937:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 938:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 939:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 940:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12733 "Parser/parser.cc"
    break;

  case 941:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 942:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 943:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 944:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 945:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 946:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12769 "Parser/parser.cc"
    break;

  case 947:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 948:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 949:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 950:
#line 3697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12793 "Parser/parser.cc"
    break;

  case 951:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 952:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12805 "Parser/parser.cc"
    break;

  case 953:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12811 "Parser/parser.cc"
    break;

  case 955:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 956:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 957:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 958:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 959:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12841 "Parser/parser.cc"
    break;

  case 960:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 961:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 962:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 963:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12865 "Parser/parser.cc"
    break;

  case 964:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 965:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 966:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12883 "Parser/parser.cc"
    break;

  case 967:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 968:
#line 3761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 970:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12901 "Parser/parser.cc"
    break;

  case 971:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 972:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 973:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 974:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 975:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 976:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 977:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12943 "Parser/parser.cc"
    break;

  case 978:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 979:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 980:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 982:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12967 "Parser/parser.cc"
    break;

  case 983:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 984:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 985:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 986:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 987:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 988:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 990:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 991:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 992:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13021 "Parser/parser.cc"
    break;

  case 993:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13027 "Parser/parser.cc"
    break;

  case 994:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 995:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13039 "Parser/parser.cc"
    break;

  case 996:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13045 "Parser/parser.cc"
    break;

  case 997:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 998:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13057 "Parser/parser.cc"
    break;

  case 999:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13063 "Parser/parser.cc"
    break;

  case 1001:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13069 "Parser/parser.cc"
    break;

  case 1002:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13075 "Parser/parser.cc"
    break;

  case 1004:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13081 "Parser/parser.cc"
    break;

  case 1005:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13087 "Parser/parser.cc"
    break;

  case 1007:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13093 "Parser/parser.cc"
    break;

  case 1008:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13099 "Parser/parser.cc"
    break;

  case 1009:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13105 "Parser/parser.cc"
    break;

  case 1010:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13111 "Parser/parser.cc"
    break;

  case 1011:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13117 "Parser/parser.cc"
    break;

  case 1012:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13123 "Parser/parser.cc"
    break;

  case 1013:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13129 "Parser/parser.cc"
    break;

  case 1016:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13135 "Parser/parser.cc"
    break;

  case 1017:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13141 "Parser/parser.cc"
    break;

  case 1018:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13147 "Parser/parser.cc"
    break;

  case 1019:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13153 "Parser/parser.cc"
    break;

  case 1020:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13159 "Parser/parser.cc"
    break;

  case 1021:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13165 "Parser/parser.cc"
    break;

  case 1022:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13171 "Parser/parser.cc"
    break;

  case 1023:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13177 "Parser/parser.cc"
    break;

  case 1025:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13183 "Parser/parser.cc"
    break;

  case 1026:
#line 3960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13189 "Parser/parser.cc"
    break;

  case 1027:
#line 3962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13195 "Parser/parser.cc"
    break;

  case 1028:
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13201 "Parser/parser.cc"
    break;

  case 1029:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13207 "Parser/parser.cc"
    break;

  case 1030:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13213 "Parser/parser.cc"
    break;

  case 1032:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13219 "Parser/parser.cc"
    break;

  case 1034:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13225 "Parser/parser.cc"
    break;

  case 1035:
#line 3992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13231 "Parser/parser.cc"
    break;

  case 1036:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13237 "Parser/parser.cc"
    break;

  case 1037:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13243 "Parser/parser.cc"
    break;

  case 1038:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13249 "Parser/parser.cc"
    break;

  case 1039:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13255 "Parser/parser.cc"
    break;

  case 1041:
#line 4016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13261 "Parser/parser.cc"
    break;

  case 1042:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13267 "Parser/parser.cc"
    break;

  case 1043:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13273 "Parser/parser.cc"
    break;

  case 1044:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13279 "Parser/parser.cc"
    break;

  case 1045:
#line 4027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13285 "Parser/parser.cc"
    break;

  case 1046:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13291 "Parser/parser.cc"
    break;

  case 1047:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13297 "Parser/parser.cc"
    break;

  case 1049:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13303 "Parser/parser.cc"
    break;

  case 1050:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13309 "Parser/parser.cc"
    break;

  case 1051:
#line 4041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13315 "Parser/parser.cc"
    break;

  case 1052:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13321 "Parser/parser.cc"
    break;

  case 1053:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13327 "Parser/parser.cc"
    break;

  case 1056:
#line 4058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13333 "Parser/parser.cc"
    break;

  case 1059:
#line 4069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13339 "Parser/parser.cc"
    break;

  case 1060:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13345 "Parser/parser.cc"
    break;

  case 1061:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13351 "Parser/parser.cc"
    break;

  case 1062:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13357 "Parser/parser.cc"
    break;

  case 1063:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13363 "Parser/parser.cc"
    break;

  case 1064:
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13369 "Parser/parser.cc"
    break;

  case 1065:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13375 "Parser/parser.cc"
    break;

  case 1066:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13381 "Parser/parser.cc"
    break;

  case 1067:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13387 "Parser/parser.cc"
    break;

  case 1068:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13393 "Parser/parser.cc"
    break;

  case 1069:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13399 "Parser/parser.cc"
    break;

  case 1070:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13405 "Parser/parser.cc"
    break;

  case 1071:
#line 4099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13411 "Parser/parser.cc"
    break;

  case 1072:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13417 "Parser/parser.cc"
    break;

  case 1073:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13423 "Parser/parser.cc"
    break;

  case 1074:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13429 "Parser/parser.cc"
    break;

  case 1075:
#line 4110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13435 "Parser/parser.cc"
    break;

  case 1076:
#line 4112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13441 "Parser/parser.cc"
    break;

  case 1077:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13447 "Parser/parser.cc"
    break;

  case 1078:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13453 "Parser/parser.cc"
    break;

  case 1080:
#line 4146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13459 "Parser/parser.cc"
    break;

  case 1084:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13465 "Parser/parser.cc"
    break;

  case 1085:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13471 "Parser/parser.cc"
    break;

  case 1086:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13477 "Parser/parser.cc"
    break;

  case 1087:
#line 4163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13483 "Parser/parser.cc"
    break;

  case 1088:
#line 4165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13489 "Parser/parser.cc"
    break;

  case 1089:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13495 "Parser/parser.cc"
    break;

  case 1090:
#line 4174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13501 "Parser/parser.cc"
    break;

  case 1091:
#line 4176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13507 "Parser/parser.cc"
    break;

  case 1092:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13513 "Parser/parser.cc"
    break;

  case 1093:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13519 "Parser/parser.cc"
    break;

  case 1094:
#line 4182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13525 "Parser/parser.cc"
    break;

  case 1095:
#line 4184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13531 "Parser/parser.cc"
    break;

  case 1096:
#line 4189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 1097:
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13543 "Parser/parser.cc"
    break;

  case 1098:
#line 4193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13549 "Parser/parser.cc"
    break;

  case 1099:
#line 4200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13555 "Parser/parser.cc"
    break;

  case 1100:
#line 4202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13561 "Parser/parser.cc"
    break;

  case 1103:
#line 4226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13567 "Parser/parser.cc"
    break;

  case 1104:
#line 4228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13573 "Parser/parser.cc"
    break;


#line 13577 "Parser/parser.cc"

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
#line 4231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
