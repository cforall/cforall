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
	DeclarationNode * cur = declList, * cl = (new DeclarationNode)->addType( typeSpec );
	// printf( "distAttr2 cl %p\n", cl ); cl->type->print( std::cout );
	// cl->type->aggregate.name = cl->type->aggInst.aggregate->aggregate.name;

	for ( cur = dynamic_cast<DeclarationNode *>( cur->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
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
#define MISSING_ANON_FIELD "Missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "Missing low value for up-to range so index is uninitialized."
#define MISSING_HIGH "Missing high value for down-to range so index is uninitialized."

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
		SemanticError( yylloc, "Direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "Multiple loop indexes disallowed in for-loop declaration." );
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
			SemanticError( yylloc, "Expression disallowed. Only loop-index name allowed." ); return nullptr;
		} // if
	} else {
		SemanticError( yylloc, "Expression disallowed. Only loop-index name allowed." ); return nullptr;
	} // if
} // forCtrl

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, ::toString( "Adjacent identifiers \"", identifier1, "\" and \"", identifier2, "\" are not meaningful in a", kind, ".\n"
				   "Possible cause is misspelled type name or missing generic parameter." ) );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, ::toString( "Identifier \"", identifier, "\" cannot appear before a ", kind, ".\n"
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

#line 701 "Parser/parser.cc"

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
#define YYLAST   23979

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  309
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1103
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2226

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
       0,   605,   605,   609,   616,   617,   618,   619,   620,   624,
     625,   626,   627,   628,   629,   630,   631,   635,   636,   640,
     641,   646,   650,   651,   662,   664,   666,   670,   671,   673,
     675,   677,   679,   689,   691,   693,   695,   697,   699,   704,
     705,   716,   721,   726,   727,   732,   738,   740,   742,   748,
     750,   754,   756,   758,   760,   762,   764,   766,   768,   770,
     772,   774,   776,   778,   780,   782,   784,   794,   795,   799,
     800,   805,   808,   812,   813,   817,   818,   820,   822,   824,
     826,   828,   833,   835,   837,   845,   846,   854,   857,   858,
     860,   865,   881,   883,   885,   887,   889,   891,   893,   895,
     897,   905,   906,   908,   912,   913,   914,   915,   919,   920,
     922,   924,   926,   928,   930,   932,   934,   941,   942,   943,
     944,   948,   949,   953,   954,   959,   960,   962,   964,   969,
     970,   972,   977,   978,   980,   985,   986,   988,   990,   992,
     997,   998,  1000,  1005,  1006,  1011,  1012,  1017,  1018,  1023,
    1024,  1029,  1030,  1035,  1036,  1039,  1044,  1049,  1050,  1058,
    1064,  1065,  1069,  1070,  1074,  1075,  1079,  1080,  1081,  1082,
    1083,  1084,  1085,  1086,  1087,  1088,  1089,  1099,  1101,  1106,
    1107,  1109,  1111,  1116,  1117,  1123,  1124,  1130,  1131,  1132,
    1133,  1134,  1135,  1136,  1137,  1138,  1139,  1140,  1141,  1143,
    1144,  1150,  1152,  1162,  1164,  1172,  1173,  1178,  1180,  1182,
    1184,  1186,  1190,  1191,  1193,  1198,  1205,  1207,  1209,  1219,
    1221,  1223,  1228,  1233,  1236,  1241,  1243,  1245,  1247,  1255,
    1256,  1258,  1262,  1264,  1268,  1270,  1271,  1273,  1275,  1280,
    1281,  1285,  1290,  1291,  1295,  1297,  1302,  1304,  1309,  1311,
    1313,  1315,  1320,  1322,  1324,  1326,  1331,  1333,  1338,  1339,
    1361,  1363,  1368,  1371,  1373,  1376,  1378,  1381,  1383,  1388,
    1393,  1395,  1400,  1405,  1407,  1409,  1411,  1413,  1416,  1418,
    1421,  1423,  1428,  1434,  1437,  1439,  1444,  1450,  1452,  1457,
    1463,  1466,  1468,  1471,  1473,  1478,  1485,  1487,  1492,  1498,
    1500,  1505,  1511,  1514,  1519,  1527,  1529,  1531,  1536,  1538,
    1543,  1544,  1546,  1551,  1553,  1558,  1560,  1562,  1564,  1567,
    1571,  1574,  1578,  1580,  1582,  1584,  1586,  1588,  1590,  1592,
    1594,  1596,  1598,  1603,  1604,  1608,  1614,  1622,  1627,  1628,
    1632,  1633,  1639,  1643,  1644,  1647,  1649,  1654,  1657,  1659,
    1661,  1664,  1666,  1671,  1676,  1677,  1681,  1686,  1688,  1693,
    1695,  1700,  1702,  1704,  1706,  1709,  1711,  1716,  1722,  1724,
    1726,  1731,  1733,  1739,  1740,  1744,  1745,  1746,  1747,  1751,
    1756,  1757,  1759,  1761,  1763,  1767,  1771,  1772,  1776,  1778,
    1780,  1782,  1784,  1790,  1791,  1797,  1798,  1802,  1803,  1808,
    1810,  1819,  1820,  1822,  1827,  1832,  1843,  1844,  1848,  1849,
    1855,  1856,  1860,  1862,  1866,  1868,  1872,  1873,  1877,  1878,
    1882,  1889,  1890,  1894,  1896,  1911,  1912,  1913,  1914,  1916,
    1920,  1922,  1926,  1933,  1935,  1937,  1942,  1943,  1945,  1947,
    1949,  1981,  1984,  1989,  1991,  1997,  2002,  2007,  2018,  2025,
    2030,  2032,  2034,  2040,  2044,  2051,  2053,  2054,  2055,  2071,
    2073,  2076,  2078,  2081,  2086,  2087,  2091,  2092,  2093,  2094,
    2104,  2105,  2106,  2115,  2116,  2117,  2121,  2122,  2123,  2132,
    2133,  2134,  2139,  2140,  2149,  2150,  2155,  2156,  2160,  2162,
    2164,  2166,  2168,  2173,  2178,  2179,  2181,  2191,  2192,  2197,
    2199,  2201,  2203,  2205,  2207,  2210,  2212,  2214,  2219,  2221,
    2223,  2225,  2227,  2229,  2231,  2233,  2235,  2237,  2239,  2241,
    2243,  2245,  2247,  2249,  2251,  2253,  2255,  2257,  2259,  2261,
    2263,  2265,  2267,  2269,  2271,  2273,  2278,  2279,  2283,  2290,
    2291,  2297,  2298,  2300,  2302,  2304,  2309,  2311,  2316,  2317,
    2319,  2321,  2326,  2328,  2330,  2332,  2334,  2336,  2341,  2348,
    2350,  2352,  2357,  2365,  2364,  2368,  2376,  2377,  2379,  2381,
    2386,  2387,  2389,  2394,  2395,  2397,  2399,  2404,  2405,  2407,
    2412,  2414,  2416,  2418,  2419,  2421,  2426,  2428,  2430,  2435,
    2442,  2446,  2447,  2452,  2451,  2456,  2455,  2465,  2464,  2475,
    2474,  2484,  2489,  2490,  2495,  2501,  2519,  2520,  2524,  2526,
    2528,  2534,  2536,  2538,  2540,  2545,  2547,  2552,  2554,  2563,
    2564,  2569,  2578,  2580,  2582,  2591,  2593,  2594,  2595,  2597,
    2599,  2600,  2605,  2606,  2607,  2612,  2614,  2617,  2620,  2627,
    2628,  2629,  2635,  2640,  2642,  2648,  2649,  2655,  2656,  2660,
    2665,  2668,  2667,  2671,  2674,  2681,  2686,  2685,  2694,  2699,
    2704,  2709,  2714,  2715,  2720,  2722,  2727,  2729,  2731,  2733,
    2738,  2739,  2745,  2746,  2747,  2754,  2755,  2757,  2758,  2759,
    2761,  2763,  2770,  2771,  2773,  2775,  2780,  2781,  2787,  2788,
    2790,  2791,  2796,  2797,  2798,  2800,  2808,  2809,  2811,  2814,
    2816,  2820,  2821,  2822,  2824,  2826,  2831,  2833,  2838,  2840,
    2849,  2851,  2856,  2857,  2858,  2862,  2863,  2864,  2869,  2870,
    2875,  2876,  2877,  2878,  2882,  2883,  2888,  2889,  2890,  2891,
    2892,  2906,  2907,  2912,  2913,  2919,  2921,  2924,  2926,  2928,
    2951,  2952,  2958,  2959,  2965,  2964,  2974,  2973,  2977,  2983,
    2989,  2990,  2992,  2996,  3001,  3003,  3005,  3007,  3013,  3014,
    3018,  3019,  3024,  3026,  3033,  3035,  3036,  3038,  3043,  3045,
    3047,  3052,  3054,  3059,  3064,  3072,  3077,  3079,  3084,  3089,
    3090,  3095,  3096,  3100,  3101,  3102,  3107,  3109,  3115,  3117,
    3122,  3124,  3130,  3131,  3135,  3139,  3143,  3145,  3158,  3160,
    3162,  3164,  3166,  3168,  3170,  3171,  3176,  3179,  3178,  3190,
    3189,  3202,  3201,  3213,  3212,  3224,  3223,  3237,  3243,  3245,
    3251,  3252,  3263,  3270,  3275,  3281,  3284,  3287,  3291,  3297,
    3300,  3303,  3308,  3309,  3310,  3311,  3315,  3321,  3322,  3332,
    3333,  3337,  3338,  3343,  3348,  3349,  3355,  3356,  3358,  3363,
    3364,  3365,  3366,  3367,  3369,  3404,  3406,  3411,  3413,  3414,
    3416,  3421,  3423,  3425,  3427,  3432,  3434,  3436,  3438,  3440,
    3442,  3444,  3449,  3451,  3453,  3455,  3464,  3466,  3467,  3472,
    3474,  3476,  3478,  3480,  3485,  3487,  3489,  3491,  3496,  3498,
    3500,  3502,  3504,  3506,  3518,  3519,  3520,  3524,  3526,  3528,
    3530,  3532,  3537,  3539,  3541,  3543,  3548,  3550,  3552,  3554,
    3556,  3558,  3570,  3575,  3580,  3582,  3583,  3585,  3590,  3592,
    3594,  3596,  3601,  3603,  3605,  3607,  3609,  3611,  3613,  3618,
    3620,  3622,  3624,  3633,  3635,  3636,  3641,  3643,  3645,  3647,
    3649,  3654,  3656,  3658,  3660,  3665,  3667,  3669,  3671,  3673,
    3675,  3685,  3687,  3689,  3690,  3692,  3697,  3699,  3701,  3706,
    3708,  3710,  3712,  3717,  3719,  3721,  3735,  3737,  3739,  3740,
    3742,  3747,  3749,  3754,  3756,  3758,  3763,  3765,  3770,  3772,
    3789,  3790,  3792,  3797,  3799,  3801,  3803,  3805,  3810,  3811,
    3813,  3815,  3820,  3822,  3824,  3830,  3832,  3835,  3838,  3840,
    3844,  3846,  3848,  3849,  3851,  3853,  3857,  3859,  3864,  3866,
    3868,  3870,  3905,  3906,  3910,  3911,  3913,  3915,  3920,  3922,
    3924,  3926,  3928,  3933,  3934,  3936,  3938,  3943,  3945,  3947,
    3953,  3954,  3956,  3965,  3968,  3970,  3973,  3975,  3977,  3991,
    3992,  3994,  3999,  4001,  4003,  4005,  4007,  4012,  4013,  4015,
    4017,  4022,  4024,  4032,  4033,  4034,  4039,  4040,  4045,  4047,
    4049,  4051,  4053,  4055,  4062,  4064,  4066,  4068,  4070,  4073,
    4075,  4077,  4079,  4081,  4086,  4088,  4090,  4095,  4121,  4122,
    4124,  4128,  4129,  4133,  4135,  4137,  4139,  4141,  4143,  4150,
    4152,  4154,  4156,  4158,  4160,  4165,  4167,  4169,  4176,  4178,
    4196,  4198,  4203,  4204
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

#define YYPACT_NINF (-1888)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1102)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     291, 12750,   417,   435,  9580,   164, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,   369,  1064,
     472, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,    57,   544,
   -1888, -1888, -1888, -1888, -1888, -1888,  3047,  3047,   498, 12750,
     573,   597, 23657, -1888,   603, -1888, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888, -1888,  2282, -1888,   766,   491, -1888,
   -1888, -1888, -1888, -1888, 17683, -1888, -1888,   434,   642,   593,
     159, -1888,  3047,   642,   642,   642,   519,  4417,   806,  1017,
   12912, -1888, -1888,   754, 17531,  1897, -1888, -1888, -1888,  2950,
     881, 10405, 11283,   953,  2950,  1070,   741, -1888, -1888, -1888,
   -1888,   841, -1888, -1888, -1888, -1888,   767, -1888, -1888, -1888,
   -1888, -1888,   764,   788,   841, -1888,   841,   804, -1888, -1888,
   -1888, 18678,  3047, -1888, -1888,  3047, -1888, 12750, -1888,   802,
   18830, -1888, -1888,  4484, 20251, -1888, -1888,  1081,  1081,   838,
    3233, -1888, -1888, -1888, -1888,   452, 14969,  3332,   841, -1888,
   -1888, -1888, -1888, -1888, -1888,   835, -1888,   832,   862,   869,
   -1888,   937, 23048, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
   16450,  3789,  2282,   827,   908,   921,   928,   945,   950,   975,
   -1888, -1888, 18982, 11691,   939, -1888, 17980, -1888, -1888, -1888,
   -1888,  1012, -1888, -1888,  1031, -1888, 21124,  1157, 21272, -1888,
    1039,  3047,   788,  1045,  1047,  1049,  1055, -1888, -1888, -1888,
    3410,  3348,  1057,  1132,   424,  1132, -1888,   841,   841,   -32,
      43,   439,  1132, -1888,   841,   841,   -32,   841, -1888,   841,
   -1888,  3723, -1888, -1888,  1117,  1120,  1081, 20447, -1888, 17683,
   -1888, -1888,  2950, -1888,  1831,   741,  1119,  1152,    43,  3047,
    3047,   593, -1888, 14486, -1888,  1081,  1081,  1130,  1152,    43,
    3047, -1888, 23856, -1888, -1888, -1888,  1081, -1888, -1888, -1888,
   -1888,  1081, -1888,   833,  4695,  3047, -1888,  2673,  1146, -1888,
   -1888, -1888, 17344,   788,    59, -1888, -1888, 20396, -1888,  1132,
     306, -1888, 23048, 20251,  4094,  3723, -1888,   440, -1888, -1888,
   -1888, -1888, -1888, 18830,  3047, -1888,  1154, -1888, -1888, -1888,
   -1888,  3047,  2547,   587,   349, -1888,  3047,   832, -1888,   963,
     841,   841,  1160, 19134,   883, 15452, 20949,  2950,  2950, -1888,
    2950,  1081,  2950,  1081, -1888, -1888,   841, -1888,  1123, -1888,
   19286, -1888, -1888, -1888, 19438,  1012, -1888,   142,   733,    90,
     538,   741,  1159, -1888,  3233,  1163,   832,  3233,  1564, -1888,
    1191,  1243, 23122,  1212,  1219,  1221, 23048, 23196,  1235, 23761,
   -1888, -1888, -1888, -1888, -1888, -1888, 23270, 23270, 16294,  1231,
    4209, -1888, -1888, -1888, -1888,    36, -1888,   693, -1888,  1188,
   -1888, 23048, 23048, -1888,  1225,   675,    18,  1021,   500,  1127,
    1233,  1230,  1240,  1284,   282, -1888,   454, -1888,  1246, -1888,
    1166,  2877, 16762, -1888, -1888,   828,  1246, -1888, -1888,   575,
   -1888, -1888,  3789,  1265,  1268,  1277,  1282,  1290,  1293, -1888,
   -1888,   453,  1251, -1888,   809,  1251, -1888, -1888, 18678, -1888,
    1195,  1272, 16918, -1888, -1888,  4188,  2693,  1304, 15452,  1336,
     757,   820, -1888, -1888, -1888, -1888, -1888,  3047,  4849, -1888,
   -1888, -1888, -1888, -1888, -1888, 17239,  3179,  1231, 21124,  1314,
    1350, -1888, -1888,  1305, 21272,   894, -1888, -1888, -1888, 21346,
    1359, -1888, -1888, -1888, -1888, -1888,  3410,   951,  1368,  1378,
    1388,  1003,  1393,  1415,  1426,  1428,  1437,  1441,  3348, -1888,
   -1888, -1888,   841,  1372,  1360,  1453, -1888, -1888,  1442,   593,
   -1888, -1888,   788,  1152, -1888, -1888, -1888,   593, -1888, -1888,
     788, -1888, -1888,  3723, -1888, 16762, 16762, -1888,  1081,  4484,
   21037, 15613, -1888, -1888, -1888, -1888, -1888,   788,  1152,   306,
    1458, -1888, -1888,  2950,  1469,  1152,    43, -1888,   788,  1152,
   -1888, 23907, -1888,  1081,  1081, -1888, -1888,  1475,   -33,  1477,
     741,  1504, -1888, 17844, -1888,   836, -1888,  1543, 20846, -1888,
    4484, 17432, 20447, -1888, 17344, 23344, -1888, -1888, -1888, -1888,
   -1888,  4094,  1028,  3723, -1888, 15613,  1132, 12750, -1888,  1465,
   -1888,  1515, -1888, -1888, -1888, -1888, -1888,  3233, -1888, -1888,
    1607,  4753,  2564, 19438, 11691, -1888, 19590, -1888,  1081,  1081,
   -1888, -1888,  1012, -1888,   888,  1529,  1669, 23048,   905,  1442,
    1513, -1888,   841,   841, -1888,  1251, -1888, 19134, -1888, -1888,
   18275,  1081,  1081, -1888,  4753,   841, -1888, 20106, -1888, -1888,
   19286, -1888,   452, -1888, -1888, -1888,  1532,  3047,  1159,  1531,
     884, 18830,   902, -1888, -1888, -1888, -1888, -1888, -1888,   916,
   -1888,  1541,  1518, -1888, 16606, -1888,  4209, 19742, 19742, -1888,
   16606, -1888, 23048, -1888, -1888, -1888, -1888, -1888, -1888, 16606,
   -1888, -1888, 18374, 19742, 19742,  1166,  1462,  1550,   792,  1732,
   -1888,   922,  1544,  1214,  1545, -1888, 21346, 23048, 21420,  1540,
   23048,  2673, 23048,  2673, -1888,  2602, -1888, -1888, 21494,   749,
   23048, 21494,  2673, -1888, -1888, 23048, 23048, 23048, 23048, 23048,
   23048, 23048, 23048, 23048, 23048, 23048, 23048, 23048, 23048, 23048,
   23048, 23048, 23048, 23048, 21568,  1525,   937,  4433, 11691, -1888,
   -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
    1553, 23048, -1888, -1888,   828,  2139, -1888, -1888,   841,   841,
   -1888, -1888, 16762, -1888,   479,  1251, -1888,   971,  1251, -1888,
   -1888, -1888,  1442, -1888, -1888,  1442, 23418, -1888, -1888, 11691,
    1546,  1547,  3446,  1697,  2951,   490,  1513, -1888,   841,   841,
    1513,   560, -1888,   841,   841, 23048,  3047,  1223,  1227,  1513,
     113, 14808, 14808,  3047, -1888, -1888, 23048,  1305, -1888, 21124,
    1565, -1888,  2215, -1888, -1888, -1888, -1888, -1888,  1002, -1888,
   14808,  2673,  4484,  2673,   984,  1563,  1567,  1568,  1011,  1570,
    1571,  1573,  1575,  1578,  1579,   571,  1251, -1888, -1888,   583,
    1251, -1888, -1888,   625,  1251, -1888, -1888, -1888,  4484,   937,
    1701,  1251, 20541, -1888, -1888,   788, 17844, -1888, -1888, -1888,
    1027,  1581,  1054,  1582, -1888,  1586, -1888,   788, -1888,  1587,
   -1888,   788,  1152,  1586, -1888,   788,  1559,  1580,  1583, -1888,
   -1888, 18275, -1888,  1588, -1888, -1888, -1888,  2673,  3047, 10759,
    1671,  1574, -1888, -1888, 19903, -1888,  1272, -1888, 14808,  1058,
   -1888, -1888,  1586, -1888, 18830, 16762,  1577, -1888,  1577, -1888,
   -1888, -1888,    90,   841,   841, -1888, 19286, -1888, 11856, 17074,
   -1888, 17844,  1600,  1602,  1604, -1888,  9991,   841, -1888,   905,
   -1888, -1888, -1888, -1888,  1442, -1888, -1888, -1888,  1081, -1888,
    3663, -1888, -1888,   741,   153,  1609,  1591,  1603,    90, -1888,
   -1888,  1613,  1608,  1564, 21494, -1888,  1610,  1611,   491,  1616,
    1625,  1626,  1623,  1628, 23048,  1629,  1631,  1633, 11691, 23048,
   -1888, -1888,  1787, -1888, -1888, -1888, 23048, -1888,  1635,  1636,
   21198,  1253, -1888, 21494,  1634, -1888,  1637, -1888, -1888,  4812,
   -1888, -1888,  1082, -1888, -1888, -1888, -1888,  4812, -1888, -1888,
    1269,   214, -1888, -1888,  1225,  1225,  1225,   675,   675,    18,
      18,  1021,  1021,  1021,  1021,   500,   500,  1127,  1233,  1230,
    1240,  1284, 23048,  1275, -1888,  1639,  4812, -1888, -1888, 21124,
   -1888, 17844,  1643,  1652,  1654,  2139, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888,  1442, -1888, -1888,  1442, 17844, 17844,
   -1888, -1888,  3446,  1038,  1655,  1659,  1661,  1662,  1886,  2951,
   -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888,  1663, -1888,  1513, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888, -1888,  1664,  1668, -1888,   593,  4812,
    1302,   261, -1888, -1888,  1672, -1888, 21272, -1888, 23048,   841,
   21642, 14808, -1888, -1888, -1888,  1648,   628,  1251, -1888,   696,
    1251, -1888, -1888,   722,  1251, -1888, -1888, -1888,  1442, -1888,
   -1888, -1888,  1442, -1888, -1888, -1888,  1442,  1132,  1673, -1888,
    1442,   387, -1888,  1246,  1670, -1888, -1888, -1888, -1888, -1888,
   -1888,  1675, -1888, -1888, -1888, 18830,  1586, -1888,   788, -1888,
   -1888, -1888, -1888, -1888, 13553,  1676,  1674, -1888,   115, -1888,
     653,   331, 11526,  1679, 15973,  1681,  1683,  2376,  2451,  2530,
   21716,  1685, -1888, -1888,  1687,  1688, -1888, -1888,   788, 23048,
   23048,  1828,  1686,   736, -1888, 16138,  1770,  1689,  1677, -1888,
   -1888, -1888, 10584, -1888, -1888, -1888, -1888, -1888,  1406, -1888,
   -1888, -1888,  1374,   461, -1888,   494, -1888,   461, -1888, -1888,
   -1888,  2673, -1888, -1888, 13074, 17683,  1691, -1888,  3047, -1888,
    1680,  1694,  1695, -1888,  1329, -1888, -1888, -1888, -1888,  4484,
   -1888, -1888,  1684,  1690,  1084, 18830,   832,   832,  1532,  1159,
    1159, -1888, -1888,  1231,  1272, 16918, -1888,  1246, -1888, 12021,
   -1888,   730,  1251, -1888,  1081, 12584, -1888, -1888,    90,   841,
     841,   452,  3047, -1888, 21790, -1888,    90,  1532,  1709, -1888,
   -1888,  1089,   773, 18275, 11691,  2673, -1888,   773, 18526,   773,
   -1888, 23048, 23048, 23048, -1888, -1888, -1888, -1888, 23048, 23048,
    1703, 21124, -1888, -1888,  1706,   776, -1888, -1888, -1888,  3946,
   -1888, -1888,  1334, -1888,   327, -1888, 21494,  1338, -1888, 21346,
   -1888, -1888, 23048,  1696,  1382,  1387,  1305, -1888,   742,  1251,
   -1888, -1888, 17844, 17844, -1888, -1888,  1720,   758,  1251, -1888,
     797,  3549,   841,   841, -1888, -1888, 17844, 17844, -1888,  1718,
   -1888, 15613, 15613,  1724,  1721,  1722,  1727, -1888,  1728, 23048,
   23048,  1392,  1725, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
    1756, 23048, -1888, -1888, -1888,  1442, -1888, -1888, -1888,  1442,
   -1888, -1888, -1888,  1442, 17844, 17844, 17844,   593,   841, -1888,
   -1888,  1397, 23048, 20690,  1765,  1769,  1773, -1888, -1888, -1888,
    1774, 13708, 13863, 14018, 18830, 20447, 19742, 19742,  1775, -1888,
    1749,  1751,  1112, 14325, -1888,   357,  3047, -1888, -1888,  3047,
   -1888, 21494,    17,   285, -1888, -1888, -1888, -1888, 23048,  1778,
    1768, 11360, 10934, -1888,  1755, -1888,  1757, 23048,  1758, 21124,
    1762, 23048, 21346, 23048,  1061, -1888,  1763,    77, -1888,    95,
    1851,   280,  1792, -1888, -1888,  1794, -1888,  1771, -1888,  1772,
    1798,  1801, 15973, 15973, -1888, -1888,  1864, -1888, -1888,   184,
     184,   864, 14647,   841,   392, -1888, -1888, -1888,  1802, -1888,
    1806, -1888,  1809, -1888,  1803, -1888,  1805, -1888, -1888, -1888,
   -1888,  1814,  1810,  1811, 12186,  1815,  1817,  1818, -1888,  1823,
   -1888, -1888, -1888,  1442, 23048, 23048,  1272,  1821, -1888,  1532,
   -1888,  1159,   405,  1591, 21124, -1888,  1532,  1829, -1888, 18830,
   -1888,   982,  1827,  1824,  1096, -1888,  1825, -1888, -1888, -1888,
   -1888, -1888, 21124,  1305, 21346, -1888,  1866,  4812, -1888,  1866,
    1866, -1888,  4812,  4548,  4571, -1888, -1888,  1400, -1888, -1888,
   -1888,  1834,  1835, -1888, -1888, -1888,  1442, -1888, -1888,  1848,
    1849,   841, -1888, -1888, -1888,  1442, -1888, -1888, -1888,  1852,
   -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888,  1853, -1888, -1888, -1888, -1888,  1856,  1854,
     841, -1888, 17844, 17844, 17844, -1888, -1888, -1888, -1888, -1888,
   23048, -1888,   387, -1888,  1246, -1888, -1888, -1888,  1837,  1863,
   -1888,  1775,  1775,  1775,  3869,   987,  1839,   482, -1888,  3869,
     499, 16762, -1888, -1888, -1888,  4235, 23048,  4173,   512, -1888,
   -1888,    26,  1858,  1858,  1858,  3047, -1888, -1888, 18141, -1888,
    1098, -1888, -1888, -1888, -1888,  1126,  1869, 15973,  1689,  1868,
   23048,   434,  1860,   519, 14180, 18830, -1888, -1888, -1888,   615,
   15973, 23048,  1115,   716, -1888, 23048,  8027, -1888, -1888,   527,
   -1888,  1305, -1888,  1128,  1135,  1137, -1888, -1888, -1888, -1888,
     788,  1061,  1872, -1888, -1888, 23048, -1888,  1873,   937, -1888,
   11526, -1888, -1888, -1888, -1888, 23048, 23048, -1888, -1888,   470,
     184, -1888,   602, -1888, -1888,  9730, -1888,   841, 15613, -1888,
   -1888, 18830, -1888, -1888, -1888,    90,    90, -1888, -1888, -1888,
    1870, -1888, 17844, -1888, -1888,  1871, -1888,  1874,  1878,  1159,
    1875, -1888, -1888,  1305,  1881, -1888, -1888,  1879, -1888, -1888,
   23048, -1888, 18526, 23048,  1305,  1885,  1414, -1888,  1416, -1888,
    4812, -1888,  4812, -1888, -1888, -1888, -1888, 17844,  1887,  1894,
   -1888, -1888, 17844, 17844,  1899,  1901,  1421, 15130, 15291, -1888,
    1902, -1888, -1888, -1888, -1888, -1888,  1904,  1906,  1907,  1427,
   23048, -1888, -1888, -1888, -1888, -1888,   589,   987,  2301,   648,
   -1888, -1888, -1888, -1888,   841,   841, -1888, -1888, -1888,   650,
   -1888,  1171,  4235,   955, -1888,  4173,   841, -1888, -1888, -1888,
   -1888, -1888, -1888, -1888, -1888, -1888, 15973,   389, 21864,  1987,
   15973,  1689, 15774, -1888, -1888, -1888, -1888, 23048, -1888, 21938,
    1989,  1888, 21047, 22012, 15973, 11109,  1689,   649,  1228,  1890,
   23048, -1888,  1917,   459, 15973, -1888, -1888,  1918, -1888, -1888,
    1896,   937,   743,  1921,  1922,  1429,  1178, 15973,  1919, 15973,
   15973, 15973, 15973, -1888, -1888, -1888, -1888,  3047,  4484,  1532,
    1532, -1888, -1888,  1920,  1924, -1888, -1888, -1888,  1923,    90,
    1928, -1888,  1933, -1888, -1888, -1888, -1888,  1934, -1888, -1888,
   -1888,  1432,  1445, -1888, -1888, -1888, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888,  1935, -1888, -1888,  1937,  1940,  1941, -1888,
   -1888, -1888, -1888, -1888, -1888,  1943,  1944,  1945,  2301, -1888,
     841, -1888, -1888, -1888, -1888, -1888,  1931,  3869, -1888,  3096,
     205, 12354, -1888, 15868, -1888,    98,  1180, 15973,  2013,   689,
    1936,   496, 15973, 23048,  1946,   649,  1228,  1925, 23492,  1938,
     617,  2028, -1888, 22086, 22160, 23048,  1689,  1930, 12518, -1888,
   -1888, -1888, 19954, -1888,  1950,  1947,   328, 15973, -1888, 23048,
   21494, -1888, -1888, 23048,   461, -1888, -1888,   461, -1888, -1888,
    1954,  1962,  1960, -1888, -1888,    90,  1532, -1888, -1888, -1888,
   -1888, -1888,  1961,  1968,  1969, 15613,  1967, -1888, -1888, -1888,
     799,  1251, -1888, -1888,   987, -1888, -1888,   375, -1888,   252,
   -1888, -1888, -1888,  1975, 13236, -1888, -1888, 15973, -1888,   120,
   -1888, 15973, 23048,  1977, 22234, -1888, -1888, 22308, 22382, 23048,
    1946,  1689, 22456, 22530, 15973,  1965,   815,  1971,   818, -1888,
   -1888,  1984, 13236, 19954, -1888,  4262, 19590,  2673,  1986, -1888,
    2024,  1995,   793,  1991, -1888,  2075, -1888,  1183,  1193,   399,
     509, -1888, -1888, -1888,  1532,  2000, -1888, -1888, -1888, -1888,
   -1888, -1888, -1888,  1442, -1888, 23048, -1888, 23048, -1888, -1888,
    1528, 13398, -1888, -1888, 15973, -1888, -1888,  1689, -1888, -1888,
    1689,  1992,   845,  1996,   851, -1888, -1888,  1689, -1888,  1689,
   -1888,  2004, 22604, 22678, 22752, -1888,  1528, -1888,  1988,  3012,
    3602, -1888, -1888, -1888,   328,  2006, 23048,  1990,   328,   328,
   15973, -1888, -1888, 15973,  2093, 15973,  2094,  2029, -1888, 17844,
   -1888, -1888, 15868, -1888,  1528, -1888, -1888,  2031, 22826, 22900,
   22974, -1888, -1888,  1689, -1888,  1689, -1888,  1689, -1888,  1988,
   23048,  2027,  3602,  2023,   937,  2035, -1888,   853, -1888, -1888,
   -1888, 15973, -1888, 15973, -1888, -1888, -1888, 10240,  2034, 15868,
   -1888, -1888,  1689, -1888,  1689, -1888,  1689,  2039,  2037, -1888,
     788,   937,  2041, -1888,  2017,   937, -1888, -1888,  2042, -1888,
   -1888, -1888, 10408, -1888,   788, -1888, -1888,  1449, 23048, -1888,
    1200, -1888, -1888,   937,  2673,  2043,  2018, -1888, -1888,  1205,
   -1888, -1888,  2021,  2673, -1888, -1888
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
     521,   522,   523,   524,   525,   526,   533,   534,   839,   536,
     609,   610,   613,   615,   611,   617,     0,     0,     0,   482,
       0,     0,    17,   580,   586,     9,    10,    11,    12,    13,
      14,    15,    16,   796,   103,     0,    20,     0,     2,   101,
     102,    18,    19,   855,   482,   797,   422,     0,   425,   720,
     427,   436,     0,   426,   456,   457,     0,     0,     0,     0,
     563,   484,   486,   492,   482,   494,   497,   548,   535,   466,
     541,   546,   468,   558,   467,   573,   577,   583,   562,   589,
     601,   839,   606,   607,   590,   661,   428,   429,     3,   804,
     817,   487,     0,     0,   839,   877,   839,     2,   894,   895,
     896,   482,     0,  1081,  1082,     0,     1,   482,    17,     0,
     482,   445,   446,     0,   563,   492,   476,   477,   478,   807,
       0,   612,   614,   616,   618,     0,   482,     0,   840,   841,
     608,   537,   713,   714,   712,   773,   768,   758,     0,     0,
     805,     0,     0,   499,   798,   802,   803,   799,   800,   801,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     581,   584,   482,   482,     0,  1083,   563,   884,   902,  1087,
    1080,  1078,  1085,   421,     0,   165,   726,   164,     0,   430,
       0,     0,     0,     0,     0,     0,     0,   420,   971,   972,
       0,     0,   455,   837,   839,   837,   858,   839,   839,   465,
       2,   839,   837,   915,   839,   839,   464,   839,   934,   839,
     912,     0,   556,   557,     0,     0,   482,   482,     2,   482,
     437,   485,   495,   549,     0,   578,     0,   820,     2,     0,
       0,   720,   438,   563,   542,   559,   574,     0,   820,     2,
       0,   498,   543,   550,   551,   469,   560,   471,   472,   470,
     565,   575,   579,     0,   593,     0,   790,     2,     2,   818,
     876,   878,   482,     0,     2,     2,  1091,   563,  1094,   837,
     837,     3,     0,   563,     0,     0,   448,   839,   832,   834,
     833,   835,     2,   482,     0,   794,     0,   754,   756,   755,
     757,     0,     0,   750,     0,   740,     0,   749,   760,     0,
     839,   839,     2,   482,  1102,   483,   482,   494,   473,   541,
     474,   566,   475,   573,   570,   591,   839,   592,     0,   701,
     482,   702,  1056,  1057,   482,   703,   705,   580,   586,   662,
     664,   665,   662,   842,     0,   771,   759,     0,   846,    22,
       0,    21,     0,     0,     0,     0,     0,     0,     0,    24,
      26,     4,     8,     5,     6,     7,     0,     0,   482,     2,
       0,   104,   105,   106,   107,    88,    25,    89,    43,    87,
     108,     0,     0,   123,   125,   129,   132,   135,   140,   143,
     145,   147,   149,   151,   153,   156,     0,    27,     0,   587,
       2,   108,   482,   157,   765,   716,   577,   718,   764,     0,
     715,   719,     0,     0,     0,     0,     0,     0,     0,   856,
     882,   839,   892,   900,   904,   910,     2,  1089,   482,  1092,
       2,   101,   482,     3,   700,     0,  1102,     0,   483,   541,
     566,   573,     3,     3,   682,   686,   696,   702,   703,     2,
     885,   903,  1079,     2,     2,    24,     0,     2,   726,    25,
       0,   724,   727,  1100,     0,     0,   733,   722,   721,     0,
       0,   822,     2,     2,     2,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   861,
     918,   941,   839,     0,   461,     2,   857,   865,   999,   720,
     859,   860,     0,   820,     2,   914,   922,   720,   916,   917,
       0,   933,   935,     0,   451,   482,   482,   547,   483,     0,
     563,   482,  1084,  1088,  1086,   564,   794,     0,   820,   837,
       0,   431,   439,   496,     0,   820,     2,   794,     0,   820,
     769,   544,   545,   561,   576,   582,   585,   580,   586,   604,
     605,     0,   770,   482,   710,     0,   203,   414,   482,     3,
       0,   563,   482,   819,   482,     0,   433,     2,   434,   791,
     453,     0,     0,     0,     2,   482,   837,   482,   794,     0,
       2,     0,   753,   752,   751,   746,   493,     0,   744,   761,
     539,     0,     0,   482,   482,  1058,   483,   479,   480,   481,
    1062,  1053,  1054,  1060,     2,     2,   102,     0,  1018,  1032,
    1102,  1014,   839,   839,  1023,  1030,   708,   482,   571,   704,
     483,   567,   568,   572,     0,   839,  1068,   483,  1073,  1065,
     482,  1070,     0,   671,   663,   670,  1100,     0,   662,     0,
       0,   482,     0,   854,   853,   849,   851,   852,   850,     0,
     844,   847,     0,    23,   482,    95,     0,   482,   482,    90,
     482,    97,     0,    33,    37,    38,    34,    35,    36,   482,
      93,    94,   482,   482,   482,     2,   104,   105,     0,     0,
     183,     0,     0,   607,     0,  1078,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    62,    63,    67,     0,
       0,    67,     0,    91,    92,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     164,     0,   162,   163,     2,   983,   717,   980,   839,   839,
     988,   588,   482,   883,   839,   893,   901,   905,   911,     2,
     886,   888,   890,     2,   906,   908,     0,  1090,  1093,   482,
       0,     0,     2,   102,  1018,   839,  1102,   953,   839,   839,
    1102,   839,   968,   839,   839,     3,   704,     0,     0,  1102,
    1102,   482,   482,     0,     2,   735,     0,  1100,   732,  1101,
       0,   728,     0,     2,   731,   734,   180,   179,     0,     2,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   839,   870,   874,   913,   839,
     927,   931,   939,   839,   949,   862,   919,   942,     0,     0,
       0,   995,     0,   459,   823,     0,   482,   460,   824,   452,
       0,     0,     0,     0,   450,     2,   825,     0,   435,     2,
     794,     0,   820,     2,   826,     0,     0,     0,     0,   619,
     689,   483,     3,     3,   693,   692,   897,     0,     0,   482,
     415,     0,   465,   464,   563,     3,   101,     3,   482,     0,
       3,   795,     2,   748,   482,   482,   742,   741,   742,   540,
     538,   664,   662,   839,   839,  1064,   482,  1069,   483,   482,
    1055,   482,     0,     0,     0,  1033,     0,   839,  1103,  1019,
    1020,   709,  1016,  1017,  1031,  1059,  1063,  1061,   569,   604,
       0,  1067,  1072,   667,   662,     0,   672,     0,   662,   774,
     772,     0,     0,   846,    67,   806,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,     0,
     122,   121,     0,   118,   117,    28,     0,    29,     0,     0,
       0,     0,     3,    67,     0,    52,     0,    53,    60,     0,
      59,    71,     0,    68,    69,    72,    55,     0,    54,    58,
       0,     0,    51,   124,   126,   127,   128,   130,   131,   133,
     134,   138,   139,   136,   137,   141,   142,   144,   146,   148,
     150,   152,     0,     0,   424,     0,     0,    30,     3,   726,
     158,   482,     0,     0,     0,   984,   985,   981,   982,   767,
     766,     2,   887,   889,   891,     2,   907,   909,   482,   482,
    1009,  1008,     2,     0,     0,     0,     0,     0,   839,  1019,
     956,   973,     2,   951,   959,   706,   954,   955,   707,     2,
     966,   976,   969,   970,     0,     3,  1102,   443,     2,  1095,
       2,   697,   698,   676,     3,     3,     3,     3,   720,     0,
     156,     0,     3,     3,     0,   729,     0,   723,     0,   839,
       0,   482,     3,   447,   449,     0,   839,   871,   875,   839,
     928,   932,   940,   839,   950,     2,   863,   866,   868,     2,
     920,   923,   925,     2,   943,   945,   947,   837,     0,   462,
     996,     3,  1000,  1001,     3,   828,     3,   553,   552,   555,
     554,     2,   795,   829,   776,   482,     2,   827,     0,   795,
     830,   619,   619,   619,   482,     0,     0,   711,     0,   418,
       0,     0,   482,     0,   338,     0,     0,     0,     0,     0,
     185,     0,   333,   334,     0,     0,   387,   386,     0,   160,
     160,   393,   580,   586,   200,   482,     0,   186,     0,   211,
     187,   188,   482,   205,   189,   190,   191,   192,     0,   193,
     194,   339,     0,   353,   195,   359,   361,   367,   196,   197,
     198,     0,   199,   207,   563,   482,     0,   209,     0,   412,
       0,     0,     0,     3,     0,   808,   795,   783,   784,     0,
       3,   779,     3,     3,     0,   482,   758,   758,  1100,   662,
     662,  1066,  1071,     2,   101,   482,     3,   578,     3,   483,
       3,   839,  1026,  1029,   482,     3,  1015,  1021,   662,   839,
     839,     0,     0,   650,     0,   666,   662,  1100,     2,   843,
     845,     0,    96,   482,   482,     0,   100,    98,   482,     0,
     112,     0,     0,     0,   116,   120,   119,   184,     0,     0,
       0,   726,   109,   177,     0,     0,    46,    47,    85,     0,
      85,    85,     0,    73,    75,    49,     0,     0,    45,     0,
      48,   155,     0,     0,     0,     0,  1100,     3,   839,   991,
     994,   986,   482,   482,     3,     3,     0,   839,   962,   965,
     839,     0,   839,   839,   957,   974,   482,   482,  1096,     0,
     699,   482,   482,     0,     0,     0,     0,   432,     3,     0,
       0,     0,     0,   725,   730,     3,   821,   182,   181,     3,
       0,     0,     2,   864,   867,   869,     2,   921,   924,   926,
       2,   944,   946,   948,   482,   482,   482,   720,   839,  1007,
    1006,     0,     0,     0,     0,     0,     0,     3,   795,   831,
       0,   482,   482,   482,   482,   482,   482,   482,   602,   631,
       3,     3,   632,   563,   620,     0,     0,   879,     2,     0,
     416,    67,     0,     0,   324,   325,   208,   210,     0,     0,
       0,   482,   482,   320,     0,   318,     0,     0,     0,   726,
       0,     0,     0,     0,     0,   161,     0,     0,   394,     0,
       0,     0,     0,     3,   215,     0,   206,     0,   315,     0,
       0,     0,   338,   338,   344,   343,   338,   355,   354,   338,
     338,     0,   563,   839,     0,   413,  1011,  1010,     0,     2,
       0,   786,     2,   781,     0,   782,     0,   762,   743,   747,
     745,     0,     0,     0,   482,     0,     0,     0,     3,     0,
       2,  1022,  1024,  1025,     0,     0,   101,     0,     3,  1100,
     656,   662,   672,   672,   726,   673,  1100,     0,   775,   482,
     848,  1012,     0,     0,     0,    39,     0,   113,   115,   114,
     111,   110,   726,  1100,     0,    66,    82,     0,    76,    83,
      84,    61,     0,     0,     0,    70,    57,     0,   154,   423,
      31,     0,     0,     2,   987,   989,   990,     3,     3,     0,
       0,   839,     2,   958,   960,   961,     2,   975,   977,     0,
     952,   967,     3,     3,  1097,     3,   684,   683,   687,  1099,
       2,     2,  1098,     0,     3,   836,   736,   737,     0,     0,
     839,   454,   482,   482,   482,     3,     3,     3,   463,   838,
       0,  1002,     0,  1003,  1004,   998,   936,   812,     2,     0,
     814,   602,   602,   602,   632,   639,   607,     0,   645,   632,
       0,   482,   594,   630,   626,     0,     0,     0,     0,   633,
     635,   839,   647,   647,   647,     0,   627,   643,   482,   419,
       0,   328,   329,   326,   327,     0,     0,   338,   225,     0,
       0,   227,   427,   226,   563,   482,   306,   305,   307,     0,
     338,   185,   265,     0,   258,     0,   185,   321,   319,     0,
     313,  1100,   322,     0,     0,     0,   375,   376,   377,   378,
       0,   368,     0,   369,   330,     0,   331,     0,     0,   358,
     482,   216,   204,   317,   316,     0,     0,   347,   357,     0,
     338,   360,     0,   362,   385,     0,   417,   839,   482,   810,
     763,   482,     2,     2,   655,   662,   662,  1074,  1075,  1076,
       0,  1027,   482,     3,     3,     0,  1035,     0,     0,   662,
       0,   669,   668,  1100,     0,   653,     3,     0,  1013,    99,
       0,    32,   482,     0,  1100,     0,     0,    86,     0,    74,
       0,    80,     0,    78,    44,   159,   992,   482,     0,     0,
     880,   898,   482,   482,     0,     0,     0,   482,   482,   739,
       0,   440,   442,     3,     3,     3,     0,     0,     0,     0,
       0,   778,   816,   598,   600,   596,     0,     0,  1042,     0,
     640,  1047,   642,  1039,   839,   839,   625,   646,   629,     0,
     628,     0,     0,     0,   649,     0,   839,   621,   636,   648,
     637,   638,   644,   691,   695,   694,   338,     0,     0,   246,
     338,   228,   563,   311,   309,   312,   308,     0,   310,     0,
     254,     0,   185,     0,   338,   482,   266,     0,   291,     0,
       0,   314,     0,     0,   338,   337,   379,     0,   370,     2,
       0,     0,     0,     0,   340,     0,     0,   338,     0,   338,
     338,   338,   338,   202,   201,   441,   780,     0,     0,  1100,
    1100,  1077,     3,     0,     0,  1034,  1036,   654,     0,   662,
       0,   652,     2,    50,    42,    40,    41,     0,    64,   178,
      77,     0,     0,     3,   881,   899,     3,     3,   963,   978,
     444,     2,   681,     3,   680,   738,     0,     0,     0,   872,
     929,   937,   997,  1005,   623,     0,     0,     0,  1043,  1044,
     839,   624,  1040,  1041,   622,   603,     0,     0,   336,     0,
       0,     0,   239,   338,   217,     0,     0,   338,   248,   263,
     274,   268,   338,   185,   303,     0,   278,     0,     0,   269,
     267,   256,   259,     0,     0,   185,   292,     0,     0,   220,
     335,     2,   482,   332,     0,     0,   395,   338,   345,     0,
      67,   356,   349,     0,   350,   348,   363,   364,   785,   787,
       0,     0,     0,  1037,  1038,   662,  1100,   674,   777,    65,
      81,    79,     0,     0,     0,   482,     0,   873,   930,   938,
     839,  1050,  1052,  1045,     0,   634,   234,   229,   232,     0,
     231,   238,   237,     0,   482,   241,   240,   338,   250,     0,
     247,   338,     0,     0,     0,   255,   260,     0,     0,   185,
     304,   279,     0,     0,   338,     0,   294,   295,   293,   262,
     323,     0,   482,   482,     3,   380,   483,   384,     0,   388,
       0,     0,     0,   396,   397,   223,   341,     0,     0,     0,
       0,   658,   660,  1028,  1100,     0,   993,   964,   979,   685,
       2,  1046,  1048,  1049,   641,     0,   236,     0,   235,   219,
     242,   482,   408,   251,   338,   252,   249,   264,   277,   275,
     271,   283,   281,   282,   280,   261,   276,   272,   273,   270,
     257,     0,     0,     0,     0,   222,   242,     3,   373,     0,
    1042,   381,   382,   383,   395,     0,     0,     0,   395,     0,
     338,   346,   342,   338,     0,   338,     0,     0,   659,   482,
     230,   233,   338,     3,   243,   409,   253,     0,     0,     0,
       0,   302,   300,   297,   301,   298,   299,   296,     3,   373,
       0,     0,  1043,     0,     0,     0,   389,     0,   398,   224,
     351,   338,   365,   338,   657,     3,   212,     0,     0,   338,
     290,   288,   285,   289,   286,   287,   284,     0,     0,   374,
       0,   401,     0,   399,     0,   401,   352,   366,     0,   214,
     213,   218,     0,   221,     0,   371,   402,     0,     0,   390,
       0,  1051,   372,     0,     0,     0,     0,   403,   404,     0,
     400,   391,     0,     0,   392,   405
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1888,  6668,  5800, -1888,    -1,   412,  1370,  -117, -1888,  -334,
   -1888,   448, -1888,  -713, -1888,   878,  -984, -1082, -1888,   300,
    5551,  2062, -1888,   469, -1888,  1497,   569,   886,   891,   706,
     898,  1457,  1464,  1456,  1466,  1455, -1888,  -174,  -171,  8758,
    1019, -1888,  1786, -1888, -1888,  -693,  7002, -1054,  7037, -1888,
     225, -1888,  1008,    44, -1888, -1888, -1888,   524,   139, -1888,
   -1679, -1690,   376,   116, -1888, -1888, -1888,   386, -1566, -1888,
   -1409, -1888, -1888, -1888, -1888,  -428, -1117, -1888,   521, -1197,
     526, -1888, -1888, -1888, -1888, -1888,   168, -1171, -1888, -1888,
   -1888,    78,   545,   548,   188, -1888, -1888, -1888, -1888,  -758,
   -1888,   114,    51, -1888,   195, -1888,   117, -1888, -1888, -1888,
    1010,  -696,  -889, -1269, -1888,   247, -1292,   134,  5848,  -846,
    -744, -1888,  -287, -1888, -1888,    27, -1888,  -162,    86,  -323,
    -242,  3850,   817,  -637,    20,    84,    88,   313,  2198, -1888,
    2201, -1888,    96,  3646, -1888,  2143, -1888,    92, -1888, -1888,
     394,   119,  4475,  2731,   -38,  1993,  -328, -1888, -1888, -1888,
   -1888, -1888,  -166,  5163,  4741, -1888,  -389,   269, -1888,  -593,
     314, -1888,   244,   842, -1888,    37,  -138, -1888, -1888, -1888,
    -356,  5429,  -907,  1297,   132,  -669,  -641,  -474,  1176, -1888,
   -1318,  -159,  -184,  1399,  1032,  4574,  -209,  -464,  -196,  -178,
    -461,  1433, -1888,  1777,   447,  1339,  1653, -1888, -1888, -1888,
   -1888,   426,  -155,  -105,  -901, -1888,   484, -1888, -1888, -1130,
     552, -1888, -1888, -1888,  2269,  -817,  -380,  -823,   -30, -1888,
   -1888, -1888, -1888, -1888, -1888,   274,  -844,  -238, -1887,  -197,
    8377,   -73,  7183, -1888,  1301, -1888,  5577,   -20,  -220,  -200,
    -198,    11,   -74,   -70,   -69,   183,    -7,    29,    32,  -181,
      68,  -152,  -150,  -146,    72,  -128,  -104,  -100,  -737,  -681,
    -668,  -660,  -695,  -116,  -648, -1888, -1888,  -728,  1502,  1503,
    1506,  1569, -1888,   666,  7428, -1888,  -596,  -584,  -580,  -542,
    -752, -1888, -1594, -1749, -1737, -1691,  -616,   -65,  -291, -1888,
   -1888,   -13,   197,   -78, -1888,  8169,  1165,  1073,  -594
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1206,   224,   405,   406,    82,    83,   407,   381,   408,
    1534,  1535,   409,  1012,  1013,  1014,  1322,  1323,  1324,  1546,
     431,   411,   412,   413,   708,   709,   414,   415,   416,   417,
     418,   419,   420,   421,   422,   423,   424,   433,  1111,   710,
    1456,   771,   218,   773,   427,   838,  1207,  1208,  1209,  1210,
    1211,  1212,  1213,  2177,  1214,  1215,  1463,  1659,  2018,  2019,
    1942,  1943,  1944,  2143,  2144,  1216,  1673,  1674,  1675,  1838,
    1839,  1217,  1218,  1219,  1220,  1221,  1222,  1865,  1869,  1480,
    1472,  1223,  1224,  1479,  1473,  1225,  1226,  1227,  1228,  1229,
    1691,  2161,  1692,  1693,  2054,  1230,  1231,  1232,  1459,  2062,
    2063,  2064,  2207,  2219,  2090,  2091,   303,   304,   909,   910,
    1178,    85,    86,    87,    88,    89,    90,   464,    92,    93,
      94,    95,    96,   232,   233,   590,   285,   466,   435,   467,
      99,   313,   101,   102,   155,   346,   347,   106,   107,   170,
     108,   930,   348,   156,   111,   256,   112,   157,   264,   350,
     351,   352,   158,   428,   117,   118,   354,   119,   581,   898,
     896,   897,  1632,   355,   356,   122,   123,  1174,  1424,  1638,
    1639,  1799,  1800,  1425,  1627,  1818,  1640,   124,   668,  1739,
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
      81,   194,   192,    81,   344,   195,   196,   358,   425,   713,
     508,   426,   133,   544,   547,   589,   669,   648,  1021,   476,
     625,   103,   376,  1001,  1254,  1258,  1476,   827,    97,   180,
     509,   956,   510,  1327,  1461,  1407,   524,   656,   529,   201,
     498,   659,   950,  1586,  1587,   537,   951,  1056,  1925,   511,
     942,   372,  2020,  1084,   943,    81,    81,   220,    81,  1091,
    1926,  1287,  1334,   308,   380,  1237,   719,  1080,   197,   658,
     133,   209,   994,   661,    81,   561,  1169,   235,   512,   103,
     513,   881,   883,    81,   514,   104,    97,    98,  1460,   105,
     153,    81,   944,   113,   508,   207,    81,   109,    58,    81,
     662,   361,   515,    81,   198,  1246,  1927,   199,   239,  1081,
    1850,   267,   596,   598,   509,   278,   510,   311,   443,   190,
     114,  1074,   444,   445,   459,  1368,   516,  -599,  -820,    58,
     517,   920,  -410,   511,  1075,    91,  1450,   209,   152,   625,
     648,    81,  1076,   104,    81,    98,    81,   105,  -411,  1662,
    1662,   113,    81,  1107,  1077,   109,   194,   506,   133,    81,
     195,   196,   512,  1281,   513,   242,    81,   103,   514,   246,
     205,  1122,  1661,  2027,    97,  1695,   885,   298,   114,  1697,
    1105,  1105,   525,   739,   740,   446,   515,   893,   262,   714,
      81,    81,   720,    91,  1651,  2094,   275,   721,   142,  1105,
    2020,   142,   207,  -410,  1929,    81,  2021,   970,   950,   166,
     516,   519,  1085,  1233,   517,   489,  1088,   480,   942,  -411,
      81,   447,   943,   197,   448,  1101,  1102,   306,   922,    81,
      81,   104,   619,    98,   260,   105,   205,  1548,   272,   113,
     194,   601,   207,   109,   195,   196,   553,  1698,   220,  1243,
      81,   151,   663,  2086,  1696,   576,   142,   664,    81,   198,
     944,  1291,   199,  1843,  2026,   597,   114,   208,    81,    81,
     207,   619,    81,  1429,  1195,  2028,  1958,  1105,   631,    81,
     240,    91,   159,   268,   565,  1419,   845,   279,   457,   637,
    1315,  -788,  1430,    81,    81,   519,    81,  2095,   648,   520,
     935,   142,  -971,   521, -1101,   879,   846,   915,   847,  -971,
     553,   884,   967,    81,    81,   663,   831,   221,  1060,  -820,
     664,  1084,   648,    81,   955,   848,   207,  1341,  1420,   648,
      81,    81,   937,   873,  2013,    81,  1710,   961,  1437,  1406,
    1019,   877,  1354,   549,   142,   552,  1410,  1106,  1106,  1408,
     811,   271,   977,  1277,   849,  1306,   850,   533,   299,  1709,
     851,   563,  1460,  1712,  2022,  1330,  1106,   476,  1237,   962,
    1925,  1519,  1326,    81,   208,   188,    81,   678,   852,  1526,
     798,   845,  1926,   520,  1355,   558,   592,   521,    98,   481,
    1474,  1074,  1432,  1433,   113,  1369,   569,   631,   109,  1746,
    2142,   846,   853,   847,  1075,   523,   854,   562,  1662,   552,
    2087,  2088,  1076,  1475,   208,   753,   149,   146,   281,   209,
     848,   114,   282,  1490,  1346,   286,  2142,   291,  1927,  1370,
    1421,  1661,   443,   262,  1699,  -789,   444,   445,  1963,  1964,
     476,    81,   208,   344,  1106,   298,   379,   501,   201,   849,
    1553,   850,  1379,   480,  2179,   851,   566,   754,   597,  1586,
    1587,  2122,  1653,  2026,    81,    81,  1939,  1940,   174,   174,
    1759,  1761,  1763,   852,  2133,   976,    81,    81,   979,   980,
    1105,   981,  1554,  1277,  2060,    81,  1436,   489,   942,  1195,
     983,   298,   943,   985,   986,   987,    58,   853,   865,   446,
    2026,   854,  1360,   616,   174,    81,   934,   617,  1434,  2085,
    1166,    58,    58,   532,   920,  1645,  1233,    81,   205,  1512,
     540,   160,  1419,  1419,  1419,    58,  1929,   443,   593,   480,
     944,   444,   445,  1399,  1646,   447,  1939,  1940,   448,  1461,
     361,   557,    81,  1370,   797,  1867,  2037,  2038,    81,  1941,
    1429,    58,   568,  1662,   174,  1400,   262,   174,    63,    64,
     293,  1470,    58,  1758,    20,  1420,  1420,  1420,  2013,  1716,
     235,  1474,   174,   648,   476,   281,   297,   921,  1868,   370,
     525,  1284,   631,   865,  2135,  1619,   866,    81,  1336,    81,
     867,   534,   604,  1460,  1475,   525,   525,   470,  1578,  1195,
      81,   912,    81,  1477,  1124,   789,    81,    77,   755,   525,
     648,   213,   756,  1261,   480,   476,    81,  1841,   133,  1968,
      81,    81,  1849,   712,   165,   481,  1478,   103,   743,   744,
    1147,  1061,    58,   174,    97,   525,  1557,   476,   476,  1045,
    1807,   425,  1082,    58,  1095,  1262,   635,  1106,   202,   956,
     179,  1115,  1110,    81,   996,    58,   476,  1645,   242,  1808,
     281,   282,   913,   652,  2034,   291,    81,  1421,  1421,  1421,
    1816,   866,   745,   746,   153,   867,  1810,  1871,   592,   262,
      98,   174,   174,   892,   275,   996,   113,  2079,   811,  1817,
     109,   104,   174,    98,  1471,   105,   227,    58,  -651,   113,
      58,   481,   697,   109,  1851,  -651,   579,   174,   939,   584,
    1868,   161,  1089,   114,   162,   163,   635,   164,  1650,   576,
      81,   612,    81,  1135,    81,   181,   114,   525,    81,   781,
    1811,    81,  1662,   782,   476,  1139,   174,   628,   563,   525,
     651,    91,   215,   174,   174,  1660,  1676,  1816,   174,   182,
     613,   614,  1148,   216,   628,   190,    81,   874,   628,  1684,
    1662,  1833,  1834,  1835,   570,   878,  1924,    19,    58,   217,
      14,    15,    16,    17,    18,   996,  1901,  1143,  1902,   582,
    1382,   525,   886,  1836,   525,  2043,   174,  2069,  1957,   174,
    2070,  1498,  1837,   894,    58,  1833,  1834,  1835,   697,  1662,
      -3,    81,    58,    81,   142,  1431,  1930,   875,  1816,   251,
      52,    53,    54,    55,    58,    81,   713,  1836,  1879,  1880,
     920,   670,    81,   270,   672,  1931,   344,  1934,   489,    58,
      58,    81,   887,    14,    15,    16,    17,    18,   736,   891,
      81,    81,    81,   895,  1280,   737,   738,   996,  1386,   722,
    1543,  1756,   525,   148,   723,   689,   628,    65,    66,    67,
      68,    69,    70,    71,    72,  1016,  2032,    81,  1331,    58,
    1844,    58,   200,    64,  1390,  1845,   281,   174,   525,   187,
     733,   734,  1510,   939,  -476,   190,   635,  1585,   190,   174,
     174,  -480,    58,  -972,  1563,  -714,   293,  1975,   525,  2036,
    -972,   733,  1976,  1502,  1503,  1017,    81,    81,   489,   712,
    1572,  2049,  1367,    58,   525,   712,   297,   261,    14,    15,
      16,    17,    18,   361,   712,   295,  1310,  1545,   283,   103,
     290,   733,   292,  1311,  1326,  1748,    97,   476,  1374,   575,
      64,   648,   955,   712,  1110,   470,   989,  2127,   298,  1576,
    1397,  2080,  2128,   635,  -481,   525,  -477,   990,   991,    81,
      74,   793,  1353,   811,  -410,   525,    14,    15,    16,    17,
      18,   261,   678,   996,   290,   292,   996,    58,   312,   297,
     774,   449,  1996,  2112,   525,  2105,  2114,   374,  1681,   262,
     906,    79,    80,  1235,   907,    98,   714,   105,  -809,   470,
     149,   113,   563,   996,  1660,   109,   332,  2194,    81,   996,
    1249,  1491,  2195,  2148,   377,    74,    81,   628,   470,  2150,
      74,   378,   907,   261,   298,    58,   262,  1626,   114,   174,
     920,  1796,  1514,   931,   933,   634,  1809,    74,   969,   635,
     634,   628,   617,    91,   635,    81,    79,   636,   489,   832,
     833,    79,    80,   834,   628,   379,   971,   634,  1248,   637,
     617,   635,   450,  1743,   252,   253,   959,   254,    79,   636,
     972,    81,   255,  -478,   973,   451,   995,    81,    81,   174,
     996,  1754,   452,    14,    15,    16,    17,    18,  2074,  1536,
     261,   479,   290,   292,    14,    15,    16,    17,    18,   453,
    1155,   376,   376,   297,   454,   449,   142,   525,    81,   449,
     425,   525,  1163,  1525,    74,   251,  1167,   620,   293,    74,
    1170,   142,   161,  1065,   261,   162,   163,   525,   164,   455,
     261,  1531,   344,  1005,   634,  1007,   297,  1010,   635,  1797,
     525,  1018,    58,   525,  1022,    79,   636,   741,   742,  1676,
      79,    80,   470,    58,  1663,   534,  1119,   858,  1558,   525,
    1120,  1426,   261,   534,   483,  1740,  1168,   525,   653,  1047,
     292,  1247,  1686,  1687,  1688,  1689,  1690,   476,   476,   497,
     604,  1157,   449,   489,   525,   996,    81,    81,    81,   484,
    1082,   499,   449,   470,   635,   425,   425,   502,  1594,  1595,
     631,  1608,   103,  1588,  1023,   503,   696,   504,  1159,    97,
     604,   489,   996,   505,   525,   522,   148,    81,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,   174,   361,
      81,   523,   103,    81,    81,   174,  1325,    81,  1497,    97,
    1326,   220,   782,  1530,    74,   267,   278,  1326,    81,   212,
    1751,  1249,  1826,  1123,  1752,  1125,  1326,   747,   748,   261,
    1422,  1833,  1834,  1835,  1635,    76,  1235,  2067,    98,   545,
     105,  1636,   546,   996,   113,    79,    80,   655,   109,   556,
    1827,    81,  1853,  1836,   996,   261,   996,   653,   292,  1854,
     567,  1855,  1842,  1120,    81,   996,  1235,   586,    98,  1248,
     105,   114,   696,  1913,   113,  1024,  1025,  1026,   109,   724,
     489,   725,   726,   727,   608,   212,    91,   623,    81,  1177,
     174,   174,   262,   202,   716,  1935,   664,   275,  2092,   782,
     628,   114,  1981,   651,  2029,   261,   996,  2131,   996,   671,
     728,  1326,   344,   729,   730,   682,    91,  2132,   731,   732,
      81,   996,   623,   716,  2216,   441,  2092,   563,  2213,  2222,
     261,   683,   142,  2223,   686,   261,  2163,   261,   260,   272,
    2167,   687,  1279,   688,  1833,  1834,  1835,   998,   999,   142,
    1097,  1098,   470,  1888,  1099,  1100,   996,   692,   261,   716,
     261,   261,   750,  1409,   735,  2145,  1836,   749,  1426,  1426,
    1426,   757,  1642,  1628,  1426,  -186,  1435,   456,   261,   142,
    1313,  1120,  1247,  1663,   751,   508,   752,   268,   279,   783,
     261,    81,   784,  1454,   554,    81,  1328,  1329,    81,    -3,
     712,   785,   142,   996,  1332,   509,   786,   510,   815,   361,
    1411,  1412,  1413,   261,   787,   653,   292,   788,   489,  1031,
    1032,  1033,  1034,  1300,   511,  1793,  1794,  1795,  1304,  -157,
    -157,   103,   103,   829,  1824,  1470,  1471,   261,   653,  1312,
    -479,   489,   489,   -18,   261,  -122,  -122,  -122,  -122,  -122,
    -122,    81,   271,   512,  1467,   513,  1099,  1489,   554,   514,
    1643,  1551,  1552,   565,  1644,  1556,  1552,  1422,  1422,  1422,
     153,  1624,  1625,  1629,  1819,  1819,  1819,   515,   633,   828,
     148,   839,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,   855,   489,   869,  1665,  1665,    98,    98,   105,
     105,   516,   856,   113,   113,   517,   870,   109,   109,  1560,
    1552,   489,   857,  1536,  1071,  1544,    81,   859,   152,  1596,
    1544,    81,    81,    81,  1071,  1610,   344,  1764,  1120,  1588,
     114,   114,  1812,  -121,  -121,  -121,  -121,  -121,  -121,   860,
     563,  1899,  1120,  1900,  1552,    91,    91,   673,  1910,  1911,
     861,  1862,   862,  1468,  1922,   996,   490,  1979,  1980,  2000,
    1552,   863,   845,   476,   476,   864,   174,   212,   305,   174,
     174,   174,  2001,  1552,  1642,  1939,  1940,  2213,  2214,  1642,
     871,   142,   846,   908,   847,  1249,   562,   519,   889,  1588,
    1549,  1550,   923,    81,   174,  1027,  1028,   633,    81,   890,
     174,   848,  1029,  1030,    81,  -597,    81,  -595,   142,   142,
    1989,  2134,  2136,   584,    81,  1035,  1036,  1711,  1713,  2055,
     174,   689,   674,   361,  1741,  1742,   489,  1652,  1654,   628,
     849,   151,   850,  1248,   899,   566,   851,   925,   675,   489,
     676,   677,    65,    66,    67,    68,    69,    70,    71,    72,
    1820,  1821,  1499,  1500,   852,   929,   945,   947,   470,   637,
     964,   968,  1643,   974,   174,   975,  1644,  1643,   997,  1000,
    1003,  1644,  1044,  1070,  1071,   520,  1714,  1532,   853,   521,
     441,   441,   854,  1049,   489,  1078,  1117,  1126,   261,  1171,
     103,  1127,  1128,  1149,  1129,  1130,   142,  1131,   648,  1132,
    2055,   261,  1133,  1134,   733,  1158,  1160,  -792,  1164,   965,
    1172,  1238,  -690,  1173,  1974,    14,    15,    16,    17,    18,
     993,  1239,   262,  1255,  1271,   261,  1272,   275,  1273,    81,
    1283,    81,  1289,  1286,  1292,   425,   261,  1284,  2017,  1293,
    1537,  1538,  1539,  1288,  1295,   261,  1247,  1540,  1541,  1296,
    1297,  1298,  1299,  1301,  1665,  1302,    98,  1303,   105,  1308,
    1309,  1316,   113,  1333,  1317,   865,   109,  1338,   260,   272,
      14,    15,    16,    17,    18,  1305,  1339,  1249,  1340,  1347,
     536,    81,   957,  1348,    81,  1349,  1350,  2121,  -678,   114,
    1358,   476,  -677,  1373,  1381,   489,  -793,  1398,  1403,   489,
    1427,  1438,  1428,  1441,    91,  1442,   441,  1451,   174,  1452,
    1453,   174,  1458,   489,  1462,  -713,  1588,   996,  1656,    19,
    1483,  1486,  1487,   489,  1464,  1248,   825,  1485,   490,  2061,
    1528,  1493,   142,  1542,  1544,   103,   489,  1495,   489,   489,
     489,   489,   261,  1559,  1571,  1584,    81,    81,  1589,  1590,
    1591,  1592,  1597,   866,   174,   174,  1552,   867,    48,    49,
      50,    51,    52,    53,    54,    55,   261,   142,   470,   508,
    1114,   183,     6,     7,     8,     9,    10,    11,    12,    13,
    1600,   425,   271,   425,  2140,  1856,  2017,  1642,   142,   509,
     563,   510,  1615,  1616,  1617,  1620,  1633,  1631,  1634,  1665,
    1431,    98,  1677,   105,  1678,  1680,    81,   113,   511,  1682,
    1694,   109,   489,  1471,  1700,  1702,   489,   441,  1703,  1704,
    1705,   489,   425,  1706,  1195,  2165,  1717,  1719,    58,  2057,
    1720,  1722,   280,  1723,   114,  1724,   562,   512,  1247,   513,
    1725,  1726,  1727,   514,  1728,  1729,   489,  1731,  1736,    91,
    1745,  1749,   905,  1750,  1753,  1765,  2189,  1757,  1791,  1766,
     148,   515,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,  1770,  1771,   780,  1643,   449,  2061,  1781,  1644,
    1779,  2061,  2061,  1596,  1792,   516,  1806,  1636,   221,   517,
     791,  1828,  1830,   794,  1859,  1861,   489,  1881,  1885,  1887,
     489,  1886,  1891,  1893,   425,  1889,  1898,  2215,  1351,    76,
    2057,  1904,   142,   489,   103,   194,   601,  2192,  1905,   195,
     196,   261,  1156,  1908,    81,  1909,    81,   174,  1919,  1915,
    1920,  1921,  1947,    84,  1952,  1953,   150,  1965,  1967,   174,
    1971,  1983,   103,  1973,  2206,  1977,  1978,  1993,  2206,  1997,
     536,  1994,   174,  1995,  1998,  1999,   261,   525,  2031,  -679,
     441,  2007,   261,   489,  2008,  2009,  2217,  2010,  2011,  2012,
     519,  -580,  2039,  2044,  2033,  2071,  2042,  2050,  1665,  2058,
      98,   103,   105,  2072,  2073,  2076,   113,  1270,    81,    81,
     109,    84,  2077,  2078,  2059,  1911,  2089,   174,  2125,   489,
    2098,   207,   489,  2111,   489,  2115,  1665,   191,    98,  2113,
     105,   489,   865,   114,   113,  2124,    84,  2126,   109,  2129,
    2130,  2138,    14,    15,    16,    17,    18,  2151,    91,   231,
    2147,    81,   259,  2164,  2149,  2160,    84,  2166,  2171,  2173,
     489,   114,   489,   480,  1864,  1665,   489,    98,   489,   105,
    2174,  2190,  2191,   113,  2180,  2201,    91,   109,   520,  2193,
    2203,  2204,   521,  2208,  2209,  2221,  2211,  2220,  2224,   490,
    1895,   489,   825,   150,  1555,   992,  1037,  1039,  1041,    84,
     114,    58,   150,    81,  1038,   315,   323,   772,  1040,  1457,
    1466,   142,    81,  2202,  1863,    91,  2141,  1337,   343,  1969,
     866,  1962,  2158,  1872,   867,  1870,  1857,  2188,   174,  1858,
     261,  2117,   174,  2168,  1344,  1345,  2210,  2116,  1484,   142,
     171,  2015,   432,   191,   191,   288,   174,   555,  2084,  1630,
     189,  1282,  1116,  1481,   150,   462,   174,  1257,   259,     3,
     927,    74,   835,  1876,  1290,   905,  1052,  1053,  1790,   174,
    1054,   174,   174,   174,   174,     0,     0,     0,   142,   174,
       0,   774,   231,   231,     0,   525,     0,     0,   263,   261,
       0,     0,    79,    80,     0,     0,     0,     0,     0,   284,
     287,     0,     0,   315,    14,    15,    16,    17,    18,   148,
       0,    84,     0,    65,    66,    67,    68,    69,    70,    71,
      72,  1501,     0,     0,     0,   259,     0,     0,     0,     0,
     905,     0,     0,   780,   780,     0,     0,     0,     0,     0,
       0,     0,   263,  1063,    58,   174,  1066,     0,     0,   174,
    1527,     0,     0,     0,   174,     0,   628,     0,    76,   323,
       0,   824,     0,    58,     0,   323,   315,   315,     0,  1321,
       0,     0,     0,     0,     0,   150,   148,  1321,     0,   174,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,   263,   343,   638,   647,     0,  1561,
       0,     0,     0,     0,    74,  2205,  1321,   536,     0,   490,
     441,     0,   343,     0,  1137,     0,   343,     0,  1141,  2212,
     957,     0,  1145,    74,    75,    76,     0,     0,     0,   174,
       0,     0,     0,   174,     0,    79,    80,   628,  2066,     0,
     905,     0,     0,  1797,     0,     0,   174,   525,     0,     0,
     432,     0,     0,     0,    79,    80,     0,   905,   905,  2123,
       0,   263,     0,     0,     0,     0,     0,     0,     0,  1321,
     148,   261,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   432,     0,     0,   775,     0,     0,
       0,     0,     0,     0,   191,   263,   174,     0,     0,     0,
       0,   263,     0,     0,     0,     0,     0,     0,  1567,  1568,
     150,     0,     0,     0,   462,     0,     0,     0,   804,     0,
     647,     0,  1582,  1583,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   263,     0,   174,     0,   174,     0,     0,
       0,     0,     0,  1443,   174,   148,     0,   172,   173,    65,
      66,    67,    68,    69,    70,    71,    72,     0,   231,     0,
    1605,  1606,  1607,     0,     0,     0,     0,     0,     0,     0,
     231,     0,     0,   174,     0,   174,     0,   698,     0,   174,
       0,   174,  1738,     0,     0,     0,     0,     0,     0,  1744,
       0,     0,     0,     0,     0,   315,     0,   432,   432,     0,
       0,   315,     0,   343,   174,     0,  1755,     0,     0,     0,
       0,     0,     0,     0,   780,     0,  2218,     0,  1445,     0,
       0,     0,   261,     0,   148,  2225,   172,   173,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,   261,
       0,   148,   315,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,   315,     0,   315,   263,   343,   148,    84,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,   490,     0,   698,     0,   343,   462,     0,   647,  1321,
       0,     0,     0,  1447,     0,  1384,   638,     0,  1388,     0,
     638,     0,  1392,     0,     0,     0,   148,   610,     0,   343,
      65,    66,    67,    68,    69,    70,    71,    72,  1008,   647,
       0,     0,   343,     0,   932,     0,     0,     0,     0,     0,
       0,     0,   116,   150,     0,   116,     0,     0,     0,     0,
       0,   905,   905,     0,     0,   261,   432,     0,   263,   150,
     150,     0,   432,     0,  1852,   905,   905,     0,  1009,     0,
       0,   432,     0,     0,   150,   150,   150,     0,     0,   263,
       0,     0,     0,     0,     0,     0,     0,   148,  1783,  1784,
    1785,    65,    66,    67,    68,    69,    70,    71,    72,   263,
     116,     0,     0,   905,   905,   905,   441,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,   116,  1890,     0,     0,   490,
     462,     0,     0,     0,   263,    74,     0,  1897,     0,     0,
       0,   265,     0,     0,     0,   116,   775,   775,     0,     0,
       0,     0,     0,     0,   432,   802,    76,     0,   263,   635,
       0,     0,     0,     0,     0,   263,    79,   803,     0,     0,
       0,   462,     0,     0,   804,     0,   804,     0,     0,   637,
       0,     0,   116,   261,     0,     0,     0,     0,   116,     0,
       0,   116,     0,   343,   343,   265,     0,     0,     0,     0,
       0,     0,     0,     0,   490,     0,   339,   116,   371,     0,
       0,     0,   343,     0,   315,     0,     0,  1565,  1882,     0,
       0,     0,   490,     0,     0,     0,  1574,  1321,     0,     0,
       0,   436,  1321,  1321,  1321,     0,     0,     0,     0,     0,
     315,     0,     0,   116,   436,     0,     0,   265,     0,     0,
       0,     0,     0,  1903,     0,     0,     0,     0,  1906,  1907,
       0,     0,  1990,  1991,   183,     6,     7,     8,     9,    10,
      11,    12,    13,     0,    14,    15,    16,    17,    18,     0,
       0,   432,     0,     0,     0,     0,     0,     0,     0,   261,
     343,     0,     0,     0,     0,     0,   150,   432,   116,     0,
     116,     0,     0,     0,     0,     0,     0,     0,   343,     0,
    1265,   905,   905,   905,   265,     0,     0,     0,     0,     0,
       0,   638,   759,   760,   761,   762,   763,   764,   765,   766,
     767,   768,   769,    58,     0,   580,   215,     0,     0,     0,
       0,     0,     0,   116,     0,     0,     0,     0,   265,     0,
       0,     0,     0,     0,   265,     0,     0,  1825,     0,     0,
     462,     0,     0,   770,   116,   148,     0,   228,   229,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,  2075,
    1152,     0,     0,     0,   116,     0,   265,   116,     0,     0,
       0,     0,     0,    74,    58,     0,     0,     0,     0,     0,
       0,   116,     0,     0,     0,   116,     0,  2016,     0,   263,
       0,     0,     0,   802,    76,     0,     0,   635,     0,     0,
       0,     0,   263,     0,    79,   803,   148,   775,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   436,
    1321,   905,  1321,     0,   804,     0,   263,     0,     0,     0,
       0,   804,     0,     0,    74,   382,     0,  2137,   383,     0,
     384,   148,   385,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,   436,  2119,    76,   905,     0,   525,   386,
       0,   905,   905,     0,     0,    79,    80,     0,     0,     0,
       0,     0,     0,   343,     0,     0,     0,     0,     0,   116,
       0,     0,     0,   436,  1801,     0,     0,   387,   388,   265,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,   150,    74,     0,
       0,     0,     0,     0,     0,     0,   150,     0,     0,     0,
       0,     0,     0,     0,   432,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,   432,     0,     0,
       0,     0,     0,     0,   432,     0,   436,   436,     0,     0,
       0,   265,   116,   148,     0,   200,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   259,    84,   327,   328,
     329,   330,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   315,     0,     0,   116,  2175,     0,   150,     0,   116,
       0,     0,   265,   116,     0,   116,     0,   462,     0,     0,
       0,     0,    76,     0,     0,   824,   116,   148,   116,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   371,     0,   116,   436,   462,   265,     0,     0,
     150,    14,    15,    16,    17,    18,  1801,  1801,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,     0,
       0,   265,     0,     0,     0,   580,     0,     0,   265,   331,
       0,   116,     0,   963,     0,     0,     0,     0,     0,     0,
       0,     0,   116,     0,     0,     0,     0,   332,     0,     0,
       0,     0,     0,     0,     0,   436,     0,     0,   116,   116,
      58,   436,     0,   343,   343,     0,     0,     0,     0,     0,
     436,     0,   263,   116,   116,   116,   148,     0,   367,   368,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,   263,     0,     0,
       0,     0,     0,   150,   150,   150,   150,     0,   150,   150,
      74,     0,    58,     0,  1637,   323,     0,    77,     0,   436,
       0,     0,   369,     0,     0,     0,     0,  1801,     0,     0,
     230,    76,     0,   432,   432,     0,     0,     0,     0,     0,
       0,    79,    80,   436,   148,     0,   228,   229,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
     436,     0,     0,     0,     0,     0,     0,     0,   905,     0,
       0,     0,    74,     0,   259,     0,     0,     0,     0,     0,
     148,     0,   116,   116,    65,    66,    67,    68,    69,    70,
      71,    72,   230,    76,     0,     0,   462,     0,     0,     0,
       0,   116,     0,    79,    80,     0,     0,     0,    74,  2082,
       0,     0,     0,  1801,     0,     0,     0,     0,     0,     0,
       0,   150,     0,   638,     0,     0,     0,     0,  1072,    76,
       0,  1613,   635,  1153,     0,     0,     0,   116,     0,    79,
      80,     0,     0,     0,     0,    14,    15,    16,    17,    18,
       0,   263,     0,     0,  1801,     0,     0,     0,     0,     0,
       0,     0,   265,     0,     0,     0,     0,     0,     0,     0,
     436,     0,     0,     0,     0,   265,     0,   110,     0,   116,
       0,     0,     0,   148,     0,   116,   436,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,   116,     0,  1267,
     436,     0,   116,     0,    58,     0,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,  1637,  1798,  1801,  1801,
       0,  1637,     0,   432,     0,     0,     0,  1637,     0,  1637,
       0,  1351,    76,     0,     0,   110,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   436,
       0,     0,     0,     0,     0,     0,   323,   150,     0,     0,
       0,  1801,     0,     0,    74,     0,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     274,     0,     0,     0,  2119,    76,     0,     0,   525,     0,
       0,     0,   432,     0,     0,    79,    80,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
     343,     0,   116,   150,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   110,     0,    58,     0,     0,     0,   116,
     116,     0,    14,    15,    16,    17,    18,     0,     0,     0,
       0,     0,   349,     0,   150,     0,     0,     0,     0,     0,
       0,     0,     0,  1278,     0,     0,     0,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,   343,
     343,     0,     0,     0,     0,     0,     0,     0,     0,   469,
       0,   100,   116,     0,   154,    74,     0,     0,     0,  1798,
    1798,    58,   263,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1637,   314,    76,  1637,     0,     0,
       0,     0,     0,     0,     0,     0,    79,    80,     0,     0,
       0,     0,     0,   148,   323,     0,   116,    65,    66,    67,
      68,    69,    70,    71,    72,   116,     0,   432,     0,   100,
       0,     0,     0,   436,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,   572,     0,
       0,     0,     0,     0,   206,     0,   436,     0,     0,     0,
     315,    75,    76,   436,     0,     0,     0,     0,   110,     0,
       0,     0,    79,    80,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   265,   116,     0,     0,     0,
       0,     0,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   116,     0,     0,   627,
    1798,   307,   274,     0,     0,     0,   436,   100,     0,  1637,
    1267,    74,     0,     0,     0,     0,   627,     0,     0,     0,
     627,     0,  1522,     0,     0,     0,   345,     0,     0,     0,
       0,  1635,    76,     0,   116,   436,     0,     0,  1636,   116,
     263,     0,    79,    80,   150,     0,     0,     0,     0,     0,
       0,   442,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,   307,   468,    65,    66,    67,    68,    69,    70,
      71,    72,  1318,     0,     0,     0,  1319,   343,  1320,     0,
       0,     0,     0,   116,   116,     0,  1798,     0,     0,     0,
       0,   518,     0,     0,     0,     0,   150,   116,   116,     0,
       0,     0,   116,   116,     0,     0,     0,     0,     0,    76,
       0,   543,  1547,     0,     0,     0,   548,   550,   627,   206,
       0,     0,     0,     0,   150,   150,     0,  2120,   323,     0,
       0,     0,     0,     0,     0,   116,   116,   116,     0,     0,
       0,     0,   571,     0,  1614,     0,   573,     0,     0,     0,
       0,   574,   116,   116,   116,   116,   116,   116,   116,     0,
       0,     0,   591,   150,   265,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   603,    58,     0,     0,     0,
       0,     0,   436,   436,     0,     0,     0,     0,     0,     0,
       0,  2120,  2120,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,   626,     0,     0,   650,   469,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     657,     0,     0,   265,   657,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2120,     0,    74,     0,     0,   349,
       0,     0,     0,     0,     0,   436,     0,     0,   274,     0,
     110,     0,     0,     0,     0,    58,   314,    76,     0,     0,
       0,   469,     0,   110,   263,     0,     0,    79,    80,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,   627,
     469,     0,     0,     0,     0,     0,     0,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   148,   627,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,     0,    74,   627,    58,   307,     0,
       0,   388,   626,   389,   390,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,  1635,    76,     0,     0,     0,
       0,     0,     0,   116,   116,   116,    79,    80,     0,   148,
     479,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   718,   436,     0,    77,   399,   148,    74,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   116,
       0,     0,     0,     0,     0,     0,     0,  1635,    76,     0,
       0,     0,     0,     0,    74,   265,   116,     0,    79,    80,
       0,   468,     0,     0,   469,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2119,    76,     0,     0,   525,     0,
       0,     0,     0,     0,     0,    79,    80,     0,     0,     0,
       0,   436,     0,   901,     0,     0,     0,     0,   550,     0,
       0,     0,   914,     0,   591,   469,     0,     0,     0,   116,
       0,     0,   116,     0,     0,   345,     0,   100,     0,     0,
       0,     0,     0,   116,     0,     0,     0,   349,   349,     0,
       0,     0,     0,   657,   938,     0,   115,     0,     0,     0,
       0,     0,     0,   116,     0,     0,   349,     0,   949,     0,
       0,     0,     0,     0,     0,     0,     0,   626,   116,     0,
       0,     0,   958,   116,   116,     0,     0,     0,   116,   116,
     657,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,   349,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,     0,   115,     0,     0,   148,     0,   575,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    74,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,     0,     0,   265,   349,     0,     0,     0,     0,   230,
      76,     0,     0,     0,     0,     0,   436,     0,     0,   276,
      79,    80,   627,     0,     0,   274,     0,   349,   148,  1046,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   115,     0,     0,  1055,     0,     0,     0,     0,
     175,   178,     0,     0,   469,     0,   314,    76,     0,     0,
       0,   353,     0,     0,     0,     0,     0,    79,    80,   938,
       0,     0,   148,     0,  1079,     0,    65,    66,    67,    68,
      69,    70,    71,    72,  1318,     0,   223,     0,  1319,     0,
    1320,   468,   468,     0,     0,   148,     0,     0,   471,    65,
      66,    67,    68,    69,    70,    71,    72,  1318,     0,     0,
     468,  1319,     0,  1320,     0,     0,     0,   349,     0,     0,
       0,    76,     0,   116,  1760,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   349,   349,   309,     0,     0,   310,
       0,     0,     0,     0,    76,     0,   901,  1762,     0,     0,
       0,     0,     0,     0,   333,     0,   116,     0,     0,     0,
       0,     0,   121,     0,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,  1234,
       0,     0,     0,     0,     0,     0,     0,   349,   468,     0,
       0,     0,     0,     0,   154,     0,     0,   115,     0,     0,
       0,     0,     0,   116,   116,     0,   657,   265,     0,  1269,
       0,   901,     0,     0,     0,   500,  1275,     0,     0,   148,
     121,   577,   578,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,     0,     0,     0,   629,     0,
       0,   276,   116,     0,     0,   121,     0,     0,   110,     0,
       0,     0,     0,     0,     0,   629,     0,     0,   345,   629,
       0,     0,     0,   559,   560,   121,     0,     0,     0,     0,
      77,     0,     0,   289,   175,     0,     0,   148,   110,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   175,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   274,   121,     0,     0,     0,     0,     0,   121,     0,
       0,   121,     0,     0,     0,     0,     0,     0,   606,     0,
       0,   901,     0,     0,     0,   609,   611,     0,    77,     0,
     618,   627,     0,     0,     0,     0,   148,     0,   901,   901,
      65,    66,    67,    68,    69,    70,    71,    72,  1318,     0,
       0,   121,  1319,     0,  1320,     0,     0,   629,     0,   349,
     469,     0,     0,   121,     0,     0,     0,     0,   333,     0,
       0,   333,     0,   148,     0,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,    76,     0,     0,     0,     0,
       0,   468,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   349,   349,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
     121,   483,   349,   349,     0,   121,     0,   349,   349,     0,
       0,     0,     0,     0,     0,   154,     0,     0,     0,     0,
       0,     0,     0,     0,  1423,     0,   471,     0,     0,     0,
       0,     0,  1234,     0,     0,     0,     0,     0,     0,   223,
     349,   349,   349,   121,     0,     0,     0,     0,     0,     0,
       0,   819,   820,     0,     0,     0,     0,     0,   353,     0,
       0,     0,  1234,     0,   121,     0,     0,   276,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,   115,     0,     0,  1482,     0,   110,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   629,   471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   626,     0,     0,     0,     0,
       0,     0,   629,     0,   548,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   629,     0,     0,     0,   121,
       0,     0,     0,   901,   345,     0,     0,     0,     0,     0,
     469,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,   120,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   333,   901,   901,     0,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,   901,   901,     0,     0,
       0,   468,   468,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   471,     0,     0,     0,     0,     0,     0,
       0,   966,     0,     0,   901,   901,   901,   120,   349,   349,
     349,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1423,  1423,  1423,   154,   550,     0,   120,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,   121,     0,     0,
       0,  1664,  1664,     0,   349,     0,   353,   353,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
     120,   274,     0,   120,     0,   353,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,     0,     0,   121,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,   121,     0,
       0,   353,     0,   120,   345,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   349,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   349,   154,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
    1096,     0,     0,   353,     0,     0,     0,  1108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   629,   121,   349,   276,     0,   353,     0,   349,   349,
     120,     0,   120,   349,   349,   121,     0,   120,   121,   121,
     125,   121,     0,   125,     0,     0,     0,     0,     0,     0,
     121,     0,     0,   121,   121,   121,     0,     0,     0,     0,
       0,     0,   901,   901,   901,     0,     0,     0,     0,     0,
       0,     0,     0,   471,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1179,     0,     0,     0,   120,  1815,   125,     0,
       0,   110,     0,     0,     0,     0,     0,     0,   901,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     0,  1832,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,   353,     0,     0,     0,
       0,     0,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   353,   353,     0,     0,     0,     0,     0,
    1664,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,   345,     0,
     125,   154,     0,     0,     0,     0,   125,     0,     0,   125,
       0,     0,   901,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,   353,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   901,   627,   125,
       0,   120,   901,   901,     0,     0,     0,   468,   468,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1928,     0,
     121,   349,   193,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,   121,   121,     0,     0,     0,
     110,     0,     0,     0,   234,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,   115,   125,     0,
       0,     0,     0,   125,     0,  1664,     0,     0,   110,   627,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   120,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     317,   125,     0,   410,     0,     0,     0,   110,     0,     0,
     629,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,   125,     0,     0,   120,     0,   120,     0,     0,
       0,  1444,  1446,  1448,     0,     0,     0,     0,   353,   471,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   349,     0,     0,     0,     0,
       0,     0,  1469,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   507,   234,     0,
       0,     0,  1179,     0,     0,     0,     0,   353,   353,     0,
       0,     0,  2056,     0,     0,     0,     0,   125,   317,     0,
       0,   353,   353,     0,   120,     0,   353,   353,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
     120,   120,     0,   120,     0,   468,  1523,     0,     0,     0,
       0,   125,   120,     0,     0,   120,   120,   120,     0,   353,
     353,   353,     0,     0,  1664,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,   214,     0,
       0,   602,   317,     0,   225,   226,     0,     0,     0,     0,
       0,     0,  1664,  2056,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   115,   115,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,   296,     0,
       0,     0,   204,   685,     0,     0,     0,   410,   691,     0,
       0,  1664,     0,     0,     0,   120,   121,   700,   701,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   410,     0,     0,     0,     0,     0,     0,
    2162,     0,     0,     0,   125,   125,   121,     0,     0,   471,
       0,     0,     0,   410,     0,     0,     0,     0,     0,   901,
       0,     0,     0,     0,     0,     0,   121,     0,   204,     0,
    1647,     0,     0,  1649,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   204,     0,     0,   125,     0,     0,
       0,   125,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,     0,   204,   121,
       0,     0,     0,   805,     0,     0,     0,     0,     0,     0,
       0,   465,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,   353,   353,   353,
       0,     0,     0,   844,     0,     0,     0,   120,   120,     0,
       0,     0,     0,     0,     0,   234,     0,     0,     0,     0,
     125,     0,     0,     0,     0,     0,     0,   204,     0,     0,
       0,   599,     0,   125,     0,     0,   125,   125,     0,   125,
     317,     0,     0,   353,     0,     0,   317,     0,   125,     0,
       0,   125,   125,   125,     0,     0,     0,     0,     0,     0,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,   121,   121,   121,   121,   121,   121,     0,
       0,     0,     0,     0,     0,     0,     0,   317,     0,     0,
       0,   204,     0,     0,     0,   115,     0,     0,   919,     0,
     317,     0,   121,   121,     0,     0,     0,     0,     0,     0,
       0,   204,     0,   353,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   353,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,  1822,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,     0,   353,   353,     0,
       0,     0,   353,   353,     0,     0,     0,     0,     0,     0,
       0,   800,     0,   801,     0,     0,     0,     0,     0,     0,
     121,     0,   817,   818,     0,     0,     0,     0,     0,     0,
     204,     0,     0,     0,     0,     0,   410,   410,   410,   410,
     410,   410,   410,   410,   410,   410,   410,   410,   410,   410,
     410,   410,   410,   410,   410,     0,     0,     0,     0,     0,
     204,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   125,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,   125,   125,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,     0,   120,     0,
       0,     0,   121,     0,     0,   120,     0,     0,     0,  1073,
       0,   805,     0,     0,     0,     0,     0,     0,     0,   911,
       0,     0,     0,   204,   204,     0,     0,     0,   120,   465,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   317,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,   317,     0,   629,     0,     0,
       0,  1988,     0,   204,     0,     0,     0,     0,     0,     0,
       0,   120,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   465,     0,     0,     0,     0,     0,     0,     0,
     353,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,     0,   204,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   204,
       0,     0,     0,     0,     0,     0,     0,   115,   629,     0,
       0,     0,     0,     0,     0,   410,     0,     0,     0,     0,
     410,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,   120,   120,   120,   120,   120,   120,
     120,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,   410,   125,     0,     0,     0,     0,     0,
       0,     0,     0,   125,   120,   120,   465,     0,     0,     0,
       0,   125,     0,     0,   353,  1094,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     204,     0,     0,     0,   125,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,   465,     0,  1073,
       0,     0,     0,     0,     0,  1352,   805,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,     1,   465,
     465,   147,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   465,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1175,  1176,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1240,  1241,  1242,     0,     0,
    1244,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,     0,     0,   410,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   203,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   465,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,   120,     0,     0,     0,     0,     0,
       0,     0,  1314,     0,     0,   302,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   317,     0,   120,     0,
       0,     0,   121,     0,     0,   410,   204,     0,     0,     0,
     125,   125,   125,   125,   125,   125,   125,     0,  1335,     0,
       0,     0,   410,   410,   410,     0,     0,     0,     0,   410,
     410,     0,     0,   120,     0,     0,     0,     0,     0,     0,
     125,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1359,     0,     0,     0,     0,
       0,     0,     0,     0,  1363,  1364,  1365,  1366,   302,     0,
       0,     0,  1371,  1372,     0,   120,     0,     0,     0,     0,
     410,   410,  1380,     0,     0,     0,   551,     0,  1579,     0,
       0,     0,     0,     0,     0,     0,   302,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   302,     0,     0,
       0,  1401,     0,     0,  1404,     0,  1405,     0,   125,     0,
       0,     0,     0,     0,     0,   583,   587,     0,     0,   465,
       0,     0,   594,   595,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     605,     0,     0,     0,     0,     0,     0,     0,     0,  1641,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
     624,     0,  1465,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1488,     0,     0,     0,     0,     0,     0,
    1492,     0,  1494,  1496,     0,     0,     0,     0,     0,     0,
     125,     0,     0,     0,  1505,     0,  1506,   717,  1507,     0,
    1509,     0,     0,     0,     0,  1517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   758,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   796,     0,     0,     0,   799,   125,
       0,     0,     0,     0,     0,     0,     0,  1562,     0,     0,
       0,     0,   204,     0,  1569,  1570,     0,   821,     0,     0,
     125,   822,   823,     0,     0,   826,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1593,     0,
     840,   841,   842,   843,     0,  1598,     0,     0,     0,  1599,
       0,   125,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,   872,     0,     0,     0,     0,     0,     0,
       0,  1641,   876,     0,     0,     0,  1641,  1618,     0,   465,
     465,     0,  1813,     0,  1641,   120,     0,     0,     0,     0,
       0,   225,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   169,     0,     0,   302,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,   410,     0,   169,     0,
       0,     0,     0,  1701,     0,   918,     0,     0,     0,     0,
       0,     0,   583,     0,   125,     0,     0,     0,   924,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   941,   946,   169,     0,     0,     0,  1730,     0,
       0,     0,     0,     0,     0,     0,  1735,   169,  1737,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,   204,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1768,  1769,     0,
       0,     0,     0,   988,     0,   373,     0,     0,     0,     0,
       0,     0,  1774,  1775,     0,  1776,     0,     0,     0,  1936,
       0,     0,  1641,     0,  1780,     0,     0,     0,     0,     0,
     711,     0,     0,     0,     0,  1786,  1787,  1788,     0,     0,
       0,     0,     0,   169,     0,     0,     0,   169,     0,     0,
     169,   169,     0,     0,   169,     0,     0,   169,   169,     0,
     169,     0,   169,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1051,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     0,   317,     0,  1068,     0,     0,
       0,  1069,     0,     0,     0,     0,     0,     0,     0,     0,
     941,     0,     0,     0,     0,     0,     0,     0,     0,   204,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
     410,     0,  1109,     0,     0,     0,     0,   169,     0,     0,
     169,  1118,     0,     0,     0,     0,     0,  1121,     0,     0,
       0,     0,     0,     0,  1641,     0,     0,     0,     0,     0,
     125,     0,     0,   169,   169,     0,     0,     0,     0,     0,
     410,     0,     0,  1883,  1884,     0,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,  1892,   880,   882,     0,
       0,     0,     0,     1,     0,     0,     0,  1165,     0,     0,
       0,     1,     0,     0,     0,     0,   204,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1916,  1917,  1918,     0,     0,     0,     0,
       1,     0,     0,     0,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   465,   465,     0,     0,     0,
     458,   360,   317,     0,   169,     0,   410,     0,   410,     0,
       0,     0,     0,     0,     0,     0,  1294,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   528,     0,     0,     0,     0,     0,     0,   528,
       0,     0,     0,     0,     0,     0,     0,   410,     0,     0,
       0,     0,  1992,     0,     0,     0,   711,     0,     0,   373,
       0,     0,   711,     0,     0,     0,   602,   317,     0,     0,
       0,   711,     0,  2002,     0,   169,  2003,  2004,     0,     0,
       0,   410,     0,  2006,     0,     0,     0,     0,     0,     0,
     711,     0,     0,     0,     0,     0,     0,     0,     0,  1342,
       0,     0,     0,  1343,     0,     0,     0,     0,     0,   317,
     941,     0,     0,     0,     0,   528,     0,     0,     0,     0,
    1356,     0,     0,     0,     0,     0,  1043,  1357,     0,   410,
       0,     0,     0,     0,     0,     0,  1361,     0,  1362,     0,
       0,   360,   639,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,   660,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1394,     0,     0,     0,  1395,     0,     0,
       0,  1396,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   169,   169,     0,     0,   147,
       0,     0,     0,     0,     1,     0,     0,     0,   169,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   465,  2118,     0,     0,     0,     0,     0,
       0,     0,     0,   528,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   528,
     792,     0,   528,   795,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,   639,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2159,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1504,     0,     0,     0,   528,     0,     0,     0,   528,
       0,     0,     0,  2178,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1529,     0,  2187,     0,
       0,   169,   169,     0,     0,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,  2198,     0,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,   169,   169,     0,   169,     0,   169,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     528,     0,     0,   360,     0,     0,     0,     0,   169,     0,
       0,     0,   169,     0,     0,     0,   169,     0,     0,     0,
    1602,   936,   360,     0,  1603,     0,     0,     0,  1604,     0,
       0,     0,   639,     0,     0,     0,   639,     0,     0,     0,
       0,     0,     0,   954,     0,   360,   382,     0,     0,   383,
       0,   384,     0,   385,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1648,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   169,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,   388,
     169,   389,   390,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   391,   392,   379,     0,   393,   394,   395,
       0,   396,   397,     0,     0,     0,     0,  1718,     0,    74,
    1721,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1666,  1667,  1668,     0,     0,  1732,   398,
    1847,     0,    77,   399,     0,     0,   360,     0,     0,   400,
      79,    80,   401,   402,   403,   404,     0,     0,     0,     0,
       0,     0,   528,   528,     0,     0,     0,   711,     0,     0,
       0,     0,   528,  1064,     0,   528,  1067,     0,     0,     0,
       0,  1440,     0,     0,     0,     0,     0,   360,     0,     0,
     639,  1767,   639,   639,     0,     0,     0,     0,     0,   639,
    1772,     0,     0,     0,  1773,     0,     0,     0,     0,   360,
     360,     0,     0,   211,     0,     0,     0,     0,  1777,  1778,
       0,   169,     0,     0,     0,     0,     0,     0,   360,   269,
       0,     0,   528,     0,     0,     0,   528,     0,     0,     0,
       0,     0,     0,   528,  1138,     0,  1721,   528,  1142,     0,
       0,   528,  1146,     0,     0,     0,     0,     0,     0,  1150,
       0,     0,   169,     0,     0,     0,     0,     0,     0,   169,
       0,     0,   169,     0,     0,     0,   169,     0,     0,   211,
       0,     0,     0,   324,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,   528,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   211,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   478,     0,     0,   482,     0,   639,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1877,  1878,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,   211,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1655,     0,   269,  1658,  1672,     0,     0,     0,     0,  1679,
       0,     0,     0,  1683,   169,  1685,     0,     0,     0,     0,
       0,     0,   169,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   482,   528,     0,     0,     0,     0,     0,     0,
       0,     0,   211,     0,     0,     0,     0,     0,   294,     0,
     639,   639,     0,     0,     0,     0,     0,   639,     0,  1707,
    1708,   300,   632,   301,   649,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,  1972,     0,     0,
     169,     0,     0,   169,     0,   169,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,   528,  1385,     0,   528,  1389,     0,
    1721,   528,  1393,     0,     0,     0,     0,   715,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2005,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,  2024,
       0,     0,  1789,     0,   530,   531,     0,     0,   535,     0,
       0,   538,   539,     0,   541,     0,   542,     0,     0,     0,
       0,   632,     0,     0,     0,     0,  2052,   816,     0,  2053,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1831,     0,     0,     0,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1846,  1848,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   360,  1829,     0,     0,     0,     0,   639,
    1513,     0,  1658,     0,     0,     0,     0,  1840,  1866,     0,
       0,     0,     0,     0,   211,   211,     0,   621,   622,     0,
     478,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   654,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2139,     0,
       0,     0,  1874,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   528,  1566,     0,     0,
       0,     0,     0,     0,   365,   528,  1575,     0,   639,     0,
       0,     0,     0,   169,     0,     0,     0,     0,     0,   360,
     360,     0,     0,   478,     0,   940,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   632,     0,   169,     0,
       0,     0,     0,     0,   169,     0,     0,     0,   790,     0,
    1946,     0,     0,     0,     0,     0,     0,     0,     0,  1949,
     211,  1951,     0,     0,  1956,  1960,     0,  1672,     0,     0,
       0,     0,  1966,   715,     0,     0,   715,   715,     0,   715,
       0,     0,     0,  1938,     0,     0,     0,  1948,   715,     0,
       0,   715,   715,   715,     0,     0,     0,     0,     0,     0,
       0,  1961,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1970,     0,     0,     0,     0,     0,     0,     0,   868,
     169,     0,     0,     0,  1982,     0,  1984,  1985,  1986,  1987,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   434,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,   639,
    2041,   463,     0,     0,     0,  2046,  2048,     0,   478,     0,
       0,     0,     0,     0,   491,     0,   491,     0,     0,     0,
    2025,     0,     0,     0,  2030,  2068,     0,   169,   169,  2035,
     478,   478,     0,     0,     0,   373,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   478,
       0,     0,     0,     0,  2065,     0,     0,     0,     0,   952,
     953,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   960,     0,  2097,     0,  2100,     0,     0,  2102,
    2104,     0,     0,     0,  2107,  2109,     0,     0,     0,     0,
       0,     0,     0,   528,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2093,     0,     0,     0,  2096,   528,
     600,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2110,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   816,     0,     0,
       0,     0,     0,   169,  2153,  2155,  2157,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2146,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
    2182,  2184,  2186,     0,     0,  1057,  1058,   365,     0,     0,
       0,  1062,     0,     0,     0,     0,     0,  2169,     0,     0,
    2170,     0,  2172,     0,     0,     0,     0,     0,     0,  2176,
       0,     0,  1083,     0,     0,  1086,  1087,     0,  1090,     0,
    1092,  1093,     0,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,   360,     0,  2196,     0,
    2197,     0,     0,     0,  2200,     0,  2176,     0,     0,     0,
       0,     0,     0,     0,     0,   528,   528,     0,     0,     0,
       0,     0,  1136,     0,     0,     0,  1140,     0,     0,  2200,
    1144,   528,     0,     0,     0,     0,   491,     0,     0,     0,
       0,     0,   491,     0,     0,     0,     0,   837,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     478,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
    1259,  1260,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1276,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   715,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   917,     0,     0,   528,     0,     0,     0,
       0,     0,     0,     0,   528,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   715,     0,     0,     0,     0,     0,
       0,     0,   463,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   948,     0,     0,     0,     0,
       0,     0,     0,   269,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,   360,   632,     0,     0,     0,   528,  2083,
       0,     0,   528,     0,     0,     0,     0,     0,     0,     0,
     982,     0,     0,     0,     0,  1276,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,   715,     0,     0,
       0,     0,     0,     0,   837,  1002,     0,     0,  1004,     0,
    1006,     0,     0,   528,     0,     0,  1015,     0,  1020,  1015,
       0,     0,     0,     0,     0,     0,  1376,     0,     0,     0,
       0,     0,     0,  1383,     0,     0,  1387,     0,     0,     0,
    1391,     0,     0,     0,     0,     0,  1048,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1050,
     478,   478,     0,     0,     0,     0,     0,     0,     0,     0,
    1059,     0,     0,     0,     0,     0,     0,   528,   528,     0,
       0,     0,     0,     0,   463,     0,     0,  1048,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     715,   715,   715,     0,  1112,   715,   715,   491,     0,     0,
     528,     0,   482,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
    1151,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,  1511,     0,
       0,   269,    58,     0,     0,     0,  1520,  1521,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   434,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,  1266,  1268,     0,     0,
       0,     0,     0,     0,   463,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,  1564,     0,     0,     0,     0,
       0,     0,     0,     0,  1573,     0,     0,  1577,     0,  1580,
    1581,  1873,  1015,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,    79,    80,     0,  1048,     0,  1439,     0,
       0,     0,     0,     0,  1307,     0,     0,     0,     0,     0,
       0,  1015,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1609,     0,     0,     0,   382,
       0,     0,   383,     0,   384,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     211,  1181,     0,   386,    -2,     0,  1183,   491,     0,  1184,
    1185,  1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  -338,  -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,
       0,   387,   388,   269,   485,   390,  1202,  1203,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,  1204,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
    1715,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   491,     0,  1375,     0,  1378,     0,
       0,     0,  1205,     0,     0,    77,   399,   365,     0,     0,
     298,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,     0,     0,  -185,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   715,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   478,   478,  1577,     0,
       0,     0,     0,     0,     0,     0,     0,  1455,  1455,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1782,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1274,     0,     0,     0,
       0,   269,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1508,     0,     0,
       0,     0,     0,  1518,     0,     0,     0,     0,     0,     0,
     382,     0,     0,   383,     0,   384,     0,   385,     0,     0,
       0,     0,   463,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,   386,     0,     0,     0,     0,   491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1015,     0,     0,   837,     0,     0,
       0,     0,   387,   388,  1875,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1601,
       0,   715,     0,   398,     0,     0,    77,   399,     0,     0,
       0,     0,     0,   400,   461,    80,   401,   402,   403,   404,
    1611,  1612,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   478,     0,     0,     0,     0,     0,
       0,  1932,  1933,     0,     0,     0,     0,     0,     0,  1015,
       0,     0,     0,  1937,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   491,     0,     0,
     837,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   715,     0,     0,   482,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2199,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1439,     0,
       0,     0,  1002,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1733,  1734,     0,     0,     0,     0,     0,     0,
       0,     0,   491,     0,     0,     0,     0,     0,     0,   382,
       0,     0,   383,     0,   384,     0,   385,     0,     0,     0,
     491,     0,   837,     0,     0,     0,     0,  2014,     0,     0,
       0,  1181,     0,   386,    -2,     0,  1183,  -244,  -244,  1184,
    1185,  1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  -338,  -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,
       0,   387,   388,     0,   485,   390,  1202,  1203,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,  1204,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2081,     0,   434,
       0,  -244,  1205,     0,  1814,    77,   399,     0,     0,     0,
     298,     0,   400,    79,    80,   401,   402,   403,   404,  2199,
       0,     0,     0,     0,     0,     0,     0,  -185,    14,    15,
      16,    17,    18,     0,     0,    20,  1439,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,  1860,     0,     0,    46,   382,    47,     0,
     383,     0,   384,     0,   385,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,  1181,
       0,   386,    -2,     0,  1183,  -245,  -245,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  -338,
    -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,  1894,   387,
     388,  1896,   485,   390,  1202,  1203,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,  1204,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,  1923,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -245,
    1205,     0,     0,    77,   399,     0,     0,     0,   298,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,     0,  -185,     0,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1180,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,  1181,    58,  1182,    -2,     0,
    1183,     0,     0,  1184,  1185,  1186,  1187,  1188,  1189,  1190,
    1191,  1192,  1193,  1194,  1195,  -338,  -338,  1196,  1197,  1198,
    1199,  1200,     0,  1201,     0,   387,   388,    61,   485,   390,
    1202,  1203,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,  1204,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -3,  1205,     0,  1015,    77,
     430,     0,     0,     0,   298,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
       0,  -185,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1180,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
    1181,    58,  1182,    -2,     0,  1183,     0,     0,  1184,  1185,
    1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,
    -338,  -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,
     387,   388,    61,   485,   390,  1202,  1203,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,  1204,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1205,     0,     0,    77,   430,     0,     0,     0,   298,
       0,   400,    79,    80,   401,   402,   403,   404,     0,     0,
       0,     0,     0,     0,     0,     0,  -185,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,    61,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1666,  1667,  1668,     0,     0,     0,   398,  1669,  1670,    77,
     430,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
       0,  1671,     4,   183,     6,     7,     8,     9,    10,    11,
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
       0,     0,     0,     0,     0,  1666,  1667,  1668,     0,     0,
       0,   398,  1669,     0,    77,   430,     0,     0,     0,     0,
       0,   400,    79,    80,   401,   402,   403,   404,     0,     0,
       0,     0,     0,     0,     0,     0,  1671,   183,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     252,   253,     0,   254,    46,     0,    47,     0,   255,     0,
       0,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   386,     0,     0,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -458,   387,   388,    61,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,  1657,    77,   430,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,    61,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,   430,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     382,     0,    46,   383,    47,   384,     0,   385,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,   388,     0,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   398,     0,     0,    77,   460,     0,     0,
       0,     0,     0,   400,   461,    80,   401,   402,   403,   404,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,  1263,     0,     0,     0,     0,     0,   400,  1264,
      80,   401,   402,   403,   404,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     382,     0,    46,   383,    47,   384,     0,   385,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,   388,     0,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   398,     0,     0,    77,   399,     0,     0,
       0,     0,     0,   400,    79,    80,   401,   402,   403,   404,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,   460,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,  2023,     0,    -2,    -2,    -2,
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
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,  2051,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,  1515,
      -2,     0,     0,     0,     0,    -2,    -2,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,  1516,    80,   401,
     402,   403,   404,     4,     5,     6,     7,     8,     9,    10,
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
       0,     0,  -811,     0,     0,    79,    80,   257,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
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
     258,     0,     0,     0,     0,     0,     0,    79,    80,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -406,  -406,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -406,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,    79,
      80,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -407,  -407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -407,
       0,     0,     0,    77,    78,     0,  1414,     0,  1415,     0,
       0,    79,    80,  1416,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1418,     0,     0,     0,    77,   978,
       0,  1414,     0,  1415,     0,     0,    79,    80,  1416,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1621,
       0,     0,     0,    77,   978,     0,  1414,     0,  1415,     0,
       0,    79,    80,  1416,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1622,     0,     0,     0,    77,   978,
       0,  1414,     0,  1415,     0,     0,    79,    80,  1416,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1623,
       0,     0,     0,    77,   978,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   257,     0,
       0,     0,     0,     0,     0,    77,   258,     0,    14,    15,
      16,    17,    18,    79,    80,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   322,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,    76,
       0,    77,   258,     0,     0,     0,  -815,     0,     0,    79,
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
      76,     0,    77,   258,     0,     0,     0,     0,     0,     0,
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
      74,     0,  1103,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -675,    77,   342,     0,     0,     0,     0,     0,
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
       0,     0,     0,   341,    77,   342,     0,     0,     0,     0,
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
       0,     0,    74,     0,  1912,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   342,     0,     0,     0,
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
       0,     0,     0,    74,     0,  1914,     0,     0,     0,     0,
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
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   322,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,    77,   342,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,  1439,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   382,     0,     0,
     383,     0,   384,     0,   385,     0,     0,     0,     0,    77,
     258,     0,     0,     0,     0,     0,     0,    79,    80,  1181,
       0,   386,    -2,     0,  1183,  1939,  1940,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,     0,
       0,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,   387,
     388,     0,   485,   390,  1202,  1203,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,  1204,   393,   394,
     395,  1439,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1205,     0,   382,    77,   399,   383,     0,   384,   298,   385,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,  1181,  -185,   386,    -2,     0,  1183,
       0,     0,  1184,  1185,  1186,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  1195,     0,     0,  1196,  1197,  1198,  1199,
    1200,     0,  1201,     0,   387,   388,     0,   485,   390,  1202,
    1203,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,  1204,   393,   394,   395,     0,   396,   397,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1205,     0,     0,    77,   399,
       0,     0,     0,   298,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,     0,     0,
    -185,    14,    15,    16,    17,    18,    19,   702,    20,   703,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   704,     0,     0,     0,     0,  1195,     0,
    -338,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1205,     0,     0,    77,   705,     0,     0,     0,   298,     0,
     400,    79,    80,   706,   707,   403,   404,    14,    15,    16,
      17,    18,    19,   702,    20,   703,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   704,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     705,     0,     0,     0,   298,     0,   400,    79,    80,   706,
     707,   403,   404,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,   429,    77,   430,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,    14,
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
       0,    77,   705,     0,     0,     0,   298,     0,   400,    79,
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
       0,     0,     0,     0,   398,     0,     0,    77,   430,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     398,     0,     0,    77,   460,     0,     0,     0,     0,     0,
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
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   693,     0,   694,   695,   588,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,   -17,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      63,    64,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -483,
    -483,     0,  -483,    46,     0,    47,     0,  -483,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,    77,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,    78,     0,     0,
       0,  -813,     0,     0,    79,    80,    14,    15,    16,    17,
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
       0,     0,     0,     0,     0,     0,     0,     0,   900,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,  -688,    77,
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
       0,     0,    75,    76,     0,    77,   322,     0,     0,     0,
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
       0,     0,     0,     0,     0,  1823,     0,     0,     0,   183,
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
       0,     0,     0,     0,     0,     0,     0,     0,   984,    77,
     978,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1533,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   978,     0,     0,     0,     0,     0,     0,    79,
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
       0,     0,     0,    77,   456,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,    77,   978,     0,
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
     978,     0,     0,     0,     0,     0,     0,    79,    80,    14,
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
     382,     0,    46,   383,    47,   384,     0,   385,     0,     0,
       0,     0,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,   388,     0,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   398,     0,     0,    77,   399,     0,     0,
       0,     0,     0,   400,   461,    80,   401,   402,   403,   404,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,    14,
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
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    63,    64,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,   382,     0,     0,   383,
       0,   384,     0,   385,    77,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,   387,   388,
       0,   389,   390,  1954,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   391,   392,   379,     0,   393,   394,   395,
       0,   396,   397,   382,     0,     0,   383,     0,   384,    74,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,  1666,  1667,  1668,     0,   386,     0,   398,
    1955,     0,    77,   399,     0,     0,     0,     0,     0,   400,
      79,    80,   401,   402,   403,   404,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   485,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,    76,     0,   486,
     487,     0,     0,     0,   488,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1310,     0,    77,   399,     0,     0,     0,  1311,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,     0,     0,    77,   399,     0,
       0,     0,   488,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,   836,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
     298,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  1011,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,  1042,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  1377,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,  1449,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
    1524,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,  1945,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1950,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  1959,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2045,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2047,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2099,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2101,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2103,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2106,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2108,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2152,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2154,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2156,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2181,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2183,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2185,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   684,     0,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   690,     0,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   699,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,   916,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,   461,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,  2040,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,     0,   396,   397,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   398,     0,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   184,     0,   185,   186,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   693,     0,   694,   695,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -482,  -482,     0,  -482,    46,     0,    47,
       0,  -482,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    75,    75,     4,   166,    75,    75,   166,   182,   398,
     230,   182,     1,   251,   256,   302,   372,   345,   731,   203,
     343,     1,   177,   716,   925,   932,  1223,   488,     1,    59,
     230,   647,   230,  1017,  1205,  1165,   233,   360,   235,    77,
     218,   364,   638,  1361,  1362,   242,   640,   775,  1797,   230,
     634,   167,  1939,   805,   634,    56,    57,    89,    59,   811,
    1797,   968,  1046,   141,   181,   909,   400,   804,    75,   360,
      59,    84,   709,   364,    75,   271,   893,    97,   230,    59,
     230,   545,   546,    84,   230,     1,    59,     1,  1205,     1,
       4,    92,   634,     1,   314,    84,    97,     1,    72,   100,
      10,   166,   230,   104,    75,   922,  1797,    75,    97,   804,
    1676,   100,   309,   310,   314,   104,   314,   147,   192,   152,
       1,   802,   192,   192,   202,  1109,   230,   160,   160,    72,
     230,   605,    89,   314,   802,     1,  1190,   150,     4,   462,
     468,   142,   802,    59,   145,    59,   147,    59,    89,  1441,
    1442,    59,   153,   822,   802,    59,   230,   230,   147,   160,
     230,   230,   314,    10,   314,    97,   167,   147,   314,    97,
      84,   840,  1441,    75,   147,    98,   556,   160,    59,    84,
     821,   822,   156,   165,   166,   192,   314,   567,   100,   398,
     191,   192,   156,    59,   177,    75,   104,   161,     1,   840,
    2087,     4,   191,   160,  1798,   206,     1,   671,   804,   152,
     314,   231,   806,   909,   314,   216,   810,   206,   802,   160,
     221,   192,   802,   230,   192,   819,   820,   141,   608,   230,
     231,   147,   337,   147,   100,   147,   150,  1319,   104,   147,
     314,   314,   231,   147,   314,   314,   259,   152,    89,   918,
     251,     4,   162,     1,   177,   293,    59,   167,   259,   230,
     802,   974,   230,  1672,  1943,   152,   147,    84,   269,   270,
     259,   376,   273,   158,    90,   177,  1842,   918,   343,   280,
      97,   147,   118,   100,   273,  1174,   506,   104,   202,   176,
    1003,     0,   177,   294,   295,   315,   297,   177,   626,   231,
     623,   104,   160,   231,   151,   543,   506,   594,   506,   167,
     323,   549,   668,   314,   315,   162,   494,   158,   782,   160,
     167,  1073,   650,   324,   647,   506,   315,  1055,  1174,   657,
     331,   332,   623,   529,  1928,   336,   152,   660,  1182,  1162,
     729,   537,  1079,   257,   147,   259,  1169,   821,   822,  1166,
     466,   104,   686,   949,   506,   992,   506,   240,   133,  1476,
     506,   273,  1479,  1480,   159,   151,   840,   551,  1212,   660,
    2119,  1278,   158,   374,   191,    62,   377,   378,   506,  1286,
     458,   601,  2119,   315,  1079,   268,   302,   315,   302,   206,
     110,  1072,    61,    62,   302,   134,   279,   462,   302,  1529,
    2090,   601,   506,   601,  1072,    99,   506,   273,  1700,   323,
     158,   159,  1072,   133,   231,   133,     4,     0,   105,   432,
     601,   302,   109,  1246,  1072,   112,  2116,   114,  2119,   168,
    1174,  1700,   506,   345,   154,     0,   506,   506,  1847,  1848,
     624,   442,   259,   605,   918,   160,   118,   222,   486,   601,
     123,   601,  1121,   442,  2144,   601,   273,   175,   152,  1777,
    1778,  2055,   177,  2142,   465,   466,    77,    78,    56,    57,
    1552,  1553,  1554,   601,    75,   684,   477,   478,   687,   688,
    1121,   690,   155,  1079,   156,   486,  1182,   488,  1072,    90,
     699,   160,  1072,   702,   703,   704,    72,   601,   518,   506,
    2179,   601,  1096,   154,    92,   506,   622,   158,   177,   134,
     890,    72,    72,   239,   988,   158,  1212,   518,   432,  1271,
     246,   152,  1411,  1412,  1413,    72,  2120,   601,   303,   518,
    1072,   601,   601,   146,   177,   506,    77,    78,   506,  1710,
     605,   267,   543,   168,   458,    75,  1955,  1956,   549,   160,
     158,    72,   278,  1845,   142,   168,   468,   145,   106,   107,
     155,    91,    72,  1547,    20,  1411,  1412,  1413,  2162,   177,
     590,   110,   160,   901,   758,   262,   152,   607,   108,   167,
     156,   176,   647,   603,    75,  1408,   518,   588,  1049,   590,
     518,   152,   152,  1710,   133,   156,   156,   203,  1350,    90,
     601,   590,   603,   109,   842,   152,   607,   155,   154,   156,
     938,   177,   158,   936,   603,   799,   617,  1671,   607,   160,
     621,   622,  1676,   398,   152,   442,   132,   607,   128,   129,
     868,   152,    72,   221,   607,   156,  1329,   821,   822,   756,
     158,   815,   152,    72,   815,   936,   156,  1121,   157,  1265,
     152,   829,   826,   654,   158,    72,   840,   158,   590,   177,
     347,   348,   590,   350,   168,   352,   667,  1411,  1412,  1413,
     158,   603,   172,   173,   588,   603,   177,    75,   594,   591,
     594,   269,   270,   566,   592,   158,   594,  2005,   804,   177,
     594,   607,   280,   607,    92,   607,   177,    72,   160,   607,
      72,   518,   389,   607,   177,   167,   294,   295,   624,   297,
     108,    58,   152,   594,    61,    62,   156,    64,  1431,   757,
     721,   134,   723,   152,   725,   152,   607,   156,   729,   154,
    1631,   732,  2024,   158,   918,   152,   324,   343,   650,   156,
     346,   607,   149,   331,   332,  1441,  1442,   158,   336,   152,
     163,   164,   869,   160,   360,   152,   757,   532,   364,  1452,
    2052,   146,   147,   148,   280,   540,   177,    18,    72,   176,
      13,    14,    15,    16,    17,   158,  1760,   152,  1762,   295,
     152,   156,   557,   168,   156,   168,   374,  1984,  1842,   377,
    1987,  1255,   177,   568,    72,   146,   147,   148,   485,  2091,
     158,   802,    72,   804,   607,   152,   158,   533,   158,     3,
      61,    62,    63,    64,    72,   816,  1205,   168,  1725,  1726,
    1294,   374,   823,    69,   377,   177,   988,   177,   829,    72,
      72,   832,   558,    13,    14,    15,    16,    17,   163,   565,
     841,   842,   843,   569,   960,   170,   171,   158,   152,   156,
    1311,  1544,   156,   104,   161,   386,   462,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   177,   868,  1042,    72,
     154,    72,   106,   107,   152,   159,   563,   465,   156,    62,
     411,   412,   152,   799,     3,   152,   156,  1361,   152,   477,
     478,   134,    72,   160,   152,   159,   155,   154,   156,  1953,
     167,   432,   159,  1259,  1260,   156,   907,   908,   909,   684,
     152,  1965,  1108,    72,   156,   690,   152,   100,    13,    14,
      15,    16,    17,   988,   699,   158,   153,   151,   111,   909,
     113,   462,   115,   160,   158,  1531,   909,  1121,  1116,   106,
     107,  1269,  1265,   718,  1118,   551,   154,   154,   160,   152,
    1147,   152,   159,   156,   134,   156,     3,   165,   166,   960,
     132,   152,  1078,  1079,   160,   156,    13,    14,    15,    16,
      17,   154,   973,   158,   157,   158,   158,    72,   176,   152,
     152,   154,  1889,   168,   156,  2039,   168,   152,  1449,   901,
     154,   163,   164,   909,   158,   909,  1205,   909,   160,   605,
     588,   909,   914,   158,  1700,   909,   174,   154,  1009,   158,
     924,  1249,   159,   168,   152,   132,  1017,   623,   624,   168,
     132,   152,   158,   206,   160,    72,   938,  1416,   909,   617,
    1504,  1624,  1274,   621,   622,   152,  1629,   132,   154,   156,
     152,   647,   158,   909,   156,  1046,   163,   164,  1049,   155,
     156,   163,   164,   159,   660,   118,   154,   152,   924,   176,
     158,   156,   154,  1524,    47,    48,   654,    50,   163,   164,
     154,  1072,    55,     3,   158,   154,   154,  1078,  1079,   667,
     158,  1542,   154,    13,    14,    15,    16,    17,  1995,  1298,
     273,   152,   275,   276,    13,    14,    15,    16,    17,   154,
     875,  1256,  1257,   152,   154,   154,   909,   156,  1109,   154,
    1284,   156,   887,  1284,   132,     3,   891,   154,   155,   132,
     895,   924,    58,   152,   307,    61,    62,   156,    64,   154,
     313,  1293,  1294,   721,   152,   723,   152,   725,   156,   152,
     156,   729,    72,   156,   732,   163,   164,   126,   127,  1845,
     163,   164,   758,    72,  1441,   152,   154,   154,  1332,   156,
     158,  1174,   345,   152,   152,  1521,   892,   156,   351,   757,
     353,   924,   111,   112,   113,   114,   115,  1361,  1362,    22,
     152,   154,   154,  1184,   156,   158,  1187,  1188,  1189,   158,
     152,   152,   154,   799,   156,  1369,  1370,   152,  1369,  1370,
    1265,  1397,  1182,  1362,   735,   158,   389,   158,   154,  1182,
     152,  1212,   158,   158,   156,   158,   104,  1218,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   816,  1294,
    1231,    99,  1212,  1234,  1235,   823,   154,  1238,   154,  1212,
     158,    89,   158,   154,   132,  1234,  1235,   158,  1249,    84,
     154,  1165,   154,   841,   158,   843,   158,   130,   131,   442,
    1174,   146,   147,   148,   152,   153,  1182,  1980,  1182,   152,
    1182,   159,   152,   158,  1182,   163,   164,   154,  1182,   160,
     154,  1282,   154,   168,   158,   468,   158,   470,   471,   154,
     160,   154,   177,   158,  1295,   158,  1212,   151,  1212,  1165,
    1212,  1182,   485,  1777,  1212,   736,   737,   738,  1212,   121,
    1311,   123,   124,   125,   160,   150,  1182,   157,  1319,   907,
     908,   909,  1234,   157,   158,   154,   167,  1235,  2024,   158,
     936,  1212,   154,   939,   154,   518,   158,   154,   158,   176,
     152,   158,  1504,   155,   156,   154,  1212,   154,   160,   161,
    1351,   158,   157,   158,   154,   190,  2052,  1269,   158,   154,
     543,   118,  1165,   158,   152,   548,  2124,   550,  1234,  1235,
    2128,   152,   960,   152,   146,   147,   148,   163,   164,  1182,
     157,   158,   988,  1739,   157,   158,   158,   152,   571,   158,
     573,   574,   162,  1168,   169,  2091,   168,   164,  1411,  1412,
    1413,   155,  1422,  1416,  1417,   177,  1181,   156,   591,  1212,
     157,   158,  1165,  1700,   174,  1635,   132,  1234,  1235,   154,
     603,  1422,   154,  1198,   259,  1426,   157,   158,  1429,   157,
    1205,   154,  1235,   158,   159,  1635,   154,  1635,   134,  1504,
    1171,  1172,  1173,   626,   154,   628,   629,   154,  1449,   743,
     744,   745,   746,   984,  1635,  1621,  1622,  1623,   989,   157,
     158,  1441,  1442,   158,  1648,    91,    92,   650,   651,  1000,
     134,  1472,  1473,   159,   657,    13,    14,    15,    16,    17,
      18,  1482,  1235,  1635,    78,  1635,   157,   158,   323,  1635,
    1422,   157,   158,  1482,  1422,   157,   158,  1411,  1412,  1413,
    1414,  1415,  1416,  1417,  1642,  1643,  1644,  1635,   343,   159,
     104,   152,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   154,  1524,   152,  1441,  1442,  1441,  1442,  1441,
    1442,  1635,   154,  1441,  1442,  1635,   176,  1441,  1442,   157,
     158,  1542,   154,  1752,   157,   158,  1547,   154,  1414,   157,
     158,  1552,  1553,  1554,   157,   158,  1718,   157,   158,  1718,
    1441,  1442,  1635,    13,    14,    15,    16,    17,    18,   154,
    1482,   157,   158,   157,   158,  1441,  1442,    13,   157,   158,
     154,  1698,   154,   177,   157,   158,   216,   158,   159,   157,
     158,   154,  1812,  1777,  1778,   154,  1184,   432,   156,  1187,
    1188,  1189,   157,   158,  1624,    77,    78,   158,   159,  1629,
     157,  1414,  1812,    70,  1812,  1529,  1482,  1637,   160,  1778,
    1320,  1321,   157,  1624,  1212,   739,   740,   462,  1629,   160,
    1218,  1812,   741,   742,  1635,   160,  1637,   160,  1441,  1442,
    1878,  2069,  2070,  1231,  1645,   747,   748,  1479,  1480,  1972,
    1238,  1182,    88,  1718,  1522,  1523,  1657,  1432,  1433,  1265,
    1812,  1414,  1812,  1529,   160,  1482,  1812,   152,   104,  1670,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1643,  1644,  1256,  1257,  1812,    78,   157,    18,  1294,   176,
     158,   160,  1624,   152,  1282,   177,  1624,  1629,   154,   154,
     160,  1629,   177,   157,   157,  1637,  1481,  1295,  1812,  1637,
     545,   546,  1812,   160,  1715,    18,   151,   154,   901,   160,
    1700,   154,   154,    22,   154,   154,  1529,   154,  2056,   154,
    2053,   914,   154,   154,  1265,   154,   154,   151,   151,   666,
     160,    70,   154,   160,  1861,    13,    14,    15,    16,    17,
      18,   177,  1664,   176,   154,   938,   154,  1665,   154,  1760,
     151,  1762,   154,   160,   154,  1939,   949,   176,  1939,   158,
    1301,  1302,  1303,   160,   158,   958,  1529,  1308,  1309,   154,
     154,   158,   154,   154,  1700,   154,  1700,   154,  1700,   154,
     154,   157,  1700,   154,   157,  1815,  1700,   154,  1664,  1665,
      13,    14,    15,    16,    17,    18,   154,  1721,   154,   154,
     241,  1812,   647,   154,  1815,   154,   154,  2055,   154,  1700,
     157,  2005,   154,   151,   176,  1826,   151,   154,   158,  1830,
     154,   152,   158,   152,  1700,   152,   671,   152,  1426,   152,
     152,  1429,    14,  1844,    74,   159,  2005,   158,    80,    18,
     159,   157,   157,  1854,   177,  1721,   486,   177,   488,  1976,
     151,   177,  1665,   160,   158,  1845,  1867,   177,  1869,  1870,
    1871,  1872,  1055,   177,   154,   157,  1877,  1878,   154,   158,
     158,   154,   157,  1815,  1472,  1473,   158,  1815,    57,    58,
      59,    60,    61,    62,    63,    64,  1079,  1700,  1504,  2119,
     827,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     154,  2085,  1665,  2087,  2085,  1690,  2087,  1937,  1721,  2119,
    1832,  2119,   157,   154,   151,   151,   177,   152,   177,  1845,
     152,  1845,   177,  1845,   177,   177,  1937,  1845,  2119,   177,
     177,  1845,  1943,    92,   152,   151,  1947,   782,   177,   177,
     152,  1952,  2126,   152,    90,  2126,   154,   151,    72,  1972,
     151,   158,    65,   158,  1845,   151,  1832,  2119,  1721,  2119,
     160,   160,   157,  2119,   157,   157,  1977,   154,   157,  1845,
     151,   154,   583,   159,   159,   151,  2160,   121,   151,   154,
     104,  2119,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   154,   154,   435,  1937,   154,  2124,   154,  1937,
     157,  2128,  2129,   157,   151,  2119,   177,   159,   158,  2119,
     451,   152,   154,   454,   152,   152,  2027,   157,   157,   151,
    2031,   157,   151,   154,  2208,   160,   151,  2208,   152,   153,
    2053,   154,  1845,  2044,  2024,  2119,  2119,  2164,   154,  2119,
    2119,  1234,   876,   154,  2055,   154,  2057,  1645,   154,   157,
     154,   154,    75,     1,    75,   177,     4,   177,   151,  1657,
     152,   152,  2052,   177,  2191,   154,   154,   157,  2195,   151,
     511,   157,  1670,   160,   151,   151,  1269,   156,    75,   154,
     925,   154,  1275,  2094,   154,   154,  2213,   154,   154,   154,
    2120,   155,   177,    75,   168,   151,   168,   177,  2024,   159,
    2024,  2091,  2024,   151,   154,   154,  2024,   941,  2119,  2120,
    2024,    59,   154,   154,   177,   158,   151,  1715,   104,  2130,
     153,  2120,  2133,   168,  2135,   151,  2052,    75,  2052,   168,
    2052,  2142,  2162,  2024,  2052,   159,    84,   152,  2052,   158,
      75,   151,    13,    14,    15,    16,    17,   153,  2024,    97,
     168,  2162,   100,   157,   168,   177,   104,   177,    75,    75,
    2171,  2052,  2173,  2162,  1705,  2091,  2177,  2091,  2179,  2091,
     151,   154,   159,  2091,   153,   151,  2052,  2091,  2120,   154,
     151,   154,  2120,   152,   177,   177,   154,   154,   177,   829,
    1752,  2202,   832,   141,  1326,   708,   749,   751,   753,   147,
    2091,    72,   150,  2214,   750,   153,   154,   431,   752,  1200,
    1212,  2024,  2223,  2179,  1700,  2091,  2087,  1051,   166,  1853,
    2162,  1845,  2116,  1712,  2162,  1709,  1691,  2159,  1826,  1691,
    1423,  2053,  1830,  2129,  1068,  1069,  2195,  2052,  1238,  2052,
      49,  1937,   190,   191,   192,   112,  1844,   264,  2014,  1417,
      62,   964,   829,  1231,   202,   203,  1854,   928,   206,     0,
     617,   132,   495,  1721,   973,   876,   774,   774,  1612,  1867,
     774,  1869,  1870,  1871,  1872,    -1,    -1,    -1,  2091,  1877,
      -1,   152,   230,   231,    -1,   156,    -1,    -1,   100,  1482,
      -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,   111,
     112,    -1,    -1,   251,    13,    14,    15,    16,    17,   104,
      -1,   259,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,  1258,    -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,
     941,    -1,    -1,   774,   775,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   784,    72,  1943,   787,    -1,    -1,  1947,
    1287,    -1,    -1,    -1,  1952,    -1,  1972,    -1,   153,   307,
      -1,   156,    -1,    72,    -1,   313,   314,   315,    -1,  1009,
      -1,    -1,    -1,    -1,    -1,   323,   104,  1017,    -1,  1977,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,   206,   343,   344,   345,    -1,  1336,
      -1,    -1,    -1,    -1,   132,  2190,  1046,   848,    -1,  1049,
    1255,    -1,   360,    -1,   855,    -1,   364,    -1,   859,  2204,
    1265,    -1,   863,   132,   152,   153,    -1,    -1,    -1,  2027,
      -1,    -1,    -1,  2031,    -1,   163,   164,  2053,  1979,    -1,
    1051,    -1,    -1,   152,    -1,    -1,  2044,   156,    -1,    -1,
     398,    -1,    -1,    -1,   163,   164,    -1,  1068,  1069,  2057,
      -1,   273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,
     104,  1664,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,   432,    -1,    -1,   435,    -1,    -1,
      -1,    -1,    -1,    -1,   442,   307,  2094,    -1,    -1,    -1,
      -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,  1342,  1343,
     458,    -1,    -1,    -1,   462,    -1,    -1,    -1,   466,    -1,
     468,    -1,  1356,  1357,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2130,   345,    -1,  2133,    -1,  2135,    -1,    -1,
      -1,    -1,    -1,   177,  2142,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   506,    -1,
    1394,  1395,  1396,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     518,    -1,    -1,  2171,    -1,  2173,    -1,   389,    -1,  2177,
      -1,  2179,  1519,    -1,    -1,    -1,    -1,    -1,    -1,  1526,
      -1,    -1,    -1,    -1,    -1,   543,    -1,   545,   546,    -1,
      -1,   549,    -1,   551,  2202,    -1,  1543,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1055,    -1,  2214,    -1,   177,    -1,
      -1,    -1,  1815,    -1,   104,  2223,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,  1832,
      -1,   104,   590,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   601,    -1,   603,   468,   605,   104,   607,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,  1311,    -1,   485,    -1,   623,   624,    -1,   626,  1319,
      -1,    -1,    -1,   163,    -1,  1126,   634,    -1,  1129,    -1,
     638,    -1,  1133,    -1,    -1,    -1,   104,   160,    -1,   647,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   657,
      -1,    -1,   660,    -1,   160,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,   671,    -1,     4,    -1,    -1,    -1,    -1,
      -1,  1342,  1343,    -1,    -1,  1928,   684,    -1,   550,   687,
     688,    -1,   690,    -1,  1681,  1356,  1357,    -1,   156,    -1,
      -1,   699,    -1,    -1,   702,   703,   704,    -1,    -1,   571,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,  1602,  1603,
    1604,   108,   109,   110,   111,   112,   113,   114,   115,   591,
      59,    -1,    -1,  1394,  1395,  1396,  1631,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    84,  1743,    -1,    -1,  1449,
     758,    -1,    -1,    -1,   626,   132,    -1,  1754,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,   774,   775,    -1,    -1,
      -1,    -1,    -1,    -1,   782,   152,   153,    -1,   650,   156,
      -1,    -1,    -1,    -1,    -1,   657,   163,   164,    -1,    -1,
      -1,   799,    -1,    -1,   802,    -1,   804,    -1,    -1,   176,
      -1,    -1,   141,  2056,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,   821,   822,   154,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1524,    -1,   165,   166,   167,    -1,
      -1,    -1,   840,    -1,   842,    -1,    -1,  1338,  1732,    -1,
      -1,    -1,  1542,    -1,    -1,    -1,  1347,  1547,    -1,    -1,
      -1,   190,  1552,  1553,  1554,    -1,    -1,    -1,    -1,    -1,
     868,    -1,    -1,   202,   203,    -1,    -1,   206,    -1,    -1,
      -1,    -1,    -1,  1767,    -1,    -1,    -1,    -1,  1772,  1773,
      -1,    -1,  1879,  1880,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    13,    14,    15,    16,    17,    -1,
      -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2162,
     918,    -1,    -1,    -1,    -1,    -1,   924,   925,   257,    -1,
     259,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,
     938,  1602,  1603,  1604,   273,    -1,    -1,    -1,    -1,    -1,
      -1,   949,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,    72,    -1,   294,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,   307,    -1,
      -1,    -1,    -1,    -1,   313,    -1,    -1,  1648,    -1,    -1,
     988,    -1,    -1,   176,   323,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,  1996,
     872,    -1,    -1,    -1,   343,    -1,   345,   346,    -1,    -1,
      -1,    -1,    -1,   132,    72,    -1,    -1,    -1,    -1,    -1,
      -1,   360,    -1,    -1,    -1,   364,    -1,     1,    -1,   901,
      -1,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,    -1,
      -1,    -1,   914,    -1,   163,   164,   104,  1055,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   398,
    1760,  1732,  1762,    -1,  1072,    -1,   938,    -1,    -1,    -1,
      -1,  1079,    -1,    -1,   132,    49,    -1,  2074,    52,    -1,
      54,   104,    56,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   432,   152,   153,  1767,    -1,   156,    73,
      -1,  1772,  1773,    -1,    -1,   163,   164,    -1,    -1,    -1,
      -1,    -1,    -1,  1121,    -1,    -1,    -1,    -1,    -1,   458,
      -1,    -1,    -1,   462,  1625,    -1,    -1,   101,   102,   468,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,  1165,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,    -1,    -1,  1212,    -1,   545,   546,    -1,    -1,
      -1,   550,   551,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,  1234,  1235,    65,    66,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1249,    -1,    -1,   583,  2139,    -1,  1255,    -1,   588,
      -1,    -1,   591,   592,    -1,   594,    -1,  1265,    -1,    -1,
      -1,    -1,   153,    -1,    -1,   156,   605,   104,   607,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   621,    -1,   623,   624,  1294,   626,    -1,    -1,
    1298,    13,    14,    15,    16,    17,  1797,  1798,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   647,    -1,
      -1,   650,    -1,    -1,    -1,   654,    -1,    -1,   657,   156,
      -1,   660,    -1,   662,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   671,    -1,    -1,    -1,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,   687,   688,
      72,   690,    -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,
     699,    -1,  1234,   702,   703,   704,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,  1269,    -1,    -1,
      -1,    -1,    -1,  1411,  1412,  1413,  1414,    -1,  1416,  1417,
     132,    -1,    72,    -1,  1422,  1423,    -1,   155,    -1,   758,
      -1,    -1,   160,    -1,    -1,    -1,    -1,  1928,    -1,    -1,
     152,   153,    -1,  1441,  1442,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,   782,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
     799,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2139,    -1,
      -1,    -1,   132,    -1,  1482,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   821,   822,   108,   109,   110,   111,   112,   113,
     114,   115,   152,   153,    -1,    -1,  1504,    -1,    -1,    -1,
      -1,   840,    -1,   163,   164,    -1,    -1,    -1,   132,  2010,
      -1,    -1,    -1,  2014,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1529,    -1,  1531,    -1,    -1,    -1,    -1,   152,   153,
      -1,  1403,   156,   872,    -1,    -1,    -1,   876,    -1,   163,
     164,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,  1423,    -1,    -1,  2055,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     909,    -1,    -1,    -1,    -1,   914,    -1,     1,    -1,   918,
      -1,    -1,    -1,   104,    -1,   924,   925,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,   936,    -1,   938,
     939,    -1,   941,    -1,    72,    -1,    -1,    -1,    -1,    -1,
    1482,    -1,    -1,    -1,    -1,    -1,  1624,  1625,  2119,  2120,
      -1,  1629,    -1,  1631,    -1,    -1,    -1,  1635,    -1,  1637,
      -1,   152,   153,    -1,    -1,    59,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   988,
      -1,    -1,    -1,    -1,    -1,    -1,  1664,  1665,    -1,    -1,
      -1,  2162,    -1,    -1,   132,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,
      -1,    -1,  1700,    -1,    -1,   163,   164,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
    1718,    -1,  1051,  1721,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   147,    -1,    72,    -1,    -1,    -1,  1068,
    1069,    -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,    -1,   166,    -1,  1752,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1777,
    1778,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,
      -1,     1,  1121,    -1,     4,   132,    -1,    -1,    -1,  1797,
    1798,    72,  1664,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1812,   152,   153,  1815,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    -1,    -1,
      -1,    -1,    -1,   104,  1832,    -1,  1165,   108,   109,   110,
     111,   112,   113,   114,   115,  1174,    -1,  1845,    -1,    59,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,   282,    -1,
      -1,    -1,    -1,    -1,    84,    -1,  1205,    -1,    -1,    -1,
    1878,   152,   153,  1212,    -1,    -1,    -1,    -1,   302,    -1,
      -1,    -1,   163,   164,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1234,  1235,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,  1255,    -1,    -1,   343,
    1928,   141,   346,    -1,    -1,    -1,  1265,   147,    -1,  1937,
    1269,   132,    -1,    -1,    -1,    -1,   360,    -1,    -1,    -1,
     364,    -1,  1281,    -1,    -1,    -1,   166,    -1,    -1,    -1,
      -1,   152,   153,    -1,  1293,  1294,    -1,    -1,   159,  1298,
    1832,    -1,   163,   164,  1972,    -1,    -1,    -1,    -1,    -1,
      -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   202,   203,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,    -1,    -1,   120,  2005,   122,    -1,
      -1,    -1,    -1,  1342,  1343,    -1,  2014,    -1,    -1,    -1,
      -1,   231,    -1,    -1,    -1,    -1,  2024,  1356,  1357,    -1,
      -1,    -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,   153,
      -1,   251,   156,    -1,    -1,    -1,   256,   257,   462,   259,
      -1,    -1,    -1,    -1,  2052,  2053,    -1,  2055,  2056,    -1,
      -1,    -1,    -1,    -1,    -1,  1394,  1395,  1396,    -1,    -1,
      -1,    -1,   282,    -1,  1403,    -1,   286,    -1,    -1,    -1,
      -1,   291,  1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,
      -1,    -1,   302,  2091,  1423,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   315,    72,    -1,    -1,    -1,
      -1,    -1,  1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2119,  2120,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,   343,    -1,    -1,   346,   551,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     360,    -1,    -1,  1482,   364,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2162,    -1,   132,    -1,    -1,   583,
      -1,    -1,    -1,    -1,    -1,  1504,    -1,    -1,   592,    -1,
     594,    -1,    -1,    -1,    -1,    72,   152,   153,    -1,    -1,
      -1,   605,    -1,   607,  2056,    -1,    -1,   163,   164,    -1,
    1529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   623,
     624,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   104,   647,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   132,   660,    72,   458,    -1,
      -1,   102,   462,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,  1602,  1603,  1604,   163,   164,    -1,   104,
     152,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,  1631,    -1,   155,   156,   104,   132,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,  1648,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   132,  1664,  1665,    -1,   163,   164,
      -1,   551,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    -1,    -1,    -1,
      -1,  1700,    -1,   583,    -1,    -1,    -1,    -1,   588,    -1,
      -1,    -1,   592,    -1,   594,   799,    -1,    -1,    -1,  1718,
      -1,    -1,  1721,    -1,    -1,   605,    -1,   607,    -1,    -1,
      -1,    -1,    -1,  1732,    -1,    -1,    -1,   821,   822,    -1,
      -1,    -1,    -1,   623,   624,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,    -1,   840,    -1,   638,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   647,  1767,    -1,
      -1,    -1,   652,  1772,  1773,    -1,    -1,    -1,  1777,  1778,
     660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   876,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    59,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   132,
      -1,    -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1832,   918,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,  1845,    -1,    -1,   104,
     163,   164,   936,    -1,    -1,   939,    -1,   941,   104,   156,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   758,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,   147,    -1,    -1,   775,    -1,    -1,    -1,    -1,
      56,    57,    -1,    -1,   988,    -1,   152,   153,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,   163,   164,   799,
      -1,    -1,   104,    -1,   804,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,    92,    -1,   120,    -1,
     122,   821,   822,    -1,    -1,   104,    -1,    -1,   203,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,    -1,
     840,   120,    -1,   122,    -1,    -1,    -1,  1051,    -1,    -1,
      -1,   153,    -1,  1972,   156,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1068,  1069,   142,    -1,    -1,   145,
      -1,    -1,    -1,    -1,   153,    -1,   876,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,  2005,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,    -1,    -1,    -1,   909,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1121,   918,    -1,
      -1,    -1,    -1,    -1,   924,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,  2052,  2053,    -1,   936,  2056,    -1,   939,
      -1,   941,    -1,    -1,    -1,   221,   946,    -1,    -1,   104,
      59,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,
      -1,   346,  2091,    -1,    -1,    84,    -1,    -1,  1182,    -1,
      -1,    -1,    -1,    -1,    -1,   360,    -1,    -1,   988,   364,
      -1,    -1,    -1,   269,   270,   104,    -1,    -1,    -1,    -1,
     155,    -1,    -1,   112,   280,    -1,    -1,   104,  1212,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   295,
    2139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1235,   141,    -1,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,   324,    -1,
      -1,  1051,    -1,    -1,    -1,   331,   332,    -1,   155,    -1,
     336,  1265,    -1,    -1,    -1,    -1,   104,    -1,  1068,  1069,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
      -1,   190,   120,    -1,   122,    -1,    -1,   462,    -1,  1293,
    1294,    -1,    -1,   202,    -1,    -1,    -1,    -1,   374,    -1,
      -1,   377,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   153,    -1,    -1,    -1,    -1,
      -1,  1121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1342,  1343,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,    -1,
     259,   152,  1356,  1357,    -1,   264,    -1,  1361,  1362,    -1,
      -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1174,    -1,   551,    -1,    -1,    -1,
      -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,   465,
    1394,  1395,  1396,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   477,   478,    -1,    -1,    -1,    -1,    -1,   583,    -1,
      -1,    -1,  1212,    -1,   323,    -1,    -1,   592,    -1,   594,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     605,    -1,   607,    -1,    -1,  1235,    -1,  1441,  1442,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   623,   624,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,
      -1,    -1,   647,    -1,  1274,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,    -1,   398,
      -1,    -1,    -1,  1293,  1294,    -1,    -1,    -1,    -1,    -1,
    1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   617,  1342,  1343,    -1,    -1,    -1,    -1,    -1,   458,
      -1,    -1,    -1,    -1,    -1,    -1,  1356,  1357,    -1,    -1,
      -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   667,    -1,    -1,  1394,  1395,  1396,    84,  1602,  1603,
    1604,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1411,  1412,  1413,  1414,  1415,    -1,   104,    -1,    -1,
      -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   545,   546,    -1,    -1,
      -1,  1441,  1442,    -1,  1648,    -1,   821,   822,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
     147,  1665,    -1,   150,    -1,   840,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   588,
      -1,    -1,    -1,   592,    -1,   594,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1700,    -1,   607,    -1,
      -1,   876,    -1,   190,  1504,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1718,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1732,  1529,
      -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,    -1,
     816,    -1,    -1,   918,    -1,    -1,    -1,   823,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   936,   671,  1767,   939,    -1,   941,    -1,  1772,  1773,
     257,    -1,   259,  1777,  1778,   684,    -1,   264,   687,   688,
       1,   690,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     699,    -1,    -1,   702,   703,   704,    -1,    -1,    -1,    -1,
      -1,    -1,  1602,  1603,  1604,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   988,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   908,    -1,    -1,    -1,   323,  1637,    59,    -1,
      -1,  1845,    -1,    -1,    -1,    -1,    -1,    -1,  1648,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    -1,  1665,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   782,    -1,    -1,  1051,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1068,  1069,    -1,    -1,    -1,    -1,    -1,
    1700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   398,    -1,    -1,    -1,    -1,    -1,    -1,  1718,    -1,
     141,  1721,    -1,    -1,    -1,    -1,   147,    -1,    -1,   150,
      -1,    -1,  1732,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   432,  1121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1767,  1972,   190,
      -1,   458,  1772,  1773,    -1,    -1,    -1,  1777,  1778,    -1,
      -1,   202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1798,    -1,
     909,  2005,    75,    -1,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   924,   925,    -1,    -1,    -1,
    2024,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   257,  1212,   259,    -1,
      -1,    -1,    -1,   264,    -1,  1845,    -1,    -1,  2052,  2053,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,   546,
    1235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   302,    -1,   182,    -1,    -1,    -1,  2091,    -1,    -1,
    1265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   588,   323,    -1,    -1,   592,    -1,   594,    -1,    -1,
      -1,  1187,  1188,  1189,    -1,    -1,    -1,    -1,  1293,  1294,
     607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2139,    -1,    -1,    -1,    -1,
      -1,    -1,  1218,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
      -1,    -1,  1238,    -1,    -1,    -1,    -1,  1342,  1343,    -1,
      -1,    -1,  1972,    -1,    -1,    -1,    -1,   398,   251,    -1,
      -1,  1356,  1357,    -1,   671,    -1,  1361,  1362,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,
     687,   688,    -1,   690,    -1,  2005,  1282,    -1,    -1,    -1,
      -1,   432,   699,    -1,    -1,   702,   703,   704,    -1,  1394,
    1395,  1396,    -1,    -1,  2024,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   458,    88,    -1,
      -1,   314,   315,    -1,    94,    95,    -1,    -1,    -1,    -1,
      -1,    -1,  2052,  2053,    -1,    -1,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1174,  1441,  1442,    -1,    -1,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    84,   382,    -1,    -1,    -1,   386,   387,    -1,
      -1,  2091,    -1,    -1,    -1,   782,  1205,   396,   397,    -1,
      -1,    -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   411,   412,    -1,    -1,    -1,    -1,    -1,    -1,
    2120,    -1,    -1,    -1,   545,   546,  1235,    -1,    -1,  1504,
      -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,  2139,
      -1,    -1,    -1,    -1,    -1,    -1,  1255,    -1,   150,    -1,
    1426,    -1,    -1,  1429,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   462,   166,    -1,    -1,   588,    -1,    -1,
      -1,   592,    -1,   594,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   607,    -1,   190,  1298,
      -1,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   203,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   909,    -1,    -1,    -1,    -1,  1602,  1603,  1604,
      -1,    -1,    -1,   506,    -1,    -1,    -1,   924,   925,    -1,
      -1,    -1,    -1,    -1,    -1,   518,    -1,    -1,    -1,    -1,
     671,    -1,    -1,    -1,    -1,    -1,    -1,   259,    -1,    -1,
      -1,   311,    -1,   684,    -1,    -1,   687,   688,    -1,   690,
     543,    -1,    -1,  1648,    -1,    -1,   549,    -1,   699,    -1,
      -1,   702,   703,   704,    -1,    -1,    -1,    -1,    -1,    -1,
    1665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,    -1,
      -1,   323,    -1,    -1,    -1,  1700,    -1,    -1,   601,    -1,
     603,    -1,  1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   343,    -1,  1718,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1732,    -1,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1645,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1767,    -1,    -1,    -1,    -1,  1772,  1773,    -1,
      -1,    -1,  1777,  1778,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   461,    -1,   463,    -1,    -1,    -1,    -1,    -1,    -1,
    1529,    -1,   472,   473,    -1,    -1,    -1,    -1,    -1,    -1,
     432,    -1,    -1,    -1,    -1,    -1,   735,   736,   737,   738,
     739,   740,   741,   742,   743,   744,   745,   746,   747,   748,
     749,   750,   751,   752,   753,    -1,    -1,    -1,    -1,    -1,
     462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1174,   909,    -1,
      -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   924,   925,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,  1205,    -1,
      -1,    -1,  1631,    -1,    -1,  1212,    -1,    -1,    -1,   802,
      -1,   804,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   589,
      -1,    -1,    -1,   545,   546,    -1,    -1,    -1,  1235,   551,
      -1,    -1,    -1,    -1,    -1,    -1,  1665,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1255,   842,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1700,    -1,    -1,    -1,   868,    -1,  1972,    -1,    -1,
      -1,  1877,    -1,   605,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1298,  1721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   624,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2005,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,   647,    -1,    -1,    -1,  2024,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2052,  2053,    -1,
      -1,    -1,    -1,    -1,    -1,   984,    -1,    -1,    -1,    -1,
     989,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1000,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2091,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1411,  1412,  1413,  1414,  1415,  1416,
    1417,    -1,    -1,    -1,    -1,    -1,  1845,    -1,    -1,    -1,
      -1,    -1,    -1,  1042,  1165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1174,  1441,  1442,   758,    -1,    -1,    -1,
      -1,  1182,    -1,    -1,  2139,   815,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     782,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,    -1,    -1,    -1,    -1,    -1,   799,    -1,  1072,
      -1,    -1,    -1,    -1,    -1,  1078,  1079,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1235,    -1,    -1,    -1,     0,   821,
     822,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1255,    -1,    -1,    -1,   840,    -1,
      -1,    -1,  1529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   902,   903,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   915,   916,   917,    -1,    -1,
     920,    -1,    -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,   918,    -1,    -1,    -1,
      -1,    -1,    -1,   925,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2052,  1631,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1002,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1265,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1249,    -1,  1665,    -1,
      -1,    -1,  2091,    -1,    -1,  1284,   988,    -1,    -1,    -1,
    1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,  1048,    -1,
      -1,    -1,  1301,  1302,  1303,    -1,    -1,    -1,    -1,  1308,
    1309,    -1,    -1,  1700,    -1,    -1,    -1,    -1,    -1,    -1,
    1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1332,  1721,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1095,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1104,  1105,  1106,  1107,   240,    -1,
      -1,    -1,  1112,  1113,    -1,  1752,    -1,    -1,    -1,    -1,
    1369,  1370,  1122,    -1,    -1,    -1,   258,    -1,  1351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,
      -1,  1151,    -1,    -1,  1154,    -1,  1156,    -1,  1529,    -1,
      -1,    -1,    -1,    -1,    -1,   297,   298,    -1,    -1,  1121,
      -1,    -1,   304,   305,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1422,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1845,    -1,
     342,    -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1243,    -1,    -1,    -1,    -1,    -1,    -1,
    1250,    -1,  1252,  1253,    -1,    -1,    -1,    -1,    -1,    -1,
    1631,    -1,    -1,    -1,  1264,    -1,  1266,   399,  1268,    -1,
    1270,    -1,    -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1665,    -1,    -1,    -1,   430,    -1,
      -1,    -1,    -1,  1255,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,   460,  1700,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,    -1,  1294,    -1,  1344,  1345,    -1,   479,    -1,    -1,
    1721,   483,   484,    -1,    -1,   487,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1368,    -1,
     502,   503,   504,   505,    -1,  1375,    -1,    -1,    -1,  1379,
      -1,  1752,    -1,    -1,    -1,    -1,    -1,  2024,    -1,    -1,
      -1,    -1,    -1,   525,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1624,   534,    -1,    -1,    -1,  1629,  1407,    -1,  1361,
    1362,    -1,  1635,    -1,  1637,  2052,    -1,    -1,    -1,    -1,
      -1,  1421,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,   566,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2091,    -1,  1705,    -1,    75,    -1,
      -1,    -1,    -1,  1463,    -1,   597,    -1,    -1,    -1,    -1,
      -1,    -1,   604,    -1,  1845,    -1,    -1,    -1,   610,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   634,   635,   121,    -1,    -1,    -1,  1508,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1516,   134,  1518,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,  1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1567,  1568,    -1,
      -1,    -1,    -1,   705,    -1,   192,    -1,    -1,    -1,    -1,
      -1,    -1,  1582,  1583,    -1,  1585,    -1,    -1,    -1,  1812,
      -1,    -1,  1815,    -1,  1594,    -1,    -1,    -1,    -1,    -1,
     398,    -1,    -1,    -1,    -1,  1605,  1606,  1607,    -1,    -1,
      -1,    -1,    -1,   230,    -1,    -1,    -1,   234,    -1,    -1,
     237,   238,    -1,    -1,   241,    -1,    -1,   244,   245,    -1,
     247,    -1,   249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2024,    -1,  1878,    -1,   789,    -1,    -1,
      -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1631,
      -1,  2052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1939,    -1,   824,    -1,    -1,    -1,    -1,   314,    -1,    -1,
     317,   833,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,
      -1,    -1,    -1,    -1,  1937,    -1,    -1,    -1,    -1,    -1,
    2091,    -1,    -1,   340,   341,    -1,    -1,    -1,    -1,    -1,
    1979,    -1,    -1,  1733,  1734,    -1,    -1,    -1,    -1,   356,
      -1,    -1,    -1,    -1,    -1,    -1,  1746,   545,   546,    -1,
      -1,    -1,    -1,   885,    -1,    -1,    -1,   889,    -1,    -1,
      -1,   893,    -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1783,  1784,  1785,    -1,    -1,    -1,    -1,
     922,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1777,  1778,    -1,    -1,    -1,
     202,   203,  2055,    -1,   451,    -1,  2085,    -1,  2087,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   978,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,    -1,    -1,    -1,    -1,    -1,   241,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2126,    -1,    -1,
      -1,    -1,  1882,    -1,    -1,    -1,   684,    -1,    -1,   506,
      -1,    -1,   690,    -1,    -1,    -1,  2119,  2120,    -1,    -1,
      -1,   699,    -1,  1903,    -1,   522,  1906,  1907,    -1,    -1,
      -1,  2160,    -1,  1913,    -1,    -1,    -1,    -1,    -1,    -1,
     718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1061,
      -1,    -1,    -1,  1065,    -1,    -1,    -1,    -1,    -1,  2162,
    1072,    -1,    -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,
    1082,    -1,    -1,    -1,    -1,    -1,   754,  1089,    -1,  2208,
      -1,    -1,    -1,    -1,    -1,    -1,  1098,    -1,  1100,    -1,
      -1,   343,   344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   601,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1135,    -1,    -1,    -1,  1139,    -1,    -1,
      -1,  1143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   642,   643,    -1,    -1,  1161,
      -1,    -1,    -1,    -1,  1166,    -1,    -1,    -1,   655,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2005,  2054,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   435,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   451,
     452,    -1,   454,   455,    -1,    -1,    -1,    -1,    -1,    -1,
     462,    -1,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1263,    -1,    -1,    -1,   507,    -1,    -1,    -1,   511,
      -1,    -1,    -1,  2143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1288,    -1,  2158,    -1,
      -1,   778,   779,    -1,    -1,    -1,    -1,   784,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2175,    -1,    -1,    -1,   551,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,
      -1,   808,   809,    -1,   811,    -1,   813,   814,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     602,    -1,    -1,   605,    -1,    -1,    -1,    -1,   855,    -1,
      -1,    -1,   859,    -1,    -1,    -1,   863,    -1,    -1,    -1,
    1382,   623,   624,    -1,  1386,    -1,    -1,    -1,  1390,    -1,
      -1,    -1,   634,    -1,    -1,    -1,   638,    -1,    -1,    -1,
      -1,    -1,    -1,   645,    -1,   647,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   933,   934,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
     947,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,  1489,    -1,   132,
    1492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   146,   147,   148,    -1,    -1,  1510,   152,
     153,    -1,   155,   156,    -1,    -1,   758,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   774,   775,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,   784,   785,    -1,   787,   788,    -1,    -1,    -1,
      -1,  1184,    -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,
     802,  1563,   804,   805,    -1,    -1,    -1,    -1,    -1,   811,
    1572,    -1,    -1,    -1,  1576,    -1,    -1,    -1,    -1,   821,
     822,    -1,    -1,    84,    -1,    -1,    -1,    -1,  1590,  1591,
      -1,  1078,    -1,    -1,    -1,    -1,    -1,    -1,   840,   100,
      -1,    -1,   844,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,   855,   856,    -1,  1618,   859,   860,    -1,
      -1,   863,   864,    -1,    -1,    -1,    -1,    -1,    -1,   871,
      -1,    -1,  1119,    -1,    -1,    -1,    -1,    -1,    -1,  1126,
      -1,    -1,  1129,    -1,    -1,    -1,  1133,    -1,    -1,   150,
      -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   918,   919,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   203,    -1,    -1,   206,    -1,   949,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1722,  1723,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   988,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,   259,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1438,    -1,   273,  1441,  1442,    -1,    -1,    -1,    -1,  1447,
      -1,    -1,    -1,  1451,  1271,  1453,    -1,    -1,    -1,    -1,
      -1,    -1,  1279,  1280,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   313,  1055,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   323,    -1,    -1,    -1,    -1,    -1,   121,    -1,
    1072,  1073,    -1,    -1,    -1,    -1,    -1,  1079,    -1,  1472,
    1473,   134,   343,   136,   345,    -1,    -1,    -1,    -1,    -1,
      -1,  1338,    -1,    -1,    -1,    -1,    -1,  1859,    -1,    -1,
    1347,    -1,    -1,  1350,    -1,  1352,  1353,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1121,
      -1,    -1,    -1,    -1,  1126,  1127,    -1,  1129,  1130,    -1,
    1892,  1133,  1134,    -1,    -1,    -1,    -1,   398,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1911,
      -1,  1398,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1941,
      -1,    -1,  1610,    -1,   237,   238,    -1,    -1,   241,    -1,
      -1,   244,   245,    -1,   247,    -1,   249,    -1,    -1,    -1,
      -1,   462,    -1,    -1,    -1,    -1,  1968,   468,    -1,  1971,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1660,    -1,    -1,    -1,  1483,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1675,  1676,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1265,  1657,    -1,    -1,    -1,    -1,  1271,
    1272,    -1,  1700,    -1,    -1,    -1,    -1,  1670,  1706,    -1,
      -1,    -1,    -1,    -1,   545,   546,    -1,   340,   341,    -1,
     551,    -1,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2080,    -1,
      -1,    -1,  1715,    -1,  1571,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1338,  1339,    -1,    -1,
      -1,    -1,    -1,    -1,   605,  1347,  1348,    -1,  1350,    -1,
      -1,    -1,    -1,  1600,    -1,    -1,    -1,    -1,    -1,  1361,
    1362,    -1,    -1,   624,    -1,   626,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   647,    -1,  1635,    -1,
      -1,    -1,    -1,    -1,  1641,    -1,    -1,    -1,   451,    -1,
    1828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,
     671,  1839,    -1,    -1,  1842,  1843,    -1,  1845,    -1,    -1,
      -1,    -1,  1850,   684,    -1,    -1,   687,   688,    -1,   690,
      -1,    -1,    -1,  1826,    -1,    -1,    -1,  1830,   699,    -1,
      -1,   702,   703,   704,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1844,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1854,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   522,
    1717,    -1,    -1,    -1,  1867,    -1,  1869,  1870,  1871,  1872,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   758,    -1,    -1,
      -1,    -1,  1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1531,
    1958,   203,    -1,    -1,    -1,  1963,  1964,    -1,   799,    -1,
      -1,    -1,    -1,    -1,   216,    -1,   218,    -1,    -1,    -1,
    1943,    -1,    -1,    -1,  1947,  1983,    -1,  1804,  1805,  1952,
     821,   822,    -1,    -1,    -1,  1812,    -1,    -1,    -1,  1816,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,
      -1,    -1,    -1,    -1,  1977,    -1,    -1,    -1,    -1,   642,
     643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   655,    -1,  2032,    -1,  2034,    -1,    -1,  2037,
    2038,    -1,    -1,    -1,  2042,  2043,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1625,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2027,    -1,    -1,    -1,  2031,  1641,
     312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2044,    -1,    -1,    -1,    -1,    -1,   918,    -1,    -1,
      -1,    -1,    -1,    -1,   925,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,  1930,  2112,  2113,  2114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2094,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,
    2148,  2149,  2150,    -1,    -1,   778,   779,   988,    -1,    -1,
      -1,   784,    -1,    -1,    -1,    -1,    -1,  2130,    -1,    -1,
    2133,    -1,  2135,    -1,    -1,    -1,    -1,    -1,    -1,  2142,
      -1,    -1,   805,    -1,    -1,   808,   809,    -1,   811,    -1,
     813,   814,    -1,  2010,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1777,  1778,    -1,  2171,    -1,
    2173,    -1,    -1,    -1,  2177,    -1,  2179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1797,  1798,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,   859,    -1,    -1,  2202,
     863,  1813,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,
      -1,    -1,   494,    -1,    -1,    -1,    -1,   499,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     933,   934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   947,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   595,    -1,    -1,  1928,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1936,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   624,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1234,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1255,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2005,  1265,    -1,    -1,    -1,  2010,  2011,
      -1,    -1,  2014,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     692,    -1,    -1,    -1,    -1,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1294,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,    -1,   716,   717,    -1,    -1,   720,    -1,
     722,    -1,    -1,  2055,    -1,    -1,   728,    -1,   730,   731,
      -1,    -1,    -1,    -1,    -1,    -1,  1119,    -1,    -1,    -1,
      -1,    -1,    -1,  1126,    -1,    -1,  1129,    -1,    -1,    -1,
    1133,    -1,    -1,    -1,    -1,    -1,   758,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   771,
    1361,  1362,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     782,    -1,    -1,    -1,    -1,    -1,    -1,  2119,  2120,    -1,
      -1,    -1,    -1,    -1,   796,    -1,    -1,   799,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1411,  1412,  1413,    -1,   826,  1416,  1417,   829,    -1,    -1,
    2162,    -1,  1423,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
     872,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,  1271,    -1,
      -1,  1482,    72,    -1,    -1,    -1,  1279,  1280,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   925,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   938,   939,    -1,    -1,
      -1,    -1,    -1,    -1,   946,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,  1338,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1347,    -1,    -1,  1350,    -1,  1352,
    1353,     1,   974,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   988,    -1,    18,    -1,
      -1,    -1,    -1,    -1,   996,    -1,    -1,    -1,    -1,    -1,
      -1,  1003,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1398,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1631,    71,    -1,    73,    74,    -1,    76,  1049,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,  1664,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
    1483,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1116,    -1,  1118,    -1,  1120,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,  1718,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1777,  1778,  1571,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1199,  1200,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
      -1,  1832,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1269,    -1,    -1,
      -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,  1311,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1326,    -1,    -1,  1329,    -1,    -1,
      -1,    -1,   101,   102,  1717,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1381,
      -1,  1972,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
    1402,  1403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2005,    -1,    -1,    -1,    -1,    -1,
      -1,  1804,  1805,    -1,    -1,    -1,    -1,    -1,    -1,  1431,
      -1,    -1,    -1,  1816,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1449,    -1,    -1,
    1452,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2053,    -1,    -1,  2056,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,  1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1514,  1515,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1524,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
    1542,    -1,  1544,    -1,    -1,    -1,    -1,  1930,    -1,    -1,
      -1,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2010,    -1,  1631,
      -1,   151,   152,    -1,  1636,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    13,    14,
      15,    16,    17,    -1,    -1,    20,    18,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,  1695,    -1,    -1,    51,    49,    53,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    71,
      -1,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    -1,    99,  1750,   101,
     102,  1753,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1790,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    -1,    99,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,    -1,  1980,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      71,    72,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
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
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,   147,   148,    -1,    -1,    -1,   152,   153,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,     3,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,    -1,    -1,   146,   147,   148,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,   101,   102,   103,   104,   105,   106,   107,   108,   109,
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
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,     5,
      72,    -1,    -1,    -1,    -1,    77,    78,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     3,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    13,    14,
      15,    16,    17,   163,   164,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,
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
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
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
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    71,
      -1,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    18,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    49,   155,   156,    52,    -1,    54,   160,    56,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,   177,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    -1,    -1,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    90,    -1,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
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
      -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,
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
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
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
     166,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,   159,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     106,   107,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,   155,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,    -1,   163,   164,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,   154,   155,
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
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,   106,   107,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,   155,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    49,    -1,    -1,    52,    -1,    54,   132,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   146,   147,   148,    -1,    73,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
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
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,   159,    -1,    -1,
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
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,
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
      -1,   104,    -1,   106,   107,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    20,    72,    22,
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
     177,   392,   419,   424,   217,   309,   158,   177,   353,   354,
     353,   353,   373,   134,   370,   371,   154,   154,   152,   226,
     154,   224,   309,   146,   147,   148,   168,   177,   247,   248,
     226,   225,   177,   248,   154,   159,   224,   153,   224,   225,
     246,   177,   485,   154,   154,   154,   228,   270,   271,   152,
     217,   152,   185,   235,   201,   255,   224,    75,   108,   256,
     258,    75,   256,     1,   226,   418,   398,   179,   179,   360,
     360,   157,   366,   180,   180,   157,   157,   151,   358,   160,
     485,   151,   180,   154,   217,   189,   217,   485,   151,   157,
     157,   194,   194,   366,   154,   154,   366,   366,   154,   154,
     157,   158,   134,   365,   134,   157,   180,   180,   180,   154,
     154,   154,   157,   217,   177,   471,   472,   473,   309,   470,
     158,   177,   418,   418,   177,   154,   424,   418,   226,    77,
      78,   160,   238,   239,   240,   154,   224,    75,   226,   224,
     153,   224,    75,   177,   106,   153,   224,   225,   246,   153,
     224,   226,   245,   248,   248,   177,   224,   151,   160,   240,
     226,   152,   179,   177,   185,   154,   159,   154,   154,   158,
     159,   154,   226,   152,   226,   226,   226,   226,   373,   415,
     485,   485,   180,   157,   157,   160,   360,   151,   151,   151,
     157,   157,   180,   180,   180,   179,   180,   154,   154,   154,
     154,   154,   154,   470,   418,   348,     1,   216,   236,   237,
     416,     1,   159,     1,   179,   226,   238,    75,   177,   154,
     226,    75,   177,   168,   168,   226,   225,   248,   248,   177,
     106,   224,   168,   168,    75,   153,   224,   153,   224,   225,
     177,     1,   179,   179,   272,   307,   309,   479,   159,   177,
     156,   185,   277,   278,   279,   226,   201,   191,   224,   257,
     257,   151,   151,   154,   360,   485,   154,   154,   154,   368,
     152,   418,   459,   462,   350,   134,     1,   158,   159,   151,
     282,   283,   289,   226,    75,   177,   226,   224,   153,   153,
     224,   153,   224,   153,   224,   225,   153,   224,   153,   224,
     226,   168,   168,   168,   168,   151,   282,   272,   180,   152,
     199,   415,   470,   183,   159,   104,   152,   154,   159,   158,
      75,   154,   154,    75,   253,    75,   253,   485,   151,   179,
     216,   236,   239,   241,   242,   289,   226,   168,   168,   168,
     168,   153,   153,   224,   153,   224,   153,   224,   241,   180,
     177,   269,   309,   277,   157,   216,   177,   277,   279,   226,
     226,    75,   226,    75,   151,   366,   226,   231,   180,   239,
     153,   153,   224,   153,   224,   153,   224,   180,   269,   215,
     154,   159,   185,   154,   154,   159,   226,   226,   180,     1,
     226,   151,   231,   151,   154,   228,   185,   280,   152,   177,
     280,   154,   228,   158,   159,   216,   154,   185,   183,   281,
     154,   177,   154,   158,   177,   183
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
     346,   346,   347,   347,   347,   348,   348,   348,   348,   349,
     349,   349,   350,   351,   351,   352,   352,   353,   353,   354,
     355,   356,   355,   355,   355,   355,   357,   355,   355,   355,
     355,   355,   358,   358,   359,   359,   360,   360,   360,   360,
     361,   361,   362,   362,   362,   363,   363,   363,   363,   363,
     363,   363,   364,   364,   364,   364,   365,   365,   366,   366,
     366,   366,   367,   367,   367,   367,   368,   368,   368,   368,
     368,   369,   369,   369,   369,   369,   370,   370,   371,   371,
     372,   372,   373,   373,   373,   374,   374,   374,   375,   375,
     376,   376,   376,   376,   377,   377,   378,   378,   378,   378,
     378,   379,   379,   380,   380,   381,   381,   381,   381,   381,
     382,   382,   383,   383,   385,   384,   386,   384,   384,   384,
     387,   387,   387,   387,   388,   388,   388,   388,   389,   389,
     390,   390,   391,   391,   392,   392,   392,   392,   393,   393,
     393,   394,   394,   395,   395,   396,   396,   396,   396,   397,
     397,   398,   398,   399,   399,   399,   400,   400,   401,   401,
     402,   402,   403,   403,   404,   405,   406,   406,   406,   406,
     406,   406,   406,   406,   406,   406,   406,   407,   406,   408,
     406,   409,   406,   410,   406,   411,   406,   412,   412,   412,
     413,   413,   414,   414,   414,   414,   414,   414,   414,   414,
     414,   414,   415,   415,   415,   415,   416,   417,   417,   418,
     418,   419,   419,   420,   421,   421,   422,   422,   422,   423,
     423,   423,   423,   423,   423,   424,   424,   425,   425,   425,
     425,   426,   426,   426,   426,   427,   427,   427,   427,   427,
     427,   427,   428,   428,   428,   428,   429,   429,   429,   430,
     430,   430,   430,   430,   431,   431,   431,   431,   432,   432,
     432,   432,   432,   432,   433,   433,   433,   434,   434,   434,
     434,   434,   435,   435,   435,   435,   436,   436,   436,   436,
     436,   436,   437,   437,   438,   438,   438,   438,   439,   439,
     439,   439,   440,   440,   440,   440,   440,   440,   440,   441,
     441,   441,   441,   442,   442,   442,   443,   443,   443,   443,
     443,   444,   444,   444,   444,   445,   445,   445,   445,   445,
     445,   446,   446,   446,   446,   446,   447,   447,   447,   448,
     448,   448,   448,   449,   449,   449,   450,   450,   450,   450,
     450,   451,   451,   452,   452,   452,   453,   453,   454,   454,
     455,   455,   455,   456,   456,   456,   456,   456,   457,   457,
     457,   457,   458,   458,   458,   459,   459,   459,   459,   459,
     460,   460,   460,   460,   460,   460,   461,   461,   462,   462,
     462,   462,   463,   463,   464,   464,   464,   464,   465,   465,
     465,   465,   465,   466,   466,   466,   466,   467,   467,   467,
     468,   468,   468,   469,   469,   469,   469,   469,   469,   470,
     470,   470,   471,   471,   471,   471,   471,   472,   472,   472,
     472,   473,   473,   474,   474,   474,   475,   475,   476,   476,
     476,   476,   476,   476,   477,   477,   477,   477,   477,   477,
     477,   477,   477,   477,   478,   478,   478,   478,   479,   479,
     479,   480,   480,   481,   481,   481,   481,   481,   481,   482,
     482,   482,   482,   482,   482,   483,   483,   483,   484,   484,
     485,   485,   486,   486
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
       2,     3,     4,     4,     4,     3,     2,     2,     3,     3,
       2,     1,     0,     1,     4,     1,     2,     2,     2,     0,
       1,     4,     1,     2,     3,     1,     2,     0,     1,     2,
       6,     0,     9,     8,     9,     8,     0,    13,    11,    12,
      11,     1,     0,     1,     3,     3,     3,     2,     5,     5,
       1,     1,     0,     2,     5,     0,     1,     1,     1,     5,
       5,     5,     1,     5,     5,     9,     1,     5,     0,     1,
       1,     5,     1,     1,     5,     5,     1,     3,     3,     4,
       1,     1,     1,     1,     2,     1,     3,     3,     2,     3,
       1,     3,     1,     1,     1,     1,     1,     2,     1,     1,
       0,     2,     2,     4,     1,     4,     0,     1,     2,     3,
       4,     2,     2,     1,     2,     2,     5,     5,     7,     6,
       1,     3,     0,     2,     0,     5,     0,     5,     3,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       1,     2,     5,     6,     1,     1,     3,     3,     2,     3,
       3,     2,     4,     1,     4,     7,     5,    10,     8,     1,
       4,     2,     2,     1,     1,     5,     2,     5,     0,     1,
       3,     4,     0,     1,     0,     0,     1,     1,     2,     2,
       2,     2,     2,     2,     1,     2,     5,     0,     6,     0,
       8,     0,     7,     0,     7,     0,     8,     1,     2,     3,
       0,     5,     3,     4,     4,     4,     4,     5,     5,     5,
       5,     6,     1,     1,     1,     1,     3,     0,     5,     0,
       1,     1,     2,     6,     1,     3,     0,     1,     4,     1,
       1,     1,     1,     1,     1,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     8,     9,     3,     4,     2,     1,     2,     6,
       8,     9,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     1,     1,     1,     4,     8,     9,
       3,     4,     2,     3,     3,     4,     4,     5,     4,     5,
       3,     4,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     8,
       9,     3,     4,     2,     1,     2,     6,     8,     9,     3,
       4,     2,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     2,     4,     1,     2,     2,     2,     3,     4,     2,
       4,     4,     3,     6,     8,     3,     2,     4,     1,     2,
       2,     1,     1,     2,     3,     4,     2,     4,     6,     8,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     5,     8,     3,     2,     3,     7,     5,     1,
       1,     1,     3,     3,     3,     5,     1,     1,     5,     5,
       6,     6,     0,     1,     1,     3,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     5,     8,     3,
       1,     2,     1,     2,     6,     5,     6,     7,     7,     1,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     8,     3,     1,     1,     2,     1,     1,     2,     3,
       2,     3,     2,     3,     3,     2,     4,     3,     2,     3,
       2,     4,     3,     2,     6,     6,     6,     7,     1,     2,
       1,     1,     1,     2,     3,     2,     3,     2,     3,     3,
       4,     2,     3,     4,     2,     5,     6,     7,     6,     6,
       0,     1,     0,     2
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
#line 605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7988 "Parser/parser.cc"
    break;

  case 3:
#line 609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7994 "Parser/parser.cc"
    break;

  case 4:
#line 616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 8000 "Parser/parser.cc"
    break;

  case 5:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8006 "Parser/parser.cc"
    break;

  case 6:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8012 "Parser/parser.cc"
    break;

  case 7:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8018 "Parser/parser.cc"
    break;

  case 8:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8024 "Parser/parser.cc"
    break;

  case 20:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8030 "Parser/parser.cc"
    break;

  case 21:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = new ExpressionNode( build_constantStr( yylloc, *(yyvsp[0].str) ) ); }
#line 8036 "Parser/parser.cc"
    break;

  case 22:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8042 "Parser/parser.cc"
    break;

  case 23:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8052 "Parser/parser.cc"
    break;

  case 24:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8058 "Parser/parser.cc"
    break;

  case 25:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8064 "Parser/parser.cc"
    break;

  case 26:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8070 "Parser/parser.cc"
    break;

  case 28:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 8076 "Parser/parser.cc"
    break;

  case 29:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].stmt) ) ) ) ); }
#line 8082 "Parser/parser.cc"
    break;

  case 30:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8088 "Parser/parser.cc"
    break;

  case 31:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8094 "Parser/parser.cc"
    break;

  case 32:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].expr) );
			(yyval.expr) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8104 "Parser/parser.cc"
    break;

  case 33:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.expr) = nullptr; }
#line 8110 "Parser/parser.cc"
    break;

  case 34:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.expr) = nullptr; }
#line 8116 "Parser/parser.cc"
    break;

  case 35:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.expr) = nullptr; }
#line 8122 "Parser/parser.cc"
    break;

  case 36:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8128 "Parser/parser.cc"
    break;

  case 37:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8134 "Parser/parser.cc"
    break;

  case 38:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.expr) = nullptr; }
#line 8140 "Parser/parser.cc"
    break;

  case 40:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8152 "Parser/parser.cc"
    break;

  case 41:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].expr) ) } } );
		}
#line 8161 "Parser/parser.cc"
    break;

  case 42:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].expr) ) } } ); }
#line 8167 "Parser/parser.cc"
    break;

  case 44:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].expr), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) ) ) )) ) ); }
#line 8173 "Parser/parser.cc"
    break;

  case 45:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8179 "Parser/parser.cc"
    break;

  case 46:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8185 "Parser/parser.cc"
    break;

  case 47:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8191 "Parser/parser.cc"
    break;

  case 48:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.expr) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) ) );
		}
#line 8201 "Parser/parser.cc"
    break;

  case 49:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 50:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].expr)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8214 "Parser/parser.cc"
    break;

  case 51:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8220 "Parser/parser.cc"
    break;

  case 52:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8226 "Parser/parser.cc"
    break;

  case 53:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].expr) ) ); }
#line 8232 "Parser/parser.cc"
    break;

  case 54:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8238 "Parser/parser.cc"
    break;

  case 55:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8244 "Parser/parser.cc"
    break;

  case 56:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8250 "Parser/parser.cc"
    break;

  case 57:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8256 "Parser/parser.cc"
    break;

  case 58:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].expr) ) ); }
#line 8262 "Parser/parser.cc"
    break;

  case 59:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8268 "Parser/parser.cc"
    break;

  case 60:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8274 "Parser/parser.cc"
    break;

  case 61:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8280 "Parser/parser.cc"
    break;

  case 62:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].expr) ) ); }
#line 8286 "Parser/parser.cc"
    break;

  case 63:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].expr) ) ); }
#line 8292 "Parser/parser.cc"
    break;

  case 64:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].init), true ) ) ); }
#line 8298 "Parser/parser.cc"
    break;

  case 65:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].init), true ))->set_maybeConstructed( false ) ) ); }
#line 8304 "Parser/parser.cc"
    break;

  case 66:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.expr) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].expr) )->set_last( (yyvsp[-1].expr) ) ) );
		}
#line 8314 "Parser/parser.cc"
    break;

  case 67:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8320 "Parser/parser.cc"
    break;

  case 70:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8326 "Parser/parser.cc"
    break;

  case 71:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8332 "Parser/parser.cc"
    break;

  case 74:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8338 "Parser/parser.cc"
    break;

  case 76:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8344 "Parser/parser.cc"
    break;

  case 77:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8350 "Parser/parser.cc"
    break;

  case 78:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8356 "Parser/parser.cc"
    break;

  case 79:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8362 "Parser/parser.cc"
    break;

  case 80:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].expr), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8368 "Parser/parser.cc"
    break;

  case 81:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].expr), build_tuple( yylloc, (yyvsp[-1].expr) ) ) ); }
#line 8374 "Parser/parser.cc"
    break;

  case 82:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8380 "Parser/parser.cc"
    break;

  case 83:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].expr) ) ); }
#line 8386 "Parser/parser.cc"
    break;

  case 84:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].expr) ) );
		}
#line 8394 "Parser/parser.cc"
    break;

  case 85:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8400 "Parser/parser.cc"
    break;

  case 86:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.expr) = (yyvsp[-1].expr) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].expr), constant ) ) : new ExpressionNode( constant );
		}
#line 8409 "Parser/parser.cc"
    break;

  case 89:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 8415 "Parser/parser.cc"
    break;

  case 90:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr)->set_extension( true ); }
#line 8421 "Parser/parser.cc"
    break;

  case 91:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8441 "Parser/parser.cc"
    break;

  case 92:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].oper), (yyvsp[0].expr) ) ); }
#line 8447 "Parser/parser.cc"
    break;

  case 93:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].expr) ) ); }
#line 8453 "Parser/parser.cc"
    break;

  case 94:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].expr) ) ); }
#line 8459 "Parser/parser.cc"
    break;

  case 95:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8465 "Parser/parser.cc"
    break;

  case 96:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8471 "Parser/parser.cc"
    break;

  case 97:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8477 "Parser/parser.cc"
    break;

  case 98:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8483 "Parser/parser.cc"
    break;

  case 99:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8489 "Parser/parser.cc"
    break;

  case 100:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.expr) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8498 "Parser/parser.cc"
    break;

  case 101:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::PointTo; }
#line 8504 "Parser/parser.cc"
    break;

  case 102:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AddressOf; }
#line 8510 "Parser/parser.cc"
    break;

  case 103:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::And; }
#line 8516 "Parser/parser.cc"
    break;

  case 104:
#line 912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::UnPlus; }
#line 8522 "Parser/parser.cc"
    break;

  case 105:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::UnMinus; }
#line 8528 "Parser/parser.cc"
    break;

  case 106:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::Neg; }
#line 8534 "Parser/parser.cc"
    break;

  case 107:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::BitNeg; }
#line 8540 "Parser/parser.cc"
    break;

  case 109:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].expr) ) ); }
#line 8546 "Parser/parser.cc"
    break;

  case 110:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 111:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].expr) ) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 112:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8564 "Parser/parser.cc"
    break;

  case 113:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].expr) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 114:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8576 "Parser/parser.cc"
    break;

  case 115:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8582 "Parser/parser.cc"
    break;

  case 116:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8588 "Parser/parser.cc"
    break;

  case 124:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8594 "Parser/parser.cc"
    break;

  case 126:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8600 "Parser/parser.cc"
    break;

  case 127:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 128:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8612 "Parser/parser.cc"
    break;

  case 130:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8618 "Parser/parser.cc"
    break;

  case 131:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8624 "Parser/parser.cc"
    break;

  case 133:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 134:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 136:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8642 "Parser/parser.cc"
    break;

  case 137:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8648 "Parser/parser.cc"
    break;

  case 138:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8654 "Parser/parser.cc"
    break;

  case 139:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8660 "Parser/parser.cc"
    break;

  case 141:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8666 "Parser/parser.cc"
    break;

  case 142:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8672 "Parser/parser.cc"
    break;

  case 144:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8678 "Parser/parser.cc"
    break;

  case 146:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8684 "Parser/parser.cc"
    break;

  case 148:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8690 "Parser/parser.cc"
    break;

  case 150:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::AndExpr ) ); }
#line 8696 "Parser/parser.cc"
    break;

  case 152:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].expr), (yyvsp[0].expr), ast::OrExpr ) ); }
#line 8702 "Parser/parser.cc"
    break;

  case 154:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].expr) ) ); }
#line 8708 "Parser/parser.cc"
    break;

  case 155:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].expr), (yyvsp[-3].expr), (yyvsp[0].expr) ) ); }
#line 8714 "Parser/parser.cc"
    break;

  case 158:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.expr) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].oper), (yyvsp[-2].expr), (yyvsp[0].expr) ) );
//			} // if
		}
#line 8726 "Parser/parser.cc"
    break;

  case 159:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8732 "Parser/parser.cc"
    break;

  case 160:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8738 "Parser/parser.cc"
    break;

  case 164:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.oper) = OperKinds::Assign; }
#line 8744 "Parser/parser.cc"
    break;

  case 165:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AtAssn; }
#line 8750 "Parser/parser.cc"
    break;

  case 166:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ExpAssn; }
#line 8756 "Parser/parser.cc"
    break;

  case 167:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MulAssn; }
#line 8762 "Parser/parser.cc"
    break;

  case 168:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::DivAssn; }
#line 8768 "Parser/parser.cc"
    break;

  case 169:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ModAssn; }
#line 8774 "Parser/parser.cc"
    break;

  case 170:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::PlusAssn; }
#line 8780 "Parser/parser.cc"
    break;

  case 171:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.oper) = OperKinds::MinusAssn; }
#line 8786 "Parser/parser.cc"
    break;

  case 172:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::LSAssn; }
#line 8792 "Parser/parser.cc"
    break;

  case 173:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::RSAssn; }
#line 8798 "Parser/parser.cc"
    break;

  case 174:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::AndAssn; }
#line 8804 "Parser/parser.cc"
    break;

  case 175:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::ERAssn; }
#line 8810 "Parser/parser.cc"
    break;

  case 176:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.oper) = OperKinds::OrAssn; }
#line 8816 "Parser/parser.cc"
    break;

  case 177:
#line 1100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].expr) ) ) ); }
#line 8822 "Parser/parser.cc"
    break;

  case 178:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].expr)->set_last( (yyvsp[-1].expr) ) ) )); }
#line 8828 "Parser/parser.cc"
    break;

  case 180:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8834 "Parser/parser.cc"
    break;

  case 181:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 8840 "Parser/parser.cc"
    break;

  case 182:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 8846 "Parser/parser.cc"
    break;

  case 184:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 8852 "Parser/parser.cc"
    break;

  case 185:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 8858 "Parser/parser.cc"
    break;

  case 198:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 8864 "Parser/parser.cc"
    break;

  case 200:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8870 "Parser/parser.cc"
    break;

  case 201:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[0].stmt)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8876 "Parser/parser.cc"
    break;

  case 202:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.stmt) = nullptr;
		}
#line 8887 "Parser/parser.cc"
    break;

  case 203:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 204:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, (yyvsp[-2].stmt) ) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 206:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8905 "Parser/parser.cc"
    break;

  case 207:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 208:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 209:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8923 "Parser/parser.cc"
    break;

  case 210:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.stmt) = new StatementNode( (yyvsp[0].decl) ); }
#line 8929 "Parser/parser.cc"
    break;

  case 213:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].stmt) ); (yyvsp[-1].stmt)->set_last( (yyvsp[0].stmt) ); (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8935 "Parser/parser.cc"
    break;

  case 214:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.stmt) = nullptr; }
#line 8941 "Parser/parser.cc"
    break;

  case 215:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_expr( yylloc, (yyvsp[-1].expr) ) ); }
#line 8947 "Parser/parser.cc"
    break;

  case 216:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = (yyvsp[-1].stmt); }
#line 8953 "Parser/parser.cc"
    break;

  case 217:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8959 "Parser/parser.cc"
    break;

  case 218:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8973 "Parser/parser.cc"
    break;

  case 219:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 8979 "Parser/parser.cc"
    break;

  case 220:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].expr), (yyvsp[0].clause) ) ); }
#line 8985 "Parser/parser.cc"
    break;

  case 221:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].expr), (yyvsp[-2].clause) ) );
			(yyval.stmt) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8994 "Parser/parser.cc"
    break;

  case 222:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.stmt) = nullptr; }
#line 9000 "Parser/parser.cc"
    break;

  case 223:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ), nullptr ) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 224:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 225:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].expr) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 226:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9024 "Parser/parser.cc"
    break;

  case 227:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9030 "Parser/parser.cc"
    break;

  case 228:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].expr) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 229:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9042 "Parser/parser.cc"
    break;

  case 230:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 232:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.clause) = new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 233:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-2].clause)->set_last( new ClauseNode( build_case( yylloc, (yyvsp[0].expr) ) ) ); }
#line 9060 "Parser/parser.cc"
    break;

  case 234:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.clause) = nullptr; }
#line 9066 "Parser/parser.cc"
    break;

  case 235:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause); }
#line 9072 "Parser/parser.cc"
    break;

  case 236:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.clause) = nullptr; }
#line 9078 "Parser/parser.cc"
    break;

  case 237:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.clause) = new ClauseNode( build_default( yylloc ) ); }
#line 9084 "Parser/parser.cc"
    break;

  case 238:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.clause) = nullptr; }
#line 9090 "Parser/parser.cc"
    break;

  case 240:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.clause) = (yyvsp[-1].clause)->set_last( (yyvsp[0].clause) ); }
#line 9096 "Parser/parser.cc"
    break;

  case 241:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 242:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = nullptr; }
#line 9108 "Parser/parser.cc"
    break;

  case 244:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9114 "Parser/parser.cc"
    break;

  case 245:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-2].clause)->set_last( (yyvsp[-1].clause)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].stmt) ) ) ) ); }
#line 9120 "Parser/parser.cc"
    break;

  case 246:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9126 "Parser/parser.cc"
    break;

  case 247:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9135 "Parser/parser.cc"
    break;

  case 248:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 249:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 250:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ) ); }
#line 9153 "Parser/parser.cc"
    break;

  case 251:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9162 "Parser/parser.cc"
    break;

  case 252:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[-5].stmt) ) ) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 253:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].expr), maybe_build_compound( yylloc, (yyvsp[-6].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9174 "Parser/parser.cc"
    break;

  case 254:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9180 "Parser/parser.cc"
    break;

  case 255:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.stmt) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9189 "Parser/parser.cc"
    break;

  case 256:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-2].forctl), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ) ); }
#line 9195 "Parser/parser.cc"
    break;

  case 257:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_for( yylloc, (yyvsp[-4].forctl), maybe_build_compound( yylloc, (yyvsp[-2].stmt) ), (yyvsp[0].stmt) ) ); }
#line 9201 "Parser/parser.cc"
    break;

  case 259:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9220 "Parser/parser.cc"
    break;

  case 260:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9226 "Parser/parser.cc"
    break;

  case 261:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].expr) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ) ) ) : nullptr;
			(yyval.forctl) = new ForCtrl( init, (yyvsp[-2].expr), (yyvsp[0].expr) );
		}
#line 9235 "Parser/parser.cc"
    break;

  case 262:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9241 "Parser/parser.cc"
    break;

  case 263:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[0].expr), nullptr ); }
#line 9247 "Parser/parser.cc"
    break;

  case 264:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = new ForCtrl( nullptr, (yyvsp[-2].expr), (yyvsp[0].expr) ); }
#line 9253 "Parser/parser.cc"
    break;

  case 265:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9259 "Parser/parser.cc"
    break;

  case 266:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9265 "Parser/parser.cc"
    break;

  case 267:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9271 "Parser/parser.cc"
    break;

  case 268:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9280 "Parser/parser.cc"
    break;

  case 269:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9289 "Parser/parser.cc"
    break;

  case 270:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9295 "Parser/parser.cc"
    break;

  case 271:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9304 "Parser/parser.cc"
    break;

  case 272:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
		}
#line 9313 "Parser/parser.cc"
    break;

  case 273:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9319 "Parser/parser.cc"
    break;

  case 274:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9325 "Parser/parser.cc"
    break;

  case 275:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9331 "Parser/parser.cc"
    break;

  case 276:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9337 "Parser/parser.cc"
    break;

  case 277:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.forctl) = nullptr; }
#line 9343 "Parser/parser.cc"
    break;

  case 278:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-2].expr), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr)->clone(), NEW_ONE ); }
#line 9349 "Parser/parser.cc"
    break;

  case 279:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-3].expr), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr)->clone() ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9355 "Parser/parser.cc"
    break;

  case 280:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9361 "Parser/parser.cc"
    break;

  case 281:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[0].expr), (yyvsp[-4].expr), (yyvsp[0].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9370 "Parser/parser.cc"
    break;

  case 282:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-4].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9380 "Parser/parser.cc"
    break;

  case 283:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9386 "Parser/parser.cc"
    break;

  case 284:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9392 "Parser/parser.cc"
    break;

  case 285:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9401 "Parser/parser.cc"
    break;

  case 286:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9411 "Parser/parser.cc"
    break;

  case 287:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr)->clone(), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9417 "Parser/parser.cc"
    break;

  case 288:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].expr), (yyvsp[-6].expr), (yyvsp[-2].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9426 "Parser/parser.cc"
    break;

  case 289:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-4].expr), (yyvsp[-6].expr), (yyvsp[-4].expr)->clone(), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9436 "Parser/parser.cc"
    break;

  case 290:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9442 "Parser/parser.cc"
    break;

  case 291:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].expr), NEW_ONE ); }
#line 9448 "Parser/parser.cc"
    break;

  case 292:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].oper), NEW_ZERO, (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9454 "Parser/parser.cc"
    break;

  case 293:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].oper), (yyvsp[-2].expr)->clone(), (yyvsp[0].expr) ), (yyvsp[-1].oper), UPDOWN( (yyvsp[-1].oper), (yyvsp[0].expr)->clone(), (yyvsp[-2].expr)->clone() ), NEW_ONE ); }
#line 9460 "Parser/parser.cc"
    break;

  case 294:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LThan || (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9469 "Parser/parser.cc"
    break;

  case 295:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::GThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-1].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].expr), (yyvsp[-1].oper), nullptr, NEW_ONE );
		}
#line 9479 "Parser/parser.cc"
    break;

  case 296:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), (yyvsp[0].expr) ); }
#line 9485 "Parser/parser.cc"
    break;

  case 297:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9494 "Parser/parser.cc"
    break;

  case 298:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, (yyvsp[0].expr) );
		}
#line 9504 "Parser/parser.cc"
    break;

  case 299:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].oper), (yyvsp[-4].expr), (yyvsp[-2].expr) ), (yyvsp[-3].oper), UPDOWN( (yyvsp[-3].oper), (yyvsp[-2].expr)->clone(), (yyvsp[-4].expr)->clone() ), nullptr ); }
#line 9510 "Parser/parser.cc"
    break;

  case 300:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::LThan || (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9519 "Parser/parser.cc"
    break;

  case 301:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].oper) == OperKinds::GThan || (yyvsp[-3].oper) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.forctl) = nullptr; }
			else if ( (yyvsp[-3].oper) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.forctl) = nullptr; }
			else (yyval.forctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].expr), (yyvsp[-3].oper), nullptr, nullptr );
		}
#line 9529 "Parser/parser.cc"
    break;

  case 302:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.forctl) = nullptr; }
#line 9535 "Parser/parser.cc"
    break;

  case 303:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9544 "Parser/parser.cc"
    break;

  case 304:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].oper) == OperKinds::LEThan || (yyvsp[-1].oper) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.forctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.forctl) = nullptr;
		}
#line 9553 "Parser/parser.cc"
    break;

  case 305:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9559 "Parser/parser.cc"
    break;

  case 306:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9565 "Parser/parser.cc"
    break;

  case 307:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9571 "Parser/parser.cc"
    break;

  case 308:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 9577 "Parser/parser.cc"
    break;

  case 309:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GThan; }
#line 9583 "Parser/parser.cc"
    break;

  case 311:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 9589 "Parser/parser.cc"
    break;

  case 312:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::GEThan; }
#line 9595 "Parser/parser.cc"
    break;

  case 313:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 314:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_computedgoto( (yyvsp[-1].expr) ) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 315:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 316:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9619 "Parser/parser.cc"
    break;

  case 317:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 318:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 319:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 320:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 321:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9649 "Parser/parser.cc"
    break;

  case 322:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_return( yylloc, (yyvsp[-1].expr) ) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 323:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.stmt) = nullptr; }
#line 9661 "Parser/parser.cc"
    break;

  case 324:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 325:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::None ) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 326:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 327:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Coroutine ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 328:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 329:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_suspend( yylloc, (yyvsp[0].stmt), ast::SuspendStmt::Generator ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 330:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_throw( yylloc, (yyvsp[-1].expr) ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 331:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume( yylloc, (yyvsp[-1].expr) ) ); }
#line 9709 "Parser/parser.cc"
    break;

  case 332:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_resume_at( (yyvsp[-3].expr), (yyvsp[-1].expr) ) ); }
#line 9715 "Parser/parser.cc"
    break;

  case 335:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_with( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 336:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].expr) ) { SemanticError( yylloc, "mutex argument list cannot be empty." ); (yyval.stmt) = nullptr; }
			(yyval.stmt) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].expr), (yyvsp[0].stmt) ) );
		}
#line 9730 "Parser/parser.cc"
    break;

  case 337:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9736 "Parser/parser.cc"
    break;

  case 338:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9742 "Parser/parser.cc"
    break;

  case 341:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.expr) = nullptr; }
#line 9748 "Parser/parser.cc"
    break;

  case 342:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9754 "Parser/parser.cc"
    break;

  case 345:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9760 "Parser/parser.cc"
    break;

  case 346:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-3].expr)->set_last( (yyvsp[-1].expr) )); }
#line 9766 "Parser/parser.cc"
    break;

  case 347:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9772 "Parser/parser.cc"
    break;

  case 348:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 349:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9784 "Parser/parser.cc"
    break;

  case 350:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].expr), (yyvsp[-1].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9790 "Parser/parser.cc"
    break;

  case 351:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9796 "Parser/parser.cc"
    break;

  case 352:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].expr), (yyvsp[-5].expr), maybe_build_compound( yylloc, (yyvsp[-4].stmt) ) ), (yyvsp[-2].expr), maybe_build_compound( yylloc, (yyvsp[0].stmt) ) ); }
#line 9802 "Parser/parser.cc"
    break;

  case 353:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9808 "Parser/parser.cc"
    break;

  case 356:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 9814 "Parser/parser.cc"
    break;

  case 357:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9820 "Parser/parser.cc"
    break;

  case 358:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9826 "Parser/parser.cc"
    break;

  case 359:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wand_waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9832 "Parser/parser.cc"
    break;

  case 360:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wand_waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9838 "Parser/parser.cc"
    break;

  case 361:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9844 "Parser/parser.cc"
    break;

  case 362:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9850 "Parser/parser.cc"
    break;

  case 363:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 3\n" ); (yyval.wfs) = nullptr; }
#line 9856 "Parser/parser.cc"
    break;

  case 364:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 4\n" ); (yyval.wfs) = nullptr; }
#line 9862 "Parser/parser.cc"
    break;

  case 365:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9868 "Parser/parser.cc"
    break;

  case 366:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 6\n" ); (yyval.wfs) = nullptr; }
#line 9874 "Parser/parser.cc"
    break;

  case 367:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_compound( yylloc, nullptr ) ); }
#line 9880 "Parser/parser.cc"
    break;

  case 368:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), (yyvsp[0].clause), nullptr ) ); }
#line 9886 "Parser/parser.cc"
    break;

  case 369:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-1].stmt), nullptr, (yyvsp[0].clause) ) ); }
#line 9892 "Parser/parser.cc"
    break;

  case 370:
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_try( yylloc, (yyvsp[-2].stmt), (yyvsp[-1].clause), (yyvsp[0].clause) ) ); }
#line 9898 "Parser/parser.cc"
    break;

  case 371:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ); }
#line 9904 "Parser/parser.cc"
    break;

  case 372:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.clause) = (yyvsp[-8].clause)->set_last( new ClauseNode( build_catch( yylloc, (yyvsp[-7].except_kind), (yyvsp[-4].decl), (yyvsp[-2].expr), (yyvsp[0].stmt) ) ) ); }
#line 9910 "Parser/parser.cc"
    break;

  case 373:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 9916 "Parser/parser.cc"
    break;

  case 374:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.expr) = (yyvsp[0].expr); }
#line 9922 "Parser/parser.cc"
    break;

  case 375:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9928 "Parser/parser.cc"
    break;

  case 376:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Terminate; }
#line 9934 "Parser/parser.cc"
    break;

  case 377:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.except_kind) = ast::Resume; }
#line 9940 "Parser/parser.cc"
    break;

  case 378:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.except_kind) = ast::Resume; }
#line 9946 "Parser/parser.cc"
    break;

  case 379:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.clause) = new ClauseNode( build_finally( yylloc, (yyvsp[0].stmt) ) ); }
#line 9952 "Parser/parser.cc"
    break;

  case 381:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9958 "Parser/parser.cc"
    break;

  case 382:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9964 "Parser/parser.cc"
    break;

  case 383:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9970 "Parser/parser.cc"
    break;

  case 388:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-4].is_volatile), (yyvsp[-2].expr), nullptr ) ); }
#line 9976 "Parser/parser.cc"
    break;

  case 389:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-6].is_volatile), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9982 "Parser/parser.cc"
    break;

  case 390:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-8].is_volatile), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9988 "Parser/parser.cc"
    break;

  case 391:
#line 1783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-10].is_volatile), (yyvsp[-8].expr), (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr) ) ); }
#line 9994 "Parser/parser.cc"
    break;

  case 392:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.stmt) = new StatementNode( build_asm( yylloc, (yyvsp[-12].is_volatile), (yyvsp[-9].expr), nullptr, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].labels) ) ); }
#line 10000 "Parser/parser.cc"
    break;

  case 393:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = false; }
#line 10006 "Parser/parser.cc"
    break;

  case 394:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.is_volatile) = true; }
#line 10012 "Parser/parser.cc"
    break;

  case 395:
#line 1797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10018 "Parser/parser.cc"
    break;

  case 398:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 10024 "Parser/parser.cc"
    break;

  case 399:
#line 1809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, "", maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) ); }
#line 10030 "Parser/parser.cc"
    break;

  case 400:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, maybeMoveBuild( (yyvsp[-3].expr) ), maybeMoveBuild( (yyvsp[-1].expr) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10039 "Parser/parser.cc"
    break;

  case 401:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 10045 "Parser/parser.cc"
    break;

  case 402:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 10051 "Parser/parser.cc"
    break;

  case 403:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) ) ); }
#line 10057 "Parser/parser.cc"
    break;

  case 404:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = new LabelNode(); (yyval.labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10066 "Parser/parser.cc"
    break;

  case 405:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.labels) = (yyvsp[-2].labels); (yyvsp[-2].labels)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10075 "Parser/parser.cc"
    break;

  case 406:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10081 "Parser/parser.cc"
    break;

  case 409:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10087 "Parser/parser.cc"
    break;

  case 410:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10093 "Parser/parser.cc"
    break;

  case 412:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10099 "Parser/parser.cc"
    break;

  case 413:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 10105 "Parser/parser.cc"
    break;

  case 420:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10116 "Parser/parser.cc"
    break;

  case 423:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].expr), maybeMoveBuild( (yyvsp[-2].expr) ) ); }
#line 10122 "Parser/parser.cc"
    break;

  case 424:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].expr), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10128 "Parser/parser.cc"
    break;

  case 428:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10134 "Parser/parser.cc"
    break;

  case 430:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].init) ); }
#line 10140 "Parser/parser.cc"
    break;

  case 431:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10146 "Parser/parser.cc"
    break;

  case 432:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10152 "Parser/parser.cc"
    break;

  case 433:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10158 "Parser/parser.cc"
    break;

  case 434:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10164 "Parser/parser.cc"
    break;

  case 435:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10170 "Parser/parser.cc"
    break;

  case 437:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10176 "Parser/parser.cc"
    break;

  case 438:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10182 "Parser/parser.cc"
    break;

  case 439:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10188 "Parser/parser.cc"
    break;

  case 440:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10199 "Parser/parser.cc"
    break;

  case 441:
#line 1983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10205 "Parser/parser.cc"
    break;

  case 442:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10211 "Parser/parser.cc"
    break;

  case 443:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10217 "Parser/parser.cc"
    break;

  case 444:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10223 "Parser/parser.cc"
    break;

  case 445:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10232 "Parser/parser.cc"
    break;

  case 446:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10241 "Parser/parser.cc"
    break;

  case 447:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10250 "Parser/parser.cc"
    break;

  case 448:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10261 "Parser/parser.cc"
    break;

  case 449:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10270 "Parser/parser.cc"
    break;

  case 450:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10276 "Parser/parser.cc"
    break;

  case 451:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10282 "Parser/parser.cc"
    break;

  case 452:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10288 "Parser/parser.cc"
    break;

  case 453:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10296 "Parser/parser.cc"
    break;

  case 454:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10304 "Parser/parser.cc"
    break;

  case 455:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 458:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "Useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "Useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 10325 "Parser/parser.cc"
    break;

  case 459:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10331 "Parser/parser.cc"
    break;

  case 460:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ); }
#line 10337 "Parser/parser.cc"
    break;

  case 461:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10343 "Parser/parser.cc"
    break;

  case 462:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10349 "Parser/parser.cc"
    break;

  case 463:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].init) ) ); }
#line 10355 "Parser/parser.cc"
    break;

  case 469:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Missing ';' after end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration" ) );
			(yyval.decl) = nullptr;
		}
#line 10366 "Parser/parser.cc"
    break;

  case 482:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10372 "Parser/parser.cc"
    break;

  case 485:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 488:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10384 "Parser/parser.cc"
    break;

  case 489:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10390 "Parser/parser.cc"
    break;

  case 490:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10396 "Parser/parser.cc"
    break;

  case 491:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10402 "Parser/parser.cc"
    break;

  case 492:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 493:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10414 "Parser/parser.cc"
    break;

  case 495:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 496:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 498:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 499:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10438 "Parser/parser.cc"
    break;

  case 500:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10444 "Parser/parser.cc"
    break;

  case 501:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10450 "Parser/parser.cc"
    break;

  case 502:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10456 "Parser/parser.cc"
    break;

  case 503:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10462 "Parser/parser.cc"
    break;

  case 504:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10468 "Parser/parser.cc"
    break;

  case 505:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10474 "Parser/parser.cc"
    break;

  case 506:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10480 "Parser/parser.cc"
    break;

  case 507:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10486 "Parser/parser.cc"
    break;

  case 508:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10492 "Parser/parser.cc"
    break;

  case 509:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10498 "Parser/parser.cc"
    break;

  case 510:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10504 "Parser/parser.cc"
    break;

  case 511:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10510 "Parser/parser.cc"
    break;

  case 512:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10516 "Parser/parser.cc"
    break;

  case 513:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 514:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10528 "Parser/parser.cc"
    break;

  case 515:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10534 "Parser/parser.cc"
    break;

  case 516:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10540 "Parser/parser.cc"
    break;

  case 517:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10546 "Parser/parser.cc"
    break;

  case 518:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10552 "Parser/parser.cc"
    break;

  case 519:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10558 "Parser/parser.cc"
    break;

  case 520:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10564 "Parser/parser.cc"
    break;

  case 521:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10570 "Parser/parser.cc"
    break;

  case 522:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10576 "Parser/parser.cc"
    break;

  case 523:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10582 "Parser/parser.cc"
    break;

  case 524:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10588 "Parser/parser.cc"
    break;

  case 525:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10594 "Parser/parser.cc"
    break;

  case 526:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10600 "Parser/parser.cc"
    break;

  case 527:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10606 "Parser/parser.cc"
    break;

  case 528:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10612 "Parser/parser.cc"
    break;

  case 529:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10618 "Parser/parser.cc"
    break;

  case 530:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10624 "Parser/parser.cc"
    break;

  case 531:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10630 "Parser/parser.cc"
    break;

  case 532:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10636 "Parser/parser.cc"
    break;

  case 533:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10642 "Parser/parser.cc"
    break;

  case 534:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10648 "Parser/parser.cc"
    break;

  case 536:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10654 "Parser/parser.cc"
    break;

  case 538:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 539:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10666 "Parser/parser.cc"
    break;

  case 540:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10672 "Parser/parser.cc"
    break;

  case 542:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 543:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 544:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 545:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 547:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 549:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 550:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10714 "Parser/parser.cc"
    break;

  case 551:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 552:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10726 "Parser/parser.cc"
    break;

  case 553:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 554:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10738 "Parser/parser.cc"
    break;

  case 555:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].expr), true ); }
#line 10744 "Parser/parser.cc"
    break;

  case 556:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10750 "Parser/parser.cc"
    break;

  case 557:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10756 "Parser/parser.cc"
    break;

  case 558:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10767 "Parser/parser.cc"
    break;

  case 559:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 560:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10779 "Parser/parser.cc"
    break;

  case 561:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10785 "Parser/parser.cc"
    break;

  case 562:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10796 "Parser/parser.cc"
    break;

  case 563:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10802 "Parser/parser.cc"
    break;

  case 564:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 565:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10817 "Parser/parser.cc"
    break;

  case 567:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10823 "Parser/parser.cc"
    break;

  case 568:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 569:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10835 "Parser/parser.cc"
    break;

  case 571:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 572:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 574:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 575:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 576:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 578:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 579:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10877 "Parser/parser.cc"
    break;

  case 580:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 581:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 582:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 584:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10901 "Parser/parser.cc"
    break;

  case 585:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10907 "Parser/parser.cc"
    break;

  case 586:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10913 "Parser/parser.cc"
    break;

  case 587:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10919 "Parser/parser.cc"
    break;

  case 588:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 10925 "Parser/parser.cc"
    break;

  case 589:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10936 "Parser/parser.cc"
    break;

  case 593:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10942 "Parser/parser.cc"
    break;

  case 594:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10948 "Parser/parser.cc"
    break;

  case 595:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10957 "Parser/parser.cc"
    break;

  case 596:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10965 "Parser/parser.cc"
    break;

  case 597:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10974 "Parser/parser.cc"
    break;

  case 598:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10983 "Parser/parser.cc"
    break;

  case 599:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10992 "Parser/parser.cc"
    break;

  case 600:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].expr), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11001 "Parser/parser.cc"
    break;

  case 602:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11007 "Parser/parser.cc"
    break;

  case 603:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-1].expr); }
#line 11013 "Parser/parser.cc"
    break;

  case 604:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11023 "Parser/parser.cc"
    break;

  case 605:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11042 "Parser/parser.cc"
    break;

  case 608:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11048 "Parser/parser.cc"
    break;

  case 609:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11054 "Parser/parser.cc"
    break;

  case 610:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11060 "Parser/parser.cc"
    break;

  case 611:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11066 "Parser/parser.cc"
    break;

  case 612:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11072 "Parser/parser.cc"
    break;

  case 613:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11078 "Parser/parser.cc"
    break;

  case 614:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11087 "Parser/parser.cc"
    break;

  case 615:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11093 "Parser/parser.cc"
    break;

  case 616:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11102 "Parser/parser.cc"
    break;

  case 617:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11108 "Parser/parser.cc"
    break;

  case 618:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11117 "Parser/parser.cc"
    break;

  case 619:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11123 "Parser/parser.cc"
    break;

  case 620:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11129 "Parser/parser.cc"
    break;

  case 621:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11142 "Parser/parser.cc"
    break;

  case 622:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 623:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11154 "Parser/parser.cc"
    break;

  case 624:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11167 "Parser/parser.cc"
    break;

  case 625:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11173 "Parser/parser.cc"
    break;

  case 628:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11179 "Parser/parser.cc"
    break;

  case 629:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11185 "Parser/parser.cc"
    break;

  case 632:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11191 "Parser/parser.cc"
    break;

  case 634:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 635:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].expr) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 636:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 637:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11215 "Parser/parser.cc"
    break;

  case 638:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].expr) ); }
#line 11221 "Parser/parser.cc"
    break;

  case 639:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11227 "Parser/parser.cc"
    break;

  case 641:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 643:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 644:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 646:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 647:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 11257 "Parser/parser.cc"
    break;

  case 649:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 11263 "Parser/parser.cc"
    break;

  case 650:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 651:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 652:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 653:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 654:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11298 "Parser/parser.cc"
    break;

  case 655:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11306 "Parser/parser.cc"
    break;

  case 656:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 11315 "Parser/parser.cc"
    break;

  case 657:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11323 "Parser/parser.cc"
    break;

  case 658:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11331 "Parser/parser.cc"
    break;

  case 659:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11339 "Parser/parser.cc"
    break;

  case 660:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].enum_hiding) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11347 "Parser/parser.cc"
    break;

  case 662:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11353 "Parser/parser.cc"
    break;

  case 663:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Hide; }
#line 11359 "Parser/parser.cc"
    break;

  case 664:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 665:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 666:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 667:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11383 "Parser/parser.cc"
    break;

  case 668:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].init) ) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 669:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 671:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.enum_hiding) = EnumHiding::Visible; }
#line 11401 "Parser/parser.cc"
    break;

  case 672:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11407 "Parser/parser.cc"
    break;

  case 673:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 674:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11419 "Parser/parser.cc"
    break;

  case 675:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11425 "Parser/parser.cc"
    break;

  case 676:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11431 "Parser/parser.cc"
    break;

  case 679:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 680:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11443 "Parser/parser.cc"
    break;

  case 681:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11449 "Parser/parser.cc"
    break;

  case 683:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 684:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 685:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 687:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 688:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11479 "Parser/parser.cc"
    break;

  case 689:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11485 "Parser/parser.cc"
    break;

  case 691:
#line 2792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11491 "Parser/parser.cc"
    break;

  case 694:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 695:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 697:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 698:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 699:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11521 "Parser/parser.cc"
    break;

  case 704:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11527 "Parser/parser.cc"
    break;

  case 706:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11533 "Parser/parser.cc"
    break;

  case 707:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11539 "Parser/parser.cc"
    break;

  case 708:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11545 "Parser/parser.cc"
    break;

  case 709:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].expr) ? new InitializerNode( (yyvsp[0].expr) ) : nullptr ); }
#line 11551 "Parser/parser.cc"
    break;

  case 710:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11557 "Parser/parser.cc"
    break;

  case 711:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11563 "Parser/parser.cc"
    break;

  case 717:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11569 "Parser/parser.cc"
    break;

  case 720:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11575 "Parser/parser.cc"
    break;

  case 721:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (yyvsp[-1].oper) == OperKinds::Assign ? (yyvsp[0].init) : (yyvsp[0].init)->set_maybeConstructed( false ); }
#line 11581 "Parser/parser.cc"
    break;

  case 722:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.init) = new InitializerNode( true ); }
#line 11587 "Parser/parser.cc"
    break;

  case 723:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11593 "Parser/parser.cc"
    break;

  case 724:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = new InitializerNode( (yyvsp[0].expr) ); }
#line 11599 "Parser/parser.cc"
    break;

  case 725:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = new InitializerNode( (yyvsp[-2].init), true ); }
#line 11605 "Parser/parser.cc"
    break;

  case 726:
#line 2888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.init) = nullptr; }
#line 11611 "Parser/parser.cc"
    break;

  case 728:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.init) = (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ); }
#line 11617 "Parser/parser.cc"
    break;

  case 729:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.init) = (InitializerNode *)( (yyvsp[-2].init)->set_last( (yyvsp[0].init) ) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 730:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.init) = (InitializerNode *)((yyvsp[-3].init)->set_last( (yyvsp[0].init)->set_designators( (yyvsp[-1].expr) ) )); }
#line 11629 "Parser/parser.cc"
    break;

  case 732:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11635 "Parser/parser.cc"
    break;

  case 734:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-1].expr)->set_last( (yyvsp[0].expr) )); }
#line 11641 "Parser/parser.cc"
    break;

  case 735:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11647 "Parser/parser.cc"
    break;

  case 736:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11653 "Parser/parser.cc"
    break;

  case 737:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11659 "Parser/parser.cc"
    break;

  case 738:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].expr) ), maybeMoveBuild( (yyvsp[-2].expr) ) ) ); }
#line 11665 "Parser/parser.cc"
    break;

  case 739:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[-2].expr); }
#line 11671 "Parser/parser.cc"
    break;

  case 741:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11677 "Parser/parser.cc"
    break;

  case 742:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11683 "Parser/parser.cc"
    break;

  case 743:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11689 "Parser/parser.cc"
    break;

  case 744:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11700 "Parser/parser.cc"
    break;

  case 745:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 746:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11712 "Parser/parser.cc"
    break;

  case 747:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 748:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11727 "Parser/parser.cc"
    break;

  case 749:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 750:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11739 "Parser/parser.cc"
    break;

  case 751:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11745 "Parser/parser.cc"
    break;

  case 752:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11751 "Parser/parser.cc"
    break;

  case 753:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11757 "Parser/parser.cc"
    break;

  case 754:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11763 "Parser/parser.cc"
    break;

  case 755:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11769 "Parser/parser.cc"
    break;

  case 756:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11775 "Parser/parser.cc"
    break;

  case 757:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11781 "Parser/parser.cc"
    break;

  case 758:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11787 "Parser/parser.cc"
    break;

  case 761:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 762:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 763:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11805 "Parser/parser.cc"
    break;

  case 764:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11811 "Parser/parser.cc"
    break;

  case 766:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11817 "Parser/parser.cc"
    break;

  case 767:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)( (yyvsp[-2].expr)->set_last( (yyvsp[0].expr) )); }
#line 11823 "Parser/parser.cc"
    break;

  case 768:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11829 "Parser/parser.cc"
    break;

  case 769:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 770:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11841 "Parser/parser.cc"
    break;

  case 771:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11847 "Parser/parser.cc"
    break;

  case 772:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11853 "Parser/parser.cc"
    break;

  case 773:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11862 "Parser/parser.cc"
    break;

  case 774:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11871 "Parser/parser.cc"
    break;

  case 775:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11880 "Parser/parser.cc"
    break;

  case 776:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11886 "Parser/parser.cc"
    break;

  case 777:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11895 "Parser/parser.cc"
    break;

  case 778:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11901 "Parser/parser.cc"
    break;

  case 780:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11907 "Parser/parser.cc"
    break;

  case 785:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 786:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11919 "Parser/parser.cc"
    break;

  case 787:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 789:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11931 "Parser/parser.cc"
    break;

  case 790:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11937 "Parser/parser.cc"
    break;

  case 791:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11943 "Parser/parser.cc"
    break;

  case 792:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11949 "Parser/parser.cc"
    break;

  case 794:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11955 "Parser/parser.cc"
    break;

  case 795:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11961 "Parser/parser.cc"
    break;

  case 796:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 797:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11984 "Parser/parser.cc"
    break;

  case 798:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11990 "Parser/parser.cc"
    break;

  case 799:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11996 "Parser/parser.cc"
    break;

  case 800:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 12002 "Parser/parser.cc"
    break;

  case 801:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12008 "Parser/parser.cc"
    break;

  case 802:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12014 "Parser/parser.cc"
    break;

  case 803:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12020 "Parser/parser.cc"
    break;

  case 805:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12029 "Parser/parser.cc"
    break;

  case 806:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].expr), nullptr ) ) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 807:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12044 "Parser/parser.cc"
    break;

  case 808:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12054 "Parser/parser.cc"
    break;

  case 809:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12063 "Parser/parser.cc"
    break;

  case 810:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12073 "Parser/parser.cc"
    break;

  case 811:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12082 "Parser/parser.cc"
    break;

  case 812:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12092 "Parser/parser.cc"
    break;

  case 813:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12101 "Parser/parser.cc"
    break;

  case 814:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12111 "Parser/parser.cc"
    break;

  case 815:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12120 "Parser/parser.cc"
    break;

  case 816:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12130 "Parser/parser.cc"
    break;

  case 818:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12136 "Parser/parser.cc"
    break;

  case 819:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].stmt) ); }
#line 12142 "Parser/parser.cc"
    break;

  case 820:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; forall = false; }
#line 12148 "Parser/parser.cc"
    break;

  case 821:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.expr) = (yyvsp[-2].expr); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.expr) = nullptr;
			} // if
		}
#line 12160 "Parser/parser.cc"
    break;

  case 822:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12171 "Parser/parser.cc"
    break;

  case 823:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12180 "Parser/parser.cc"
    break;

  case 824:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-3].decl) );
		}
#line 12189 "Parser/parser.cc"
    break;

  case 825:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12195 "Parser/parser.cc"
    break;

  case 826:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12201 "Parser/parser.cc"
    break;

  case 827:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12207 "Parser/parser.cc"
    break;

  case 828:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addType( (yyvsp[-4].decl) );
		}
#line 12216 "Parser/parser.cc"
    break;

  case 829:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12222 "Parser/parser.cc"
    break;

  case 830:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12228 "Parser/parser.cc"
    break;

  case 831:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].stmt), (yyvsp[-1].expr) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12234 "Parser/parser.cc"
    break;

  case 836:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].expr) ), maybeMoveBuild( (yyvsp[0].expr) ) ) ); }
#line 12240 "Parser/parser.cc"
    break;

  case 837:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12246 "Parser/parser.cc"
    break;

  case 838:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = maybeMoveBuild( (yyvsp[-2].expr) );
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12256 "Parser/parser.cc"
    break;

  case 839:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12262 "Parser/parser.cc"
    break;

  case 842:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12268 "Parser/parser.cc"
    break;

  case 843:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12274 "Parser/parser.cc"
    break;

  case 845:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12280 "Parser/parser.cc"
    break;

  case 846:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12286 "Parser/parser.cc"
    break;

  case 847:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12292 "Parser/parser.cc"
    break;

  case 848:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].expr) ); }
#line 12298 "Parser/parser.cc"
    break;

  case 853:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12304 "Parser/parser.cc"
    break;

  case 854:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12310 "Parser/parser.cc"
    break;

  case 855:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12316 "Parser/parser.cc"
    break;

  case 856:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12322 "Parser/parser.cc"
    break;

  case 857:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12328 "Parser/parser.cc"
    break;

  case 859:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12334 "Parser/parser.cc"
    break;

  case 860:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12340 "Parser/parser.cc"
    break;

  case 861:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12346 "Parser/parser.cc"
    break;

  case 862:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12352 "Parser/parser.cc"
    break;

  case 863:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12358 "Parser/parser.cc"
    break;

  case 864:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12364 "Parser/parser.cc"
    break;

  case 865:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12370 "Parser/parser.cc"
    break;

  case 866:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12376 "Parser/parser.cc"
    break;

  case 867:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12382 "Parser/parser.cc"
    break;

  case 868:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12388 "Parser/parser.cc"
    break;

  case 869:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12394 "Parser/parser.cc"
    break;

  case 870:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12400 "Parser/parser.cc"
    break;

  case 871:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12406 "Parser/parser.cc"
    break;

  case 872:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12412 "Parser/parser.cc"
    break;

  case 873:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12418 "Parser/parser.cc"
    break;

  case 874:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12424 "Parser/parser.cc"
    break;

  case 875:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12430 "Parser/parser.cc"
    break;

  case 876:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12436 "Parser/parser.cc"
    break;

  case 878:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12442 "Parser/parser.cc"
    break;

  case 879:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12448 "Parser/parser.cc"
    break;

  case 880:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12454 "Parser/parser.cc"
    break;

  case 881:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12460 "Parser/parser.cc"
    break;

  case 882:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12466 "Parser/parser.cc"
    break;

  case 883:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12472 "Parser/parser.cc"
    break;

  case 884:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12478 "Parser/parser.cc"
    break;

  case 885:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12484 "Parser/parser.cc"
    break;

  case 886:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12490 "Parser/parser.cc"
    break;

  case 887:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12496 "Parser/parser.cc"
    break;

  case 888:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12502 "Parser/parser.cc"
    break;

  case 889:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12508 "Parser/parser.cc"
    break;

  case 890:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12514 "Parser/parser.cc"
    break;

  case 891:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12520 "Parser/parser.cc"
    break;

  case 892:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12526 "Parser/parser.cc"
    break;

  case 893:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12532 "Parser/parser.cc"
    break;

  case 897:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12538 "Parser/parser.cc"
    break;

  case 898:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12544 "Parser/parser.cc"
    break;

  case 899:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12550 "Parser/parser.cc"
    break;

  case 900:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12556 "Parser/parser.cc"
    break;

  case 901:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12562 "Parser/parser.cc"
    break;

  case 902:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12568 "Parser/parser.cc"
    break;

  case 903:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 904:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12580 "Parser/parser.cc"
    break;

  case 905:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12586 "Parser/parser.cc"
    break;

  case 906:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12592 "Parser/parser.cc"
    break;

  case 907:
#line 3551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12598 "Parser/parser.cc"
    break;

  case 908:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12604 "Parser/parser.cc"
    break;

  case 909:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12610 "Parser/parser.cc"
    break;

  case 910:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12616 "Parser/parser.cc"
    break;

  case 911:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12622 "Parser/parser.cc"
    break;

  case 912:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12631 "Parser/parser.cc"
    break;

  case 913:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12637 "Parser/parser.cc"
    break;

  case 914:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12643 "Parser/parser.cc"
    break;

  case 916:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12649 "Parser/parser.cc"
    break;

  case 917:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12655 "Parser/parser.cc"
    break;

  case 918:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12661 "Parser/parser.cc"
    break;

  case 919:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12667 "Parser/parser.cc"
    break;

  case 920:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12673 "Parser/parser.cc"
    break;

  case 921:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12679 "Parser/parser.cc"
    break;

  case 922:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12685 "Parser/parser.cc"
    break;

  case 923:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12691 "Parser/parser.cc"
    break;

  case 924:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 925:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 926:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 927:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12715 "Parser/parser.cc"
    break;

  case 928:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 929:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 930:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 931:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12739 "Parser/parser.cc"
    break;

  case 932:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 933:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 935:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 936:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 937:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12769 "Parser/parser.cc"
    break;

  case 938:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 939:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12781 "Parser/parser.cc"
    break;

  case 940:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 941:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 942:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 943:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12805 "Parser/parser.cc"
    break;

  case 944:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12811 "Parser/parser.cc"
    break;

  case 945:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 946:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 947:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 948:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 949:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12841 "Parser/parser.cc"
    break;

  case 950:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 951:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 952:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 954:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 955:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 956:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 957:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 958:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 959:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 960:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12901 "Parser/parser.cc"
    break;

  case 961:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 962:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12913 "Parser/parser.cc"
    break;

  case 963:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 964:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 965:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12931 "Parser/parser.cc"
    break;

  case 966:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 967:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12943 "Parser/parser.cc"
    break;

  case 969:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 970:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 971:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 972:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12967 "Parser/parser.cc"
    break;

  case 973:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 974:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 975:
#line 3759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 976:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 977:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 978:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 979:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 981:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 982:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13021 "Parser/parser.cc"
    break;

  case 983:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13027 "Parser/parser.cc"
    break;

  case 984:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 985:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 986:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13045 "Parser/parser.cc"
    break;

  case 987:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 989:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13057 "Parser/parser.cc"
    break;

  case 990:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13063 "Parser/parser.cc"
    break;

  case 991:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13069 "Parser/parser.cc"
    break;

  case 992:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13075 "Parser/parser.cc"
    break;

  case 993:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13081 "Parser/parser.cc"
    break;

  case 994:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13087 "Parser/parser.cc"
    break;

  case 995:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13093 "Parser/parser.cc"
    break;

  case 996:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13099 "Parser/parser.cc"
    break;

  case 997:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].expr), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].expr), nullptr, false ) ); }
#line 13105 "Parser/parser.cc"
    break;

  case 998:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13111 "Parser/parser.cc"
    break;

  case 1000:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13117 "Parser/parser.cc"
    break;

  case 1001:
#line 3847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13123 "Parser/parser.cc"
    break;

  case 1003:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13129 "Parser/parser.cc"
    break;

  case 1004:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (ExpressionNode *)((yyvsp[-2].expr)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13135 "Parser/parser.cc"
    break;

  case 1006:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LThan; }
#line 13141 "Parser/parser.cc"
    break;

  case 1007:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.oper) = OperKinds::LEThan; }
#line 13147 "Parser/parser.cc"
    break;

  case 1008:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ); }
#line 13153 "Parser/parser.cc"
    break;

  case 1009:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13159 "Parser/parser.cc"
    break;

  case 1010:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].expr), nullptr, false ) ); }
#line 13165 "Parser/parser.cc"
    break;

  case 1011:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13171 "Parser/parser.cc"
    break;

  case 1012:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13177 "Parser/parser.cc"
    break;

  case 1015:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13183 "Parser/parser.cc"
    break;

  case 1016:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13189 "Parser/parser.cc"
    break;

  case 1017:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13195 "Parser/parser.cc"
    break;

  case 1018:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13201 "Parser/parser.cc"
    break;

  case 1019:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13207 "Parser/parser.cc"
    break;

  case 1020:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13213 "Parser/parser.cc"
    break;

  case 1021:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13219 "Parser/parser.cc"
    break;

  case 1022:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13225 "Parser/parser.cc"
    break;

  case 1024:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13231 "Parser/parser.cc"
    break;

  case 1025:
#line 3937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13237 "Parser/parser.cc"
    break;

  case 1026:
#line 3939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13243 "Parser/parser.cc"
    break;

  case 1027:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13249 "Parser/parser.cc"
    break;

  case 1028:
#line 3946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13255 "Parser/parser.cc"
    break;

  case 1029:
#line 3948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13261 "Parser/parser.cc"
    break;

  case 1031:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13267 "Parser/parser.cc"
    break;

  case 1033:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13273 "Parser/parser.cc"
    break;

  case 1034:
#line 3969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13279 "Parser/parser.cc"
    break;

  case 1035:
#line 3971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13285 "Parser/parser.cc"
    break;

  case 1036:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13291 "Parser/parser.cc"
    break;

  case 1037:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13297 "Parser/parser.cc"
    break;

  case 1038:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-4].decl), true ); }
#line 13303 "Parser/parser.cc"
    break;

  case 1040:
#line 3993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13309 "Parser/parser.cc"
    break;

  case 1041:
#line 3995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13315 "Parser/parser.cc"
    break;

  case 1042:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].oper) ); }
#line 13321 "Parser/parser.cc"
    break;

  case 1043:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].oper) ); }
#line 13327 "Parser/parser.cc"
    break;

  case 1044:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13333 "Parser/parser.cc"
    break;

  case 1045:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].oper) ) ); }
#line 13339 "Parser/parser.cc"
    break;

  case 1046:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13345 "Parser/parser.cc"
    break;

  case 1048:
#line 4014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13351 "Parser/parser.cc"
    break;

  case 1049:
#line 4016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13357 "Parser/parser.cc"
    break;

  case 1050:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13363 "Parser/parser.cc"
    break;

  case 1051:
#line 4023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13369 "Parser/parser.cc"
    break;

  case 1052:
#line 4025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13375 "Parser/parser.cc"
    break;

  case 1055:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13381 "Parser/parser.cc"
    break;

  case 1058:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13387 "Parser/parser.cc"
    break;

  case 1059:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13393 "Parser/parser.cc"
    break;

  case 1060:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13399 "Parser/parser.cc"
    break;

  case 1061:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13405 "Parser/parser.cc"
    break;

  case 1062:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13411 "Parser/parser.cc"
    break;

  case 1063:
#line 4056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13417 "Parser/parser.cc"
    break;

  case 1064:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13423 "Parser/parser.cc"
    break;

  case 1065:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13429 "Parser/parser.cc"
    break;

  case 1066:
#line 4067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13435 "Parser/parser.cc"
    break;

  case 1067:
#line 4069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13441 "Parser/parser.cc"
    break;

  case 1068:
#line 4071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13447 "Parser/parser.cc"
    break;

  case 1069:
#line 4074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13453 "Parser/parser.cc"
    break;

  case 1070:
#line 4076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13459 "Parser/parser.cc"
    break;

  case 1071:
#line 4078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13465 "Parser/parser.cc"
    break;

  case 1072:
#line 4080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13471 "Parser/parser.cc"
    break;

  case 1073:
#line 4082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13477 "Parser/parser.cc"
    break;

  case 1074:
#line 4087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13483 "Parser/parser.cc"
    break;

  case 1075:
#line 4089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), false ); }
#line 13489 "Parser/parser.cc"
    break;

  case 1076:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl), true ); }
#line 13495 "Parser/parser.cc"
    break;

  case 1077:
#line 4096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].expr), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13501 "Parser/parser.cc"
    break;

  case 1079:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13507 "Parser/parser.cc"
    break;

  case 1083:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13513 "Parser/parser.cc"
    break;

  case 1084:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13519 "Parser/parser.cc"
    break;

  case 1085:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13525 "Parser/parser.cc"
    break;

  case 1086:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13531 "Parser/parser.cc"
    break;

  case 1087:
#line 4142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].oper) ) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 1088:
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].oper) ) ); }
#line 13543 "Parser/parser.cc"
    break;

  case 1089:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13549 "Parser/parser.cc"
    break;

  case 1090:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13555 "Parser/parser.cc"
    break;

  case 1091:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13561 "Parser/parser.cc"
    break;

  case 1092:
#line 4157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13567 "Parser/parser.cc"
    break;

  case 1093:
#line 4159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 1094:
#line 4161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13579 "Parser/parser.cc"
    break;

  case 1095:
#line 4166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13585 "Parser/parser.cc"
    break;

  case 1096:
#line 4168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13591 "Parser/parser.cc"
    break;

  case 1097:
#line 4170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13597 "Parser/parser.cc"
    break;

  case 1098:
#line 4177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13603 "Parser/parser.cc"
    break;

  case 1099:
#line 4179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13609 "Parser/parser.cc"
    break;

  case 1102:
#line 4203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = nullptr; }
#line 13615 "Parser/parser.cc"
    break;

  case 1103:
#line 4205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.expr) = (yyvsp[0].expr); }
#line 13621 "Parser/parser.cc"
    break;


#line 13625 "Parser/parser.cc"

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
#line 4208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
