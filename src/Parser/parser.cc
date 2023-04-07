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
	ParseNode * pn;
	ExpressionNode * en;
	DeclarationNode * decl;
	ast::AggregateDecl::Aggregate aggKey;
	ast::TypeDecl::Kind tclass;
	StatementNode * sn;
	ast::WaitForStmt * wfs;
	ast::Expr * constant;
	CondCtl * ifctl;
	ForCtrl * fctl;
	OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	EnumHiding hide;
	ast::ExceptionKind catch_kind;
	ast::GenericExpr * genexpr;

#line 703 "Parser/parser.cc"

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
#define YYLAST   23905

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
       0,   606,   606,   610,   617,   618,   619,   620,   621,   625,
     626,   627,   628,   629,   630,   631,   632,   636,   637,   641,
     642,   647,   651,   652,   663,   665,   667,   671,   672,   674,
     676,   678,   680,   690,   692,   694,   696,   698,   700,   705,
     706,   717,   722,   727,   728,   733,   739,   741,   743,   749,
     751,   755,   757,   759,   761,   763,   765,   767,   769,   771,
     773,   775,   777,   779,   781,   783,   785,   795,   796,   800,
     801,   806,   809,   813,   814,   818,   819,   821,   823,   825,
     827,   829,   834,   836,   838,   846,   847,   855,   858,   859,
     861,   866,   882,   884,   886,   888,   890,   892,   894,   896,
     898,   906,   907,   909,   913,   914,   915,   916,   920,   921,
     923,   925,   927,   929,   931,   933,   935,   942,   943,   944,
     945,   949,   950,   954,   955,   960,   961,   963,   965,   970,
     971,   973,   978,   979,   981,   986,   987,   989,   991,   993,
     998,   999,  1001,  1006,  1007,  1012,  1013,  1018,  1019,  1024,
    1025,  1030,  1031,  1036,  1037,  1040,  1045,  1050,  1051,  1059,
    1065,  1066,  1070,  1071,  1075,  1076,  1080,  1081,  1082,  1083,
    1084,  1085,  1086,  1087,  1088,  1089,  1090,  1100,  1102,  1107,
    1108,  1110,  1112,  1117,  1118,  1124,  1125,  1131,  1132,  1133,
    1134,  1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1144,
    1145,  1151,  1153,  1163,  1165,  1173,  1174,  1179,  1181,  1183,
    1185,  1187,  1191,  1192,  1194,  1199,  1206,  1208,  1210,  1220,
    1222,  1224,  1229,  1234,  1237,  1242,  1244,  1246,  1248,  1256,
    1257,  1259,  1263,  1265,  1269,  1271,  1272,  1274,  1276,  1281,
    1282,  1286,  1291,  1292,  1296,  1298,  1303,  1305,  1310,  1312,
    1314,  1316,  1321,  1323,  1325,  1327,  1332,  1334,  1339,  1340,
    1362,  1364,  1369,  1372,  1374,  1377,  1379,  1382,  1384,  1389,
    1394,  1396,  1401,  1406,  1408,  1410,  1412,  1414,  1417,  1419,
    1422,  1424,  1429,  1435,  1438,  1440,  1445,  1451,  1453,  1458,
    1464,  1467,  1469,  1472,  1474,  1479,  1486,  1488,  1493,  1499,
    1501,  1506,  1512,  1515,  1520,  1528,  1530,  1532,  1537,  1539,
    1544,  1545,  1547,  1552,  1554,  1559,  1561,  1563,  1565,  1568,
    1572,  1575,  1579,  1581,  1583,  1585,  1587,  1589,  1591,  1593,
    1595,  1597,  1599,  1604,  1605,  1609,  1615,  1623,  1628,  1629,
    1633,  1634,  1640,  1644,  1645,  1648,  1650,  1655,  1658,  1660,
    1662,  1665,  1667,  1672,  1677,  1678,  1682,  1687,  1689,  1694,
    1696,  1701,  1703,  1705,  1707,  1710,  1712,  1717,  1723,  1725,
    1727,  1732,  1734,  1740,  1741,  1745,  1746,  1747,  1748,  1752,
    1757,  1758,  1760,  1762,  1764,  1768,  1772,  1773,  1777,  1779,
    1781,  1783,  1785,  1791,  1792,  1798,  1799,  1803,  1804,  1809,
    1811,  1820,  1821,  1823,  1828,  1833,  1844,  1845,  1849,  1850,
    1856,  1857,  1861,  1863,  1867,  1869,  1873,  1874,  1878,  1879,
    1883,  1890,  1891,  1895,  1897,  1912,  1913,  1914,  1915,  1917,
    1921,  1923,  1927,  1934,  1936,  1938,  1943,  1944,  1946,  1948,
    1950,  1982,  1985,  1990,  1992,  1998,  2003,  2008,  2019,  2026,
    2031,  2033,  2035,  2041,  2045,  2052,  2054,  2055,  2056,  2072,
    2074,  2077,  2079,  2082,  2087,  2088,  2092,  2093,  2094,  2095,
    2105,  2106,  2107,  2116,  2117,  2118,  2122,  2123,  2124,  2133,
    2134,  2135,  2140,  2141,  2150,  2151,  2156,  2157,  2161,  2163,
    2165,  2167,  2169,  2174,  2179,  2180,  2182,  2192,  2193,  2198,
    2200,  2202,  2204,  2206,  2208,  2211,  2213,  2215,  2220,  2222,
    2224,  2226,  2228,  2230,  2232,  2234,  2236,  2238,  2240,  2242,
    2244,  2246,  2248,  2250,  2252,  2254,  2256,  2258,  2260,  2262,
    2264,  2266,  2268,  2270,  2272,  2274,  2279,  2280,  2284,  2291,
    2292,  2298,  2299,  2301,  2303,  2305,  2310,  2312,  2317,  2318,
    2320,  2322,  2327,  2329,  2331,  2333,  2335,  2337,  2342,  2349,
    2351,  2353,  2358,  2366,  2365,  2369,  2377,  2378,  2380,  2382,
    2387,  2388,  2390,  2395,  2396,  2398,  2400,  2405,  2406,  2408,
    2413,  2415,  2417,  2419,  2420,  2422,  2427,  2429,  2431,  2436,
    2443,  2447,  2448,  2453,  2452,  2457,  2456,  2466,  2465,  2476,
    2475,  2485,  2490,  2491,  2496,  2502,  2520,  2521,  2525,  2527,
    2529,  2535,  2537,  2539,  2541,  2546,  2548,  2553,  2555,  2564,
    2565,  2570,  2579,  2581,  2583,  2592,  2594,  2595,  2596,  2598,
    2600,  2601,  2606,  2607,  2608,  2613,  2615,  2618,  2621,  2628,
    2629,  2630,  2636,  2641,  2643,  2649,  2650,  2656,  2657,  2661,
    2666,  2669,  2668,  2672,  2675,  2682,  2687,  2686,  2695,  2700,
    2705,  2710,  2715,  2716,  2721,  2723,  2728,  2730,  2732,  2734,
    2739,  2740,  2746,  2747,  2748,  2755,  2756,  2758,  2759,  2760,
    2762,  2764,  2771,  2772,  2774,  2776,  2781,  2782,  2788,  2789,
    2791,  2792,  2797,  2798,  2799,  2801,  2809,  2810,  2812,  2815,
    2817,  2821,  2822,  2823,  2825,  2827,  2832,  2834,  2839,  2841,
    2850,  2852,  2857,  2858,  2859,  2863,  2864,  2865,  2870,  2871,
    2876,  2877,  2878,  2879,  2883,  2884,  2889,  2890,  2891,  2892,
    2893,  2907,  2908,  2913,  2914,  2920,  2922,  2925,  2927,  2929,
    2952,  2953,  2959,  2960,  2966,  2965,  2975,  2974,  2978,  2984,
    2990,  2991,  2993,  2997,  3002,  3004,  3006,  3008,  3014,  3015,
    3019,  3020,  3025,  3027,  3034,  3036,  3037,  3039,  3044,  3046,
    3048,  3053,  3055,  3060,  3065,  3073,  3078,  3080,  3085,  3090,
    3091,  3096,  3097,  3101,  3102,  3103,  3108,  3110,  3116,  3118,
    3123,  3125,  3131,  3132,  3136,  3140,  3144,  3146,  3159,  3161,
    3163,  3165,  3167,  3169,  3171,  3172,  3177,  3180,  3179,  3191,
    3190,  3203,  3202,  3214,  3213,  3225,  3224,  3238,  3244,  3246,
    3252,  3253,  3264,  3271,  3276,  3282,  3285,  3288,  3292,  3298,
    3301,  3304,  3309,  3310,  3311,  3312,  3316,  3322,  3323,  3333,
    3334,  3338,  3339,  3344,  3349,  3350,  3356,  3357,  3359,  3364,
    3365,  3366,  3367,  3368,  3370,  3405,  3407,  3412,  3414,  3415,
    3417,  3422,  3424,  3426,  3428,  3433,  3435,  3437,  3439,  3441,
    3443,  3445,  3450,  3452,  3454,  3456,  3465,  3467,  3468,  3473,
    3475,  3477,  3479,  3481,  3486,  3488,  3490,  3492,  3497,  3499,
    3501,  3503,  3505,  3507,  3519,  3520,  3521,  3525,  3527,  3529,
    3531,  3533,  3538,  3540,  3542,  3544,  3549,  3551,  3553,  3555,
    3557,  3559,  3571,  3576,  3581,  3583,  3584,  3586,  3591,  3593,
    3595,  3597,  3602,  3604,  3606,  3608,  3610,  3612,  3614,  3619,
    3621,  3623,  3625,  3634,  3636,  3637,  3642,  3644,  3646,  3648,
    3650,  3655,  3657,  3659,  3661,  3666,  3668,  3670,  3672,  3674,
    3676,  3686,  3688,  3690,  3691,  3693,  3698,  3700,  3702,  3707,
    3709,  3711,  3713,  3718,  3720,  3722,  3736,  3738,  3740,  3741,
    3743,  3748,  3750,  3755,  3757,  3759,  3764,  3766,  3771,  3773,
    3790,  3791,  3793,  3798,  3800,  3802,  3804,  3806,  3811,  3812,
    3814,  3816,  3821,  3823,  3825,  3831,  3833,  3836,  3839,  3841,
    3845,  3847,  3849,  3850,  3852,  3854,  3858,  3860,  3865,  3867,
    3869,  3871,  3906,  3907,  3911,  3912,  3914,  3916,  3921,  3923,
    3925,  3927,  3929,  3934,  3935,  3937,  3939,  3944,  3946,  3948,
    3954,  3955,  3957,  3966,  3969,  3971,  3974,  3976,  3978,  3992,
    3993,  3995,  4000,  4002,  4004,  4006,  4008,  4013,  4014,  4016,
    4018,  4023,  4025,  4033,  4034,  4035,  4040,  4041,  4046,  4048,
    4050,  4052,  4054,  4056,  4063,  4065,  4067,  4069,  4071,  4074,
    4076,  4078,  4080,  4082,  4087,  4089,  4091,  4096,  4122,  4123,
    4125,  4129,  4130,  4134,  4136,  4138,  4140,  4142,  4144,  4151,
    4153,  4155,  4157,  4159,  4161,  4166,  4168,  4170,  4177,  4179,
    4197,  4199,  4204,  4205
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

#define YYPACT_NINF (-1836)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1102)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      67, 12750,    76,   450,  9580,   370, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,   322,   906,
     447, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,   112,   676,
   -1836, -1836, -1836, -1836, -1836, -1836,  4753,  4753,   496, 12750,
     552,   611, 23583, -1836,   623, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836,  2215, -1836,   900,   396, -1836,
   -1836, -1836, -1836, -1836, 17683, -1836, -1836,   637,   669,   724,
     159, -1836,  4753,   669,   669,   669,   658,  4484,   835,  1012,
   12912, -1836, -1836,   780, 17531,  1896, -1836, -1836, -1836,  2732,
     883, 10405, 11283,  1070,  2732,  1112,   747, -1836, -1836, -1836,
   -1836,   838, -1836, -1836, -1836, -1836,   755, -1836, -1836, -1836,
   -1836, -1836,   764,   771,   838, -1836,   838,   786, -1836, -1836,
   -1836, 18678,  4753, -1836, -1836,  4753, -1836, 12750, -1836,   778,
   18830, -1836, -1836,  4549, 20251, -1836, -1836,  1081,  1081,   834,
    2425, -1836, -1836, -1836, -1836,   462, 14969,  2696,   838, -1836,
   -1836, -1836, -1836, -1836, -1836,   860, -1836,   813,   872,   884,
   -1836,   929, 22974, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   16450,  3446,  2215,   720,   911,   921,   928,   937,   945,   950,
   -1836, -1836, 18982, 11691,   890, -1836, 17980, -1836, -1836, -1836,
   -1836,   965, -1836, -1836,   999, -1836, 21124,  1101, 21272, -1836,
    1027,  4753,   771,  1037,  1006,  1039,  1047, -1836, -1836, -1836,
    3632,  3774,  1049,  1114,    60,  1114, -1836,   838,   838,    40,
     263,   182,  1114, -1836,   838,   838,    40,   838, -1836,   838,
   -1836,  4173, -1836, -1836,  1074,  1086,  1081, 20447, -1836, 17683,
   -1836, -1836,  2732, -1836,  1831,   747,  1084,  1168,   263,  4753,
    4753,   724, -1836, 14486, -1836,  1081,  1081,  1130,  1168,   263,
    4753, -1836, 23782, -1836, -1836, -1836,  1081, -1836, -1836, -1836,
   -1836,  1081, -1836,   949,  4274,  4753, -1836,  2085,  1118, -1836,
   -1836, -1836, 17344,   771,   266, -1836, -1836, 20396, -1836,  1114,
     294, -1836, 22974, 20251,  3869,  4173, -1836,   356, -1836, -1836,
   -1836, -1836, -1836, 18830,  4753, -1836,  1132, -1836, -1836, -1836,
   -1836,  4753,  2567,   683,   401, -1836,  4753,   813, -1836,   993,
     838,   838,  1120, 19134,   888, 15452, 20949,  2732,  2732, -1836,
    2732,  1081,  2732,  1081, -1836, -1836,   838, -1836,  1143, -1836,
   19286, -1836, -1836, -1836, 19438,   965, -1836,   -95,   788,   305,
     452,   747,  1147, -1836,  2425,  1141,   813,  2425,  1564, -1836,
    1175,  1214, 23048,  1182,  1189,  1193, 22974, 23122,  1195, 23687,
   -1836, -1836, -1836, -1836, -1836, -1836, 23196, 23196, 16294,  1196,
    4700, -1836, -1836, -1836, -1836,   254, -1836,   599, -1836,  1188,
   -1836, 22974, 22974, -1836,  1192,   868,  1026,  1157,   726,  1133,
    1194,  1202,  1199,  1234,   291, -1836,   427, -1836,  1226, -1836,
    1220,  3065, 16762, -1836, -1836,   828,  1226, -1836, -1836,   567,
   -1836, -1836,  3446,  1233,  1238,  1240,  1243,  1253,  1260, -1836,
   -1836,   428,  1245, -1836,   577,  1245, -1836, -1836, 18678, -1836,
    1227,  1232, 16918, -1836, -1836,  4849,  4188,  1282, 15452,  1285,
     757,   880, -1836, -1836, -1836, -1836, -1836,  4753,  4866, -1836,
   -1836, -1836, -1836, -1836, -1836, 17239,  3808,  1196, 21124,  1263,
    1270, -1836, -1836,  1273, 21272,   866, -1836, -1836, -1836,  9164,
    1284, -1836, -1836, -1836, -1836, -1836,  3632,   472,  1290,  1293,
    1298,   827,  1309,  1319,  1352,  1355,  1357,  1368,  3774, -1836,
   -1836, -1836,   838,  1286,  1348,  1375, -1836, -1836,  1380,   724,
   -1836, -1836,   771,  1168, -1836, -1836, -1836,   724, -1836, -1836,
     771, -1836, -1836,  4173, -1836, 16762, 16762, -1836,  1081,  4549,
   21037, 15613, -1836, -1836, -1836, -1836, -1836,   771,  1168,   294,
    1409, -1836, -1836,  2732,  1422,  1168,   263, -1836,   771,  1168,
   -1836, 23833, -1836,  1081,  1081, -1836, -1836,  1435,    43,  1438,
     747,  1453, -1836, 17844, -1836,   642, -1836,  1472, 20846, -1836,
    4549, 17432, 20447, -1836, 17344, 23270, -1836, -1836, -1836, -1836,
   -1836,  3869,   918,  4173, -1836, 15613,  1114, 12750, -1836,  1390,
   -1836,  1458, -1836, -1836, -1836, -1836, -1836,  2425, -1836, -1836,
    1540,  4429,  2832, 19438, 11691, -1836, 19590, -1836,  1081,  1081,
   -1836, -1836,   965, -1836,   982,  1465,  1615, 22974,   905,  1380,
    1459, -1836,   838,   838, -1836,  1245, -1836, 19134, -1836, -1836,
   18275,  1081,  1081, -1836,  4429,   838, -1836, 20106, -1836, -1836,
   19286, -1836,   462, -1836, -1836, -1836,  1479,  4753,  1147,  1491,
     855, 18830,   861, -1836, -1836, -1836, -1836, -1836, -1836,   892,
   -1836,  1512,  1490, -1836, 16606, -1836,  4700, 19742, 19742, -1836,
   16606, -1836, 22974, -1836, -1836, -1836, -1836, -1836, -1836, 16606,
   -1836, -1836, 18374, 19742, 19742,  1220,  1442,  1462,   716,  1550,
   -1836,   895,  1533,  1247,  1535, -1836,  9164, 22974, 21346,  1544,
   22974,  2085, 22974,  2085, -1836,  2602, -1836, -1836, 21420,   749,
   22974, 21420,  2085, -1836, -1836, 22974, 22974, 22974, 22974, 22974,
   22974, 22974, 22974, 22974, 22974, 22974, 22974, 22974, 22974, 22974,
   22974, 22974, 22974, 22974, 21494,  1516,   929,  3659, 11691, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
    1553, 22974, -1836, -1836,   828,  1207, -1836, -1836,   838,   838,
   -1836, -1836, 16762, -1836,   439,  1245, -1836,   924,  1245, -1836,
   -1836, -1836,  1380, -1836, -1836,  1380, 23344, -1836, -1836, 11691,
    1560,  1562,  2904,  1705,  3139,   479,  1459, -1836,   838,   838,
    1459,   490, -1836,   838,   838, 22974,  4753,  1269,  1276,  1459,
     121, 14808, 14808,  4753, -1836, -1836, 22974,  1273, -1836, 21124,
    1573, -1836,  1476, -1836, -1836, -1836, -1836, -1836,   964, -1836,
   14808,  2085,  4549,  2085,   984,  1571,  1575,  1579,  1003,  1591,
    1592,  1593,  1597,  1599,  1600,   520,  1245, -1836, -1836,   533,
    1245, -1836, -1836,   560,  1245, -1836, -1836, -1836,  4549,   929,
    1734,  1245, 20541, -1836, -1836,   771, 17844, -1836, -1836, -1836,
    1002,  1604,  1009,  1608, -1836,  1576, -1836,   771, -1836,  1612,
   -1836,   771,  1168,  1576, -1836,   771,  1607,  1609,  1611, -1836,
   -1836, 18275, -1836,  1610, -1836, -1836, -1836,  2085,  4753, 10759,
    1702,  1601, -1836, -1836, 19903, -1836,  1232, -1836, 14808,  1029,
   -1836, -1836,  1576, -1836, 18830, 16762,  1598, -1836,  1598, -1836,
   -1836, -1836,   305,   838,   838, -1836, 19286, -1836, 11856, 17074,
   -1836, 17844,  1623,  1629,  1631, -1836,  9991,   838, -1836,   905,
   -1836, -1836, -1836, -1836,  1380, -1836, -1836, -1836,  1081, -1836,
    3036, -1836, -1836,   747,   398,  1636,  1606,  1630,   305, -1836,
   -1836,  1633,  1635,  1564, 21420, -1836,  1637,  1639,   396,  1648,
    1640,  1654,  1651,  1659, 22974,  1661,  1662,  1664, 11691, 22974,
   -1836, -1836,  1787, -1836, -1836, -1836, 22974, -1836,  1666,  1668,
   21198,  1283, -1836, 21420,  1667, -1836,  1669, -1836, -1836,  2364,
   -1836, -1836,  1036, -1836, -1836, -1836, -1836,  2364, -1836, -1836,
    1308,   547, -1836, -1836,  1192,  1192,  1192,   868,   868,  1026,
    1026,  1157,  1157,  1157,  1157,   726,   726,  1133,  1194,  1202,
    1199,  1234, 22974,  1311, -1836,  1673,  2364, -1836, -1836, 21124,
   -1836, 17844,  1674,  1676,  1677,  1207, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836,  1380, -1836, -1836,  1380, 17844, 17844,
   -1836, -1836,  2904,   951,  1678,  1679,  1681,  1683,  1628,  3139,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836,  1682, -1836,  1459, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836,  1686,  1688, -1836,   724,  2364,
    1329,    29, -1836, -1836,  1672, -1836, 21272, -1836, 22974,   838,
   21568, 14808, -1836, -1836, -1836,  1671,   571,  1245, -1836,   583,
    1245, -1836, -1836,   625,  1245, -1836, -1836, -1836,  1380, -1836,
   -1836, -1836,  1380, -1836, -1836, -1836,  1380,  1114,  1690, -1836,
    1380,   -18, -1836,  1226,  1687, -1836, -1836, -1836, -1836, -1836,
   -1836,  1697, -1836, -1836, -1836, 18830,  1576, -1836,   771, -1836,
   -1836, -1836, -1836, -1836, 13553,  1696,  1693, -1836,   482, -1836,
     653,   373, 11526,  1700, 15973,  1704,  1706,  1888,  2284,  2449,
   21642,  1708, -1836, -1836,  1709,  1711, -1836, -1836,   771, 22974,
   22974,  1843,  1714,   719, -1836, 16138,  1790,  1716,  1698, -1836,
   -1836, -1836, 10584, -1836, -1836, -1836, -1836, -1836,  1406, -1836,
   -1836, -1836,  1400,   239, -1836,   545, -1836,   239, -1836, -1836,
   -1836,  2085, -1836, -1836, 13074, 17683,  1719, -1836,  4753, -1836,
    1703,  1710,  1722, -1836,  1338, -1836, -1836, -1836, -1836,  4549,
   -1836, -1836,  1732,  1733,  1050, 18830,   813,   813,  1479,  1147,
    1147, -1836, -1836,  1196,  1232, 16918, -1836,  1226, -1836, 12021,
   -1836,   628,  1245, -1836,  1081, 12584, -1836, -1836,   305,   838,
     838,   462,  4753, -1836, 21716, -1836,   305,  1479,  1730, -1836,
   -1836,  1056,   805, 18275, 11691,  2085, -1836,   805, 18526,   805,
   -1836, 22974, 22974, 22974, -1836, -1836, -1836, -1836, 22974, 22974,
    1726, 21124, -1836, -1836,  1724,   818, -1836, -1836, -1836,  2462,
   -1836, -1836,  1347, -1836,   290, -1836, 21420,  1382, -1836,  9164,
   -1836, -1836, 22974,  1745,  1387,  1392,  1273, -1836,   638,  1245,
   -1836, -1836, 17844, 17844, -1836, -1836,  1769,   678,  1245, -1836,
     681,  3111,   838,   838, -1836, -1836, 17844, 17844, -1836,  1767,
   -1836, 15613, 15613,  1771,  1768,  1770,  1773, -1836,  1772, 22974,
   22974,  1397,  1775, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
    1780, 22974, -1836, -1836, -1836,  1380, -1836, -1836, -1836,  1380,
   -1836, -1836, -1836,  1380, 17844, 17844, 17844,   724,   838, -1836,
   -1836,  1414, 22974, 20690,  1778,  1785,  1789, -1836, -1836, -1836,
    1792, 13708, 13863, 14018, 18830, 20447, 19742, 19742,  1793, -1836,
    1776,  1777,  2669, 14325, -1836,   506,  4753, -1836, -1836,  4753,
   -1836, 21420,   342,   434, -1836, -1836, -1836, -1836, 22974,  1796,
    1864, 11360, 10934, -1836,  1779, -1836,  1781, 22974,  1783, 21124,
    1786, 22974,  9164, 22974,  1061, -1836,  1788,   108, -1836,   306,
    1857,   540,  1798, -1836, -1836,  1806, -1836,  1794, -1836,  1795,
    1810,  1818, 15973, 15973, -1836, -1836,  1884, -1836, -1836,    52,
      52,  1022, 14647,   838,   572, -1836, -1836, -1836,  1821, -1836,
    1826, -1836,  1827, -1836,  1822, -1836,  1823, -1836, -1836, -1836,
   -1836,  1832,  1824,  1825, 12186,  1830,  1833,  1836, -1836,  1828,
   -1836, -1836, -1836,  1380, 22974, 22974,  1232,  1849, -1836,  1479,
   -1836,  1147,   327,  1606, 21124, -1836,  1479,  1837, -1836, 18830,
   -1836,   987,  1835,  1851,  1058, -1836,  1854, -1836, -1836, -1836,
   -1836, -1836, 21124,  1273,  9164, -1836,  1887,  2364, -1836,  1887,
    1887, -1836,  2364,  3412,  3946, -1836, -1836,  1416, -1836, -1836,
   -1836,  1863,  1862, -1836, -1836, -1836,  1380, -1836, -1836,  1867,
    1868,   838, -1836, -1836, -1836,  1380, -1836, -1836, -1836,  1870,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836,  1860, -1836, -1836, -1836, -1836,  1861,  1871,
     838, -1836, 17844, 17844, 17844, -1836, -1836, -1836, -1836, -1836,
   22974, -1836,   -18, -1836,  1226, -1836, -1836, -1836,  1876,  1877,
   -1836,  1793,  1793,  1793,  3712,  1109,  1852,   584, -1836,  3712,
     606, 16762, -1836, -1836, -1836,  4094, 22974,  4259,   629, -1836,
   -1836,   103,  1872,  1872,  1872,  4753, -1836, -1836, 18141, -1836,
    1073, -1836, -1836, -1836, -1836,  1082,  1880, 15973,  1716,  1879,
   22974,   637,  1878,   658, 14180, 18830, -1836, -1836, -1836,   779,
   15973, 22974,  1228,    -5, -1836, 22974,  8773, -1836, -1836,   644,
   -1836,  1273, -1836,  1089,  1096,  1098, -1836, -1836, -1836, -1836,
     771,  1061,  1883, -1836, -1836, 22974, -1836,  1886,   929, -1836,
   11526, -1836, -1836, -1836, -1836, 22974, 22974, -1836, -1836,   321,
      52, -1836,   687, -1836, -1836,  9730, -1836,   838, 15613, -1836,
   -1836, 18830, -1836, -1836, -1836,   305,   305, -1836, -1836, -1836,
    1882, -1836, 17844, -1836, -1836,  1891, -1836,  1895,  1890,  1147,
    1893, -1836, -1836,  1273,  1904, -1836, -1836,  1905, -1836, -1836,
   22974, -1836, 18526, 22974,  1273,  1907,  1445, -1836,  1448, -1836,
    2364, -1836,  2364, -1836, -1836, -1836, -1836, 17844,  1906,  1908,
   -1836, -1836, 17844, 17844,  1910,  1913,  1450, 15130, 15291, -1836,
    1911, -1836, -1836, -1836, -1836, -1836,  1916,  1917,  1919,  1463,
   22974, -1836, -1836, -1836, -1836, -1836,   708,  1109,  2139,   711,
   -1836, -1836, -1836, -1836,   838,   838, -1836, -1836, -1836,   756,
   -1836,  1104,  4094,   955, -1836,  4259,   838, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, 15973,   105, 21790,  1986,
   15973,  1716, 15774, -1836, -1836, -1836, -1836, 22974, -1836, 21864,
    2000,  1899, 21047, 21938, 15973, 11109,  1716,   677,  1602,  1900,
   22974, -1836,  1928,   385, 15973, -1836, -1836,  1929, -1836, -1836,
    1912,   929,   819,  1930,  1931,  1399,  1933, 15973,  1936, 15973,
   15973, 15973, 15973, -1836, -1836, -1836, -1836,  4753,  4549,  1479,
    1479, -1836, -1836,  1926,  1934, -1836, -1836, -1836,  1935,   305,
    1943, -1836,  1946, -1836, -1836, -1836, -1836,  1947, -1836, -1836,
   -1836,  1468,  1484, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836,  1945, -1836, -1836,  1948,  1949,  1950, -1836,
   -1836, -1836, -1836, -1836, -1836,  1951,  1952,  1953,  2139, -1836,
     838, -1836, -1836, -1836, -1836, -1836,  1957,  3712, -1836,  8027,
     123, 12354, -1836, 15868, -1836,    97,  1117, 15973,  2026,   776,
    1941,   403, 15973, 22974,  1959,   677,  1602,  1938, 23418,  1954,
     499,  2042, -1836, 22012, 22086, 22974,  1716,  1956, 12518, -1836,
   -1836, -1836, 19954, -1836,  1964,  1958,   101, 15973, -1836, 22974,
   21420, -1836, -1836, 22974,   239, -1836, -1836,   239, -1836, -1836,
    1974,  1977,  1976, -1836, -1836,   305,  1479, -1836, -1836, -1836,
   -1836, -1836,  1985,  1991,  1993, 15613,  1992, -1836, -1836, -1836,
     696,  1245, -1836, -1836,  1109, -1836, -1836,   362, -1836,   363,
   -1836, -1836, -1836,  1998, 13236, -1836, -1836, 15973, -1836,    98,
   -1836, 15973, 22974,  2004, 22160, -1836, -1836, 22234, 22308, 22974,
    1959,  1716, 22382, 22456, 15973,  1995,   641,  1996,   743, -1836,
   -1836,  2009, 13236, 19954, -1836,  4417, 19590,  2085,  2006, -1836,
    2063,  2016,   831,  2011, -1836,  2099, -1836,  1122,  1128,   104,
     449, -1836, -1836, -1836,  1479,  2029, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836,  1380, -1836, 22974, -1836, 22974, -1836, -1836,
    1570, 13398, -1836, -1836, 15973, -1836, -1836,  1716, -1836, -1836,
    1716,  2013,   781,  2014,   793, -1836, -1836,  1716, -1836,  1716,
   -1836,  2031, 22530, 22604, 22678, -1836,  1570, -1836,  2008,  3338,
    3645, -1836, -1836, -1836,   101,  2033, 22974,  2025,   101,   101,
   15973, -1836, -1836, 15973,  2116, 15973,  2129,  2054, -1836, 17844,
   -1836, -1836, 15868, -1836,  1570, -1836, -1836,  2053, 22752, 22826,
   22900, -1836, -1836,  1716, -1836,  1716, -1836,  1716, -1836,  2008,
   22974,  2060,  3645,  2048,   929,  2064, -1836,   844, -1836, -1836,
   -1836, 15973, -1836, 15973, -1836, -1836, -1836, 10240,  2057, 15868,
   -1836, -1836,  1716, -1836,  1716, -1836,  1716,  2066,  2065, -1836,
     771,   929,  2068, -1836,  2046,   929, -1836, -1836,  2070, -1836,
   -1836, -1836, 10408, -1836,   771, -1836, -1836,  1487, 22974, -1836,
    1135, -1836, -1836,   929,  2085,  2072,  2050, -1836, -1836,  1137,
   -1836, -1836,  2052,  2085, -1836, -1836
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
   -1836,  6668,  5800, -1836,    -1,   412,  1638,  -117, -1836,  -366,
   -1836,   480, -1836,  -713, -1836,   907,  -984, -1017, -1836,   334,
    5551,  2062, -1836,  1858, -1836,  1523,   569,   941,   944,   580,
     935,  1486,  1489,  1485,  1493,  1488, -1836,  -174,  -171,  8758,
    1043, -1836,  1815, -1836, -1836,  -693,  7002, -1073,  1235, -1836,
     225, -1836,  1025,    69, -1836, -1836, -1836,   550,   160, -1836,
   -1725, -1601,   402,   135, -1836, -1836, -1836,   413, -1566, -1836,
   -1409, -1836, -1836, -1836, -1836,  -379, -1117, -1836,   549, -1199,
     548, -1836, -1836, -1836, -1836, -1836,   219, -1176, -1836, -1836,
   -1836,   100,   576,   581,   209, -1836, -1836, -1836, -1836,  -909,
   -1836,   144,    79, -1836,   211, -1836,    -8, -1836, -1836, -1836,
    1038,  -696,  -835, -1343, -1836,   247, -1216,   134,  5848,  -804,
    -743, -1836,  -287, -1836, -1836,    27, -1836,  -162,    86,  -323,
    -242,  3850,   817,  -650,    20,    84,    88,   313,  2198, -1836,
    2226, -1836,    96,  3646, -1836,  2165, -1836,    92, -1836, -1836,
     394,   119,  4475,  2731,   -38,  2021,  -328, -1836, -1836, -1836,
   -1836, -1836,  -285,  5163,  4741, -1836,  -389,   180, -1836,  -832,
     341, -1836,   272,   863, -1836,    59,  -193, -1836, -1836, -1836,
    -356,  5429,  -907,  1330,   193,  -669,  -641,  -474,  1572, -1836,
   -1318,  -159,  -184,  1568,  1065,  4574,  -209,  -464,  -196,  -178,
    -461,  1471, -1836,  1802,   -85,  1373,  1689, -1836, -1836, -1836,
   -1836,   465,  -155,   188,  -899, -1836,   471, -1836, -1836, -1130,
     586, -1836, -1836, -1836,  2304,  -774,  -380,  -841,   -11, -1836,
   -1836, -1836, -1836, -1836, -1836,   274,  -782,  -238, -1835,  -197,
    8377,   -73,  7183, -1836,  1332, -1836,  5577,   -20,  -220,  -200,
    -198,    11,   -74,   -70,   -69,   183,    -7,    17,    32,  -181,
      68,  -152,  -150,  -146,    72,  -128,  -104,  -100,  -699,  -745,
    -736,  -681,  -695,  -116,  -668, -1836, -1836,  -723,  1534,  1537,
    1538,  1569, -1836,   702,  7428, -1836,  -596,  -581,  -580,  -565,
    -705, -1836, -1751, -1737, -1734, -1724,  -616,   -65,  -314, -1836,
   -1836,   -13,   197,    51, -1836,  8169,  1165,  -301,  -349
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
     625,   103,   376,  1001,  1476,  1258,  1254,   827,    97,  1461,
     509,   956,   510,  1327,   719,  1407,   524,   656,   529,   201,
     498,   659,   950,  1586,  1587,   537,   658,  1929,   180,   511,
     661,   372,  1056,   942,   943,    81,    81,  1074,    81,   994,
    1925,  1287,  1334,  1926,   380,  -971,  1075,  -788,   197,   944,
     133,   209,  -971,  1927,    81,   561,   146,   235,   512,   103,
     513,   881,   883,    81,   514,   104,    97,    98,  1460,   105,
     153,    81,   198,   113,   508,   207,    81,   109,  1661,    81,
    1084,   361,   515,    81,  2020,  1080,  1091,   199,   239,  1081,
    1850,   267,   596,   598,   509,   278,   510,  1450,   443,  1169,
     114,  1076,   444,   445,  2021,  1368,   516,  1237,  1399,   220,
     517,   920,    58,   511,  1077,    91,   311,   209,   152,   625,
     648,    81,  1195,   104,    81,    98,    81,   105,  1246,  1844,
    1400,   113,    81,  1107,  1845,   109,   194,   506,   133,    81,
     195,   196,   512,  1369,   513,   242,    81,   103,   514,   246,
     205,  1122,  2027,  2094,    97,    58,   885,  2013,   114,  2133,
    1105,  1105,  1939,  1940,    58,   446,   515,   893,   262,   714,
      81,    81,   308,    91,  1195,   190,   275,  1370,   142,  1105,
    -820,   142,   207,  -599,  1710,    81,  1695,   970,   950,   447,
     516,   519,   297,  1233,   517,   489,   525,   480,  2026,   379,
      81,   942,   943,   197,   448,  1662,  1662,   306,   922,    81,
      81,   104,   533,    98,   260,   105,   205,   944,   272,   113,
     194,   601,   207,   109,   195,   196,   553,   198,   220,  1243,
      81,   151,  2020,   459,    58,   576,   142,  2060,    81,   525,
     558,  1291,   199,  1843,   166,  1941,   114,   208,    81,    81,
     207,   569,    81,   597,  2028,  2095,  1958,  1105,   631,    81,
     240,    91,  2022,   268,   565,  1696,   845,   279,   457,   670,
    1315,   951,   672,    81,    81,   519,    81,   637,   648,   520,
     935,   142,  1548,   521,  2122,   879,   846,   915,   847,   937,
     553,   884,   967,    81,    81,   662,   831,   221,  1060,  -820,
     977,  1406,   648,    81,   955,   848,   207,  1074,  1410,   648,
      81,    81,  1341,   873,   534,    81,  1075,   961,   525,  1419,
    1019,   877,  1306,   549,   142,   552,   962,  1106,  1106,  1474,
     811,   271,  -410,  1277,   849,  -411,   850,  1661,   299,  1709,
     851,   563,  1460,  1712,  2086,   965,  1106,   476,  1084,  1929,
    1420,  1519,  1475,    81,   208,   188,    81,   678,   852,  1526,
    1354,   845,  1925,   520,  1355,  1926,   592,   521,    98,   481,
    1697,  1076,  1408,   523,   113,  1927,  1867,   631,   109,  1746,
    1437,   846,   853,   847,  1346,  1490,   854,   562,  1281,   552,
     720,  2013,  1470,  1553,   208,   721,   149,  2026,   281,   209,
     848,   114,   282,  -410,   753,   286,  -411,   291,    58,  1868,
    1237,  1421,   443,   262,  1432,  1433,   444,   445,  1963,  1964,
     476,    81,   208,   344,  1106,  1554,   597,   501,   201,   849,
    -789,   850,  1379,   480,  2026,   851,   566,  1085,  1698,  1586,
    1587,  1088,  1939,  1940,    81,    81,   754,   663,   174,   174,
    1101,  1102,   664,   852,   160,   976,    81,    81,   979,   980,
    1105,   981,   293,  1277,  1662,    81,  1436,   489,   159,  2142,
     983,   942,   943,   985,   986,   987,  2085,   853,   865,   446,
      58,   854,   298,  1284,   174,    81,   934,   944,   604,   798,
    1166,    58,   525,   532,   920,  2142,  1233,    81,   205,  1651,
     540,  2087,  2088,   447,  2135,   619,  1114,   443,   593,   480,
    1370,   444,   445,   298,  1461,  1759,  1761,  1763,   448,  1195,
     361,   557,    81,  2179,   797,  1968,  2037,  2038,    81, -1101,
    1434,    58,   568,   202,   174,   616,   262,   174,   892,   617,
     663,   996,    58,  1758,   619,   664,  1512,  1619,    63,    64,
     235,  2034,   174,   648,   476,   281,  1419,  1419,  1419,   370,
     789,   755,   631,   865,   525,   756,   866,    81,  1336,    81,
     867,  1061,    58,  1460,   298,   525,   921,   470,  1841,   165,
      81,   912,    81,  1849,  1124,    58,    81,  1420,  1420,  1420,
     648,  1653,  -651,  1261,   480,   476,    81,    77,   133,  -651,
      81,    81,  1262,   712,   297,   481,   449,   103,   525,  1662,
    1147,  1082,    58,   174,    97,   635,  1557,   476,   476,  1045,
    1429,   425,  1089,    58,  1095,  1578,   635,  1106,   179,   956,
    1474,  1115,  1110,    81,  1477,    58,   476,   996,   242,  1430,
     281,   282,   913,   652,  1645,   291,    81,  2043,  1421,  1421,
    1421,   866,  1135,  1475,   153,   867,   525,  1478,   592,   262,
      98,   174,   174,  1646,   275,  1139,   113,  2079,   811,   525,
     109,   104,   174,    98,  1699,   105,    20,    58,  1330,   113,
      58,   481,   697,   109,   181,  1326,   579,   174,   939,   584,
      58,   161,  1143,   114,   162,   163,   525,   164,  1650,   576,
      81,   781,    81,  1382,    81,   782,   114,   525,    81,   793,
    1429,    81,  1811,   525,   476,  1386,   174,   628,   563,   525,
     651,    91,  1807,   174,   174,  1660,  1676,  1360,   174,  1716,
      58,   570,  1148,    58,   628,   722,    81,   874,   628,  1684,
     723,  1808,  1871,   182,  1645,   878,   582,    19,    58,  1957,
      14,    15,    16,    17,    18,   190,  1901,  1390,  1902,  1471,
    1510,   525,   886,  1810,   635,  2069,   174,  1816,  2070,   174,
    1563,  1498,  1796,   894,   525,  1868,   906,  1809,   697,   996,
     907,    81,   996,    81,   142,  1431,  1817,   875,  1662,  2112,
      52,    53,    54,    55,   213,    81,   713,   612,  1879,  1880,
     920,  1851,    81,  1833,  1834,  1835,   344,    -3,   489,    58,
    1572,    81,   887,  1576,   525,   227,  1662,   635,   251,   891,
      81,    81,    81,   895,  1280,  1836,   613,   614,  2080,   270,
    1543,  1756,   525,   148,   743,   744,   628,    65,    66,    67,
      68,    69,    70,    71,    72,  1016,  1816,    81,  1331,  1930,
     989,   190,   297,   215,   449,  1662,   281,   174,  -714,   187,
    2036,   990,   991,   939,   216,  1924,  -476,  1585,  1931,   174,
     174,  -480,  2049,    14,    15,    16,    17,    18,   745,   746,
     217,   996,   293,  1502,  1503,  1017,    81,    81,   489,   712,
      58,  2114,  1367,   295,  1816,   712,   297,   261,    14,    15,
      16,    17,    18,   361,   712,  1833,  1834,  1835,   283,   103,
     290,   298,   292,  1934,   996,  1748,    97,   476,  1374,   996,
     190,   648,   955,   712,  1110,   470,  -410,  1836,  -972,  2148,
    1397,   996,    58,  2032,   312,  -972,  1837,  1501,  1310,    81,
      74,  2150,  1353,   811,   161,  1311,  2105,   162,   163,  1545,
     164,   261,   678,  1975,   290,   292,  1326,    58,  1976,   534,
     774,   858,  1996,   525,   525,  2127,  1527,   332,  1681,   262,
    2128,    79,    80,  1235,  -809,    98,   714,   105,  2194,   470,
     149,   113,   563,  2195,  1660,   109,   200,    64,    81,   969,
    1249,  1491,   374,   617,  -481,   971,    81,   628,   470,   617,
      74,   832,   833,   261,   377,   834,   262,  1626,   114,   174,
     920,   736,  1514,   931,   933,  1561,   378,    74,   737,   738,
     634,   628,   479,    91,   635,    81,   972,   379,   489,   995,
     973,    79,   636,   996,   628,   575,    64,   634,  1248,   252,
     253,   635,   254,  1743,   637,   450,   959,   255,    79,   636,
     604,    81,   449,  -477,   525,   451,  1065,    81,    81,   174,
     525,  1754,   452,    14,    15,    16,    17,    18,  2074,  1536,
     261,   453,   290,   292,    14,    15,    16,    17,    18,   454,
    1155,   376,   376,  1082,   455,   449,   142,   635,    81,   449,
     425,   525,  1163,  1525,    74,  -478,  1167,   483,  1119,    74,
    1170,   142,  1120,   497,   261,    14,    15,    16,    17,    18,
     261,  1531,   344,  1005,   634,  1007,   297,  1010,   635,   634,
     525,  1018,    58,   635,  1022,    79,    80,   620,   293,  1676,
      79,   636,   470,    58,  1663,   534,  1157,   484,  1558,   525,
     996,  1426,   261,  1159,   503,  1740,  1168,   996,   653,  1047,
     292,  1247,  1686,  1687,  1688,  1689,  1690,   476,   476,   499,
     907,   604,   298,   489,    58,   525,    81,    81,    81,   502,
    1325,   739,   740,   470,  1326,   425,   425,   504,  1594,  1595,
     631,  1608,   103,  1588,  1497,   505,   696,   522,   782,    97,
    1530,   489,  1751,   523,  1326,  2163,  1752,    81,  1738,  2167,
      14,    15,    16,    17,    18,  1744,   545,  1826,   174,   361,
      81,  1326,   103,    81,    81,   174,  1827,    81,   546,    97,
     996,    74,  1755,  1853,   556,   267,   278,   996,    81,   212,
    1854,  1249,  1855,  1123,  1120,  1125,   996,   220,  1935,   261,
    1422,  1797,   782,   747,   748,   525,  1235,  2067,    98,   586,
     105,  2029,    79,    80,   113,   996,  2131,   623,   109,    58,
    1326,    81,  2132,   741,   742,   261,   996,   653,   292,  2216,
     567,  2222,   608,  2213,    81,  2223,  1235,   655,    98,  1248,
     105,   114,   696,  1913,   113,  1024,  1025,  1026,   109,   724,
     489,   725,   726,   727,   664,   212,    91,   671,    81,  1177,
     174,   174,   262,  1031,  1032,  1033,  1034,   275,  2092,   682,
     628,   114,   683,   651,   686,   261,  1793,  1794,  1795,    74,
     728,   687,   344,   729,   730,   688,    91,   692,   731,   732,
      81,  1411,  1412,  1413,   716,   441,  2092,   563,   749,   774,
     261,   735,   142,   525,   750,   261,   752,   261,   260,   272,
      79,    80,  1279,   751,  1833,  1834,  1835,   202,   716,   142,
    1852,   757,   470,  1888,   623,   716,   996,   783,   261,    -3,
     261,   261,   784,  1409,   785,  2145,  1836,   786,  1426,  1426,
    1426,   456,  1642,  1628,  1426,  1842,  1435,   787,   261,   142,
     998,   999,  1247,  1663,   788,   508,   815,   268,   279,  -479,
     261,    81,   -18,  1454,   554,    81,  1097,  1098,    81,   828,
     712,   829,   142,  1099,  1100,   509,   839,   510,   869,   361,
    1313,  1120,  1890,   261,   855,   653,   292,   856,   489,  1819,
    1819,  1819,   857,  1897,   511,  -122,  -122,  -122,  -122,  -122,
    -122,   103,   103,   859,  1824,  1328,  1329,   261,   653,   996,
    1332,   489,   489,   860,   261,  -121,  -121,  -121,  -121,  -121,
    -121,    81,   271,   512,  1467,   513,  -157,  -157,   554,   514,
    1643,  1470,  1471,   565,  1644,  1099,  1489,  1422,  1422,  1422,
     153,  1624,  1625,  1629,  1551,  1552,   861,   515,   633,   862,
     148,   863,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,   864,   489,   870,  1665,  1665,    98,    98,   105,
     105,   516,   871,   113,   113,   517,   305,   109,   109,  1556,
    1552,   489,   908,  1536,  1560,  1552,    81,   923,   152,  1071,
    1544,    81,    81,    81,  1596,  1544,   344,  1979,  1980,  1588,
     114,   114,  1812,    14,    15,    16,    17,    18,   993,   889,
     563,  1071,  1610,  1764,  1120,    91,    91,   673,  1990,  1991,
     148,  1862,   890,  1468,    65,    66,    67,    68,    69,    70,
      71,    72,   845,   476,   476,  -597,   174,   212,  -595,   174,
     174,   174,  1899,  1120,  1642,  1900,  1552,  1910,  1911,  1642,
     925,   142,   846,   899,   847,  1249,   562,   519,   929,  1588,
    1922,   996,   945,    81,   174,  2000,  1552,   633,    81,    76,
     174,   848,   824,   947,    81,   637,    81,   964,   142,   142,
    1989,  2001,  1552,   584,    81,  2213,  2214,  1939,  1940,  2055,
     174,   968,   674,   361,  1549,  1550,   489,  1652,  1654,   628,
     849,   151,   850,  1248,   974,   566,   851,   975,   675,   489,
     676,   677,    65,    66,    67,    68,    69,    70,    71,    72,
    1027,  1028,  1035,  1036,   852,  1029,  1030,   997,   470,  1000,
    2134,  2136,  1643,  1044,   174,  2075,  1644,  1643,  1711,  1713,
      58,  1644,  1820,  1821,  1003,   520,  1714,  1532,   853,   521,
     441,   441,   854,  1049,   489,  1741,  1742,  1070,   261,  1071,
     103,  1499,  1500,  1078,  1117,  1126,   142,  -792,   648,  1127,
    2055,   261,   148,  1128,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,  1974,  1129,  1130,  1131,  1833,  1834,
    1835,  1132,   262,  1133,  1134,   261,  1149,   275,  1158,    81,
     996,    81,  1160,  1164,  -690,   425,   261,  1171,  2017,  1172,
    1836,  1173,  1238,  2137,  1255,   261,  1247,  1271,  1239,  -186,
    1351,    76,  1284,  1272,  1665,  1273,    98,  1283,   105,  1289,
    1286,  1292,   113,  1288,  1296,   865,   109,  1293,   260,   272,
      14,    15,    16,    17,    18,  1305,  1295,  1249,  1297,  1298,
     536,    81,   957,  1299,    81,  1301,  1302,  2121,  1303,   114,
    1308,   476,  1309,  1373,  1316,   489,  1317,  1333,  1338,   489,
    1339,  1340,  1347,  1348,    91,  1349,   441,  1350,   174,  1358,
    -678,   174,  -677,   489,  1398,  1403,  1588,  1381,  -793,    19,
    1427,  1428,  1438,   489,   490,  1248,  1441,  1458,  1442,  2061,
    1451,  1452,   142,  1453,  1462,   103,   489,  1486,   489,   489,
     489,   489,   261,  -713,   996,  1464,    81,    81,  1483,  1487,
    1485,  1528,  1544,   866,   174,   174,  1542,   867,    48,    49,
      50,    51,    52,    53,    54,    55,   261,   142,   470,   508,
     183,     6,     7,     8,     9,    10,    11,    12,    13,  1493,
    1495,   425,   271,   425,  2140,  1856,  2017,  1642,   142,   509,
     563,   510,  1559,  1571,  1584,  1589,  1590,  1592,  1591,  1665,
    1552,    98,  1597,   105,  1600,  1615,    81,   113,   511,  1616,
    1617,   109,   489,  1620,  1656,  1631,   489,   441,  1431,  1471,
    1700,   489,   425,  1633,  1634,  2165,  1677,  1702,  1678,  2057,
    1680,   280,  1705,  1682,   114,  1694,   562,   512,  1247,   513,
    1706,  1703,  1704,   514,  1195,  1717,   489,  1719,  1720,    91,
    1722,  1723,  1731,  1724,  1725,  1726,  2189,  1727,  1745,  1749,
    1728,   515,   148,  1729,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,   780,  1643,  1736,  2061,  1757,  1644,
    1750,  2061,  2061,  1753,  1765,   516,  1766,  1779,  1596,   517,
     791,  1770,  1771,   794,   449,  1781,   489,  1791,  1792,  1806,
     489,  1636,  1828,  1830,   425,  1859,   221,  2215,  1861,  1881,
    2057,  1887,   142,   489,   103,   194,   601,  2192,  1885,   195,
     196,   261,  1886,  1889,    81,  1891,    81,   174,  1898,  1893,
    1904,  1947,  1905,    84,  1908,  1443,   150,  1909,  1915,   174,
    1919,  1920,   103,  1921,  2206,  1952,  1953,  1965,  2206,  1967,
     536,  1971,   174,  1993,  1977,  1978,   261,  1981,  1983,  1973,
     441,  1994,   261,   489,  1997,  1995,  2217,  1998,  1999,  -679,
     519,  2031,  2007,  2008,  2009,  2010,  2011,  2012,  1665,  2033,
      98,   103,   105,   525,  -580,  2039,   113,  2044,    81,    81,
     109,    84,  2042,  2058,   825,  2071,   490,   174,  2072,   489,
    2073,   207,   489,  2050,   489,  2059,  1665,   191,    98,  2076,
     105,   489,   865,   114,   113,  2077,    84,  2078,   109,  2089,
    1911,   905,    14,    15,    16,    17,    18,  2098,    91,   231,
    2115,    81,   259,  2111,  2113,  2124,    84,  2125,  2126,  2129,
     489,   114,   489,   480,  2130,  1665,   489,    98,   489,   105,
    2138,  2147,  2149,   113,  2151,  2160,    91,   109,   520,   148,
    2164,  2171,   521,    65,    66,    67,    68,    69,    70,    71,
      72,   489,  2166,   150,  2173,  2174,  2180,  2191,  2201,    84,
     114,    58,   150,    81,  2190,   315,   323,  2203,  2193,  2204,
    2208,   142,    81,  2209,  2211,    91,  2220,  2221,   343,  2224,
     866,   992,  1895,  1555,   867,  1037,  1039,  1466,   174,  1038,
     261,  1041,   174,  1457,   689,  1040,   772,  2141,  2202,   142,
    1863,  2158,   432,   191,   191,  1969,   174,  1870,  1962,  2188,
     189,  1872,  2117,  2116,   150,   462,   174,  1857,   259,   733,
     734,    74,  1858,  2168,  2210,   171,  1484,   288,  2015,   174,
    1630,   174,   174,   174,   174,   555,  2084,    58,   142,   174,
     733,  1797,   231,   231,  1282,   525,  1481,   835,   263,   261,
    1116,  1257,    79,    80,     3,  1290,   927,  1876,  1052,   284,
     287,  1053,  1054,   315,  1790,     0,     0,     0,     0,   148,
     733,    84,     0,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,   259,     0,     0,     0,     0,
       0,     0,     0,   780,   780,     0,     0,    74,     0,     0,
       0,     0,   263,  1063,     0,   174,  1066,     0,     0,   174,
       0,     0,     0,     0,   174,     0,   628,    75,    76,   323,
       0,     0,     0,     0,     0,   323,   315,   315,    79,    80,
       0,     0,     0,     0,     0,   150,     0,     0,   148,   174,
     172,   173,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   263,   343,   638,   647,     0,     0,
       0,     0,     0,     0,     0,  2205,     0,   536,     0,  1440,
     441,     0,   343,     0,  1137,     0,   343,     0,  1141,  2212,
     957,     0,  1145,     0,     0,     0,     0,     0,     0,   174,
       0,     0,     0,   174,   905,     0,     0,   628,  1156,     0,
       0,     0,     0,     0,     0,     0,   174,     0,     0,     0,
     432,  1445,     0,     0,     0,     0,     0,   490,   148,  2123,
     825,   263,    65,    66,    67,    68,    69,    70,    71,    72,
    1318,   261,     0,     0,  1319,     0,  1320,     0,     0,     0,
     327,   328,   329,   330,   432,     0,     0,   775,     0,     0,
       0,     0,     0,     0,   191,   263,   174,     0,     0,   905,
       0,   263,     0,  1270,     0,     0,     0,    76,     0,     0,
     150,     0,     0,     0,   462,     0,     0,     0,   804,   148,
     647,   172,   173,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   174,   263,     0,   174,     0,   174,     0,     0,
       0,     0,     0,   148,   174,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   148,     0,   231,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1318,     0,
     231,   331,  1319,   174,  1320,   174,     0,   698,     0,   174,
       0,   174,     0,  1023,     0,     0,     0,     0,     0,   332,
       0,     0,     0,     0,     0,   315,     0,   432,   432,     0,
       0,   315,  1447,   343,   174,    76,     0,     0,  1547,   905,
       0,     0,     0,  1337,   780,     0,  2218,     0,     0,     0,
       0,     0,   261,     0,     0,  2225,   905,   905,     0,     0,
    1344,  1345,     0,     0,     0,     0,     0,  1321,     0,   261,
       0,     0,   315,     0,     0,  1321,     0,     0,     0,     0,
       0,     0,     0,   315,     0,   315,   263,   343,     0,    84,
       0,   148,   251,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,   698,  1321,   343,   462,   490,   647,     0,
       0,     0,     0,     0,     0,  1384,   638,     0,  1388,     0,
     638,     0,  1392,     0,     0,     0,   148,  1707,  1708,   343,
      65,    66,    67,    68,    69,    70,    71,    72,  1008,   647,
       0,     0,   343,     0,     0,     0,     0,   610,     0,     0,
       0,     0,   116,   150,     0,   116,   183,     6,     7,     8,
       9,    10,    11,    12,    13,   261,   432,  1321,   263,   150,
     150,     0,   432,     0,     0,     0,     0,     0,  1009,     0,
       0,   432,     0,     0,   150,   150,   150,     0,     0,   263,
       0,     0,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   263,
     116,     0,     0,     0,     0,     0,   441,     0,     0,     0,
     148,    74,   367,   368,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,   116,     0,     0,     0,     0,
     462,  1635,    76,     0,   263,     0,     0,     0,  1636,     0,
       0,   265,    79,    80,     0,   116,   775,   775,     0,     0,
       0,     0,  1300,     0,   432,     0,     0,  1304,   263,     0,
       0,    77,     0,     0,     0,   263,   369,     0,  1312,     0,
       0,   462,     0,     0,   804,     0,   804,     0,     0,     0,
       0,     0,   116,   261,     0,     0,     0,     0,   116,     0,
       0,   116,     0,   343,   343,   265,     0,     0,     0,     0,
       0,     0,  1829,     0,     0,     0,   339,   116,   371,     0,
       0,     0,   343,     0,   315,  1840,     0,  1565,     0,     0,
     905,   905,     0,     0,  1567,  1568,  1574,     0,     0,     0,
       0,   436,     0,     0,   905,   905,     0,     0,  1582,  1583,
     315,     0,     0,   116,   436,     0,   148,   265,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   490,
    1874,     0,     0,     0,     0,     0,     0,  1321,     0,     0,
       0,     0,   905,   905,   905,     0,  1605,  1606,  1607,     0,
       0,   432,     0,     0,     0,     0,     0,     0,     0,   261,
     343,     0,     0,     0,     0,     0,   150,   432,   116,     0,
     116,     0,   932,     0,     0,     0,     0,     0,   343,     0,
    1265,     0,     0,     0,   265,     0,     0,     0,   148,     0,
       0,   638,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,   580,     0,     0,     0,     0,
       0,     0,     0,   116,     0,     0,    74,     0,   265,     0,
     689,     0,     0,     0,   265,     0,     0,     0,     0,     0,
     462,     0,     0,     0,   116,     0,  1072,    76,     0,     0,
     635,  1938,     0,     0,     0,  1948,     0,    79,    80,     0,
    1152,     0,     0,     0,   116,     0,   265,   116,     0,  1961,
       0,     0,     0,     0,     0,     0,     0,   490,     0,  1970,
       0,   116,     0,     0,     0,   116,     0,     0,     0,   263,
       0,     0,  1982,     0,  1984,  1985,  1986,  1987,     0,     0,
       0,     0,   263,     0,     0,     0,     0,   775,     0,     0,
       0,     0,     0,   733,     0,     0,     0,     0,     0,   436,
       0,     0,     0,     0,   804,     0,   263,     0,     0,     0,
     148,   804,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,    14,    15,    16,    17,    18,     0,     0,  1537,
    1538,  1539,   490,   436,     0,     0,  1540,  1541,     0,     0,
     905,   905,   905,     0,  1783,  1784,  1785,     0,  2025,     0,
     490,     0,  2030,   343,     0,  1321,     0,  2035,     0,   116,
    1321,  1321,  1321,   436,  1801,     0,  1278,     0,     0,   265,
     759,   760,   761,   762,   763,   764,   765,   766,   767,   768,
     769,    58,  2065,     0,   215,   148,  1825,     0,     0,    65,
      66,    67,    68,    69,    70,    71,    72,   150,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,     0,     0,
       0,   770,     0,   148,   432,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,  2093,  1351,    76,     0,  2096,   432,     0,     0,
       0,    74,     0,     0,   432,     0,   436,   436,     0,  2110,
       0,   265,   116,     0,     0,     0,     0,     0,     0,     0,
       0,   802,    76,     0,     0,   635,   259,    84,     0,     0,
     905,     0,    79,   803,  1882,     0,     0,     0,     0,     0,
       0,   315,     0,     0,   116,     0,     0,   150,     0,   116,
       0,     0,   265,   116,     0,   116,     0,   462,     0,  2146,
       0,     0,     0,     0,     0,   905,   116,     0,   116,  1903,
     905,   905,     0,     0,  1906,  1907,     0,     0,     0,     0,
       0,     0,   371,     0,   116,   436,   462,   265,     0,     0,
     150,     0,     0,     0,     0,  2169,  1801,  1801,  2170,     0,
    2172,     0,     0,     0,     0,     0,     0,  2176,   116,     0,
       0,   265,     0,     0,     0,   580,     0,     0,   265,     0,
       0,   116,     0,   963,     0,     0,     0,     0,  1321,     0,
    1321,     0,   116,     0,     0,     0,  2196,     0,  2197,     0,
      58,     0,  2200,     0,  2176,   436,     0,     0,   116,   116,
       0,   436,     0,   343,   343,     0,     0,     0,     0,     0,
     436,     0,   263,   116,   116,   116,     0,  2200,     0,     0,
       0,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,   263,     0,     0,
      74,     0,     0,   150,   150,   150,   150,     0,   150,   150,
       0,     0,     0,     0,  1637,   323,     0,     0,     0,   436,
    2119,    76,     0,     0,   525,     0,     0,  1801,     0,     0,
       0,    79,    80,   432,   432,     0,     0,     0,     0,     0,
       0,     0,     0,   436,     0,     0,   148,     0,    58,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1318,     0,
     436,     0,  1319,     0,  1320,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   259,     0,     0,     0,     0,     0,
     148,     0,   116,   116,    65,    66,    67,    68,    69,    70,
      71,    72,     0,  1864,  1866,    76,   462,     0,  1760,     0,
       0,   116,     0,     0,     0,     0,     0,     0,    74,  2082,
       0,     0,     0,  1801,     0,     0,     0,     0,     0,     0,
       0,   150,     0,   638,     0,     0,     0,     0,    75,    76,
       0,  1613,     0,  1153,     0,     0,     0,   116,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   263,     0,     0,  1801,     0,     0,     0,     0,     0,
       0,     0,   265,     0,     0,     0,     0,     0,     0,     0,
     436,     0,     0,     0,     0,   265,     0,   110,     0,   116,
       0,     0,     0,     0,     0,   116,   436,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,   116,     0,  1267,
     436,     0,   116,     0,     0,     0,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,  1637,  1798,  1801,  1801,
       0,  1637,     0,   432,     0,     0,     0,  1637,     0,  1637,
       0,     0,     0,     0,    58,   110,     0,   905,     0,     0,
       0,  2175,     0,     0,     0,     0,     0,    58,     0,   436,
       0,     0,     0,     0,     0,     0,   323,   150,     0,     0,
       0,  1801,     0,     0,     0,     0,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   148,
     274,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   432,   148,    74,   575,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,    74,     0,     0,
     343,     0,   116,   150,   230,    76,     0,    14,    15,    16,
      17,    18,     0,   110,     0,    79,    80,  2119,    76,   116,
     116,   525,     0,     0,     0,     0,     0,     0,    79,    80,
       0,     0,   349,     0,   150,  1046,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2066,     0,   343,
     343,     0,     0,     0,    74,     0,    58,     0,     0,   469,
       0,   100,   116,     0,   154,     0,     0,     0,     0,  1798,
    1798,     0,   263,     0,  1635,    76,     0,     0,     0,     0,
       0,  1636,     0,     0,  1637,    79,    80,  1637,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   323,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,   116,    74,   432,     0,   100,
       0,     0,   148,   436,   200,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   230,    76,   572,     0,
       0,     0,     0,     0,   206,     0,   436,    79,    80,     0,
     315,    58,     0,   436,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,    76,     0,     0,   824,   265,   116,     0,     0,     0,
       0,     0,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   116,     0,     0,   627,
    1798,   307,   274,     0,     0,     0,   436,   100,     0,  1637,
    1267,    74,     0,     0,     0,     0,   627,     0,     0,     0,
     627,     0,  1522,     0,     0,     0,   345,     0,     0,     0,
       0,   314,    76,     0,   116,   436,     0,     0,     0,   116,
     263,     0,    79,    80,   150,     0,     0,     0,     0,     0,
       0,   442,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,   307,   468,    65,    66,    67,    68,    69,    70,
      71,    72,  1318,     0,     0,     0,  1319,   343,  1320,     0,
       0,     0,     0,   116,   116,     0,  1798,     0,     0,     0,
       0,   518,     0,     0,     0,     0,   150,   116,   116,     0,
       0,     0,   116,   116,     0,     0,     0,     0,     0,    76,
       0,   543,  1762,     0,     0,     0,   548,   550,   627,   206,
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
     110,     0,     0,     0,     0,    58,  1635,    76,     0,     0,
       0,   469,     0,   110,   263,     0,     0,    79,    80,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,   627,
     469,     0,    14,    15,    16,    17,    18,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   148,   627,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,    74,   627,     0,   307,     0,
       0,     0,   626,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,   314,    76,     0,     0,     0,
       0,    58,     0,   116,   116,   116,    79,    80,     0,     0,
     802,    76,     0,     0,   635,     0,     0,     0,     0,     0,
       0,    79,   803,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   436,   148,   637,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   148,   116,
     577,   578,    65,    66,    67,    68,    69,    70,    71,    72,
       0,    74,     0,     0,     0,   265,   116,     0,     0,     0,
       0,   468,     0,     0,   469,     0,     0,     0,     0,     0,
       0,  1635,    76,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,     0,     0,    77,
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
      70,    71,    72,   148,   115,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,     0,     0,   265,   349,     0,     0,     0,     0,  2119,
      76,     0,     0,   525,     0,     0,   436,     0,     0,   276,
      79,    80,   627,     0,    77,   274,     0,   349,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   115,     0,     0,  1055,     0,     0,     0,     0,
     175,   178,     0,     0,   469,     0,   230,    76,     0,     0,
       0,   353,     0,     0,     0,     0,     0,    79,    80,   938,
       0,     0,     0,   148,  1079,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   223,     0,     0,     0,
       0,   468,   468,     0,     0,     0,     0,     0,   471,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
     468,     0,     0,     0,     0,     0,     0,   349,     0,     0,
       0,   314,    76,   116,     0,     0,     0,     0,     0,     0,
       0,     0,    79,    80,   349,   349,   309,     0,     0,   310,
       0,     0,     0,     0,     0,     0,   901,     0,     0,     0,
       0,     0,     0,     0,   333,     0,   116,     0,     0,     0,
       0,     0,   121,     0,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,  1234,
       0,     0,     0,     0,     0,     0,     0,   349,   468,     0,
       0,     0,     0,     0,   154,     0,     0,   115,     0,     0,
       0,     0,     0,   116,   116,     0,   657,   265,     0,  1269,
       0,   901,     0,     0,     0,   500,  1275,     0,     0,     0,
     121,     0,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   629,     0,
       0,   276,   116,     0,     0,   121,     0,     0,   110,     0,
       0,     0,     0,     0,     0,   629,     0,     0,   345,   629,
       0,     0,     0,   559,   560,   121,     0,     0,     0,     0,
       0,     0,   718,   289,   175,    77,   399,   148,   110,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,   175,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   274,   121,     0,     0,     0,     0,     0,   121,     0,
       0,   121,     0,     0,     0,     0,     0,     0,   606,     0,
       0,   901,     0,     0,     0,   609,   611,     0,     0,     0,
     618,   627,     0,     0,     0,     0,     0,     0,   901,   901,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,   629,     0,   349,
     469,     0,     0,   121,     0,     0,     0,     0,   333,     0,
       0,   333,     0,   148,     0,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
     148,   468,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,   349,   349,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
     121,   479,   349,   349,     0,   121,     0,   349,   349,     0,
       0,     0,     0,     0,     0,   154,     0,     0,   483,     0,
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
       0,     0,     0,     0,   120,     0,   410,   410,   169,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,  2016,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,  1732,   398,
       0,     0,    77,   399,     0,     0,   360,     0,     0,   400,
      79,    80,   401,   402,   403,   404,     0,     0,     0,     0,
       0,     0,   528,   528,     0,     0,     0,   711,     0,     0,
       0,     0,   528,  1064,     0,   528,  1067,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,     0,     0,
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
     639,   639,     0,     0,     0,     0,     0,   639,     0,     0,
       0,   300,   632,   301,   649,     0,     0,     0,     0,     0,
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
       0,     0,     0,   360,     0,     0,     0,     0,     0,   639,
    1513,     0,  1658,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,   211,     0,   621,   622,     0,
     478,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   654,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2139,     0,
       0,     0,     0,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   528,  1566,     0,     0,
       0,     0,     0,     0,   365,   528,  1575,     0,   639,     0,
       0,     0,     0,   169,     0,     0,     0,     0,     0,   360,
     360,     0,     0,   478,     0,   940,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   632,     0,   169,     0,
       0,     0,   382,     0,   169,   383,     0,   384,   790,   385,
    1946,     0,     0,     0,     0,     0,     0,     0,     0,  1949,
     211,  1951,     0,     0,  1956,  1960,   386,  1672,     0,     0,
       0,     0,  1966,   715,     0,     0,   715,   715,     0,   715,
       0,     0,     0,     0,     0,     0,     0,     0,   715,     0,
       0,   715,   715,   715,   387,   388,     0,   389,   390,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,     0,   393,   394,   395,     0,   396,   397,   868,
     169,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1666,
    1667,  1668,     0,     0,     0,   398,  1847,   478,    77,   399,
       0,     0,   360,     0,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,   434,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,   639,
    2041,   463,     0,     0,     0,  2046,  2048,     0,   478,     0,
       0,     0,     0,     0,   491,     0,   491,     0,     0,     0,
       0,     0,     0,     0,     0,  2068,     0,   169,   169,     0,
     478,   478,     0,     0,     0,   373,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   952,
     953,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   960,     0,  2097,     0,  2100,     0,     0,  2102,
    2104,     0,     0,     0,  2107,  2109,     0,     0,     0,     0,
       0,     0,     0,   528,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   528,
     600,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   816,     0,     0,
       0,     0,     0,   169,  2153,  2155,  2157,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
    2182,  2184,  2186,     0,     0,  1057,  1058,   365,     0,     0,
       0,  1062,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1083,     0,     0,  1086,  1087,     0,  1090,     0,
    1092,  1093,     0,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,   360,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,   528,   528,     0,     0,     0,
       0,     0,  1136,     0,     0,     0,  1140,   386,     0,     0,
    1144,   528,     0,     0,     0,     0,   491,     0,     0,     0,
       0,     0,   491,     0,     0,     0,     0,   837,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
     478,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
    1259,  1260,     0,     0,     0,     0,   398,   836,     0,    77,
     399,     0,     0,     0,  1276,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,   298,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  1011,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,  1042,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1377,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,     0,     0,    77,   399,     0,
       0,     0,  1449,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,  1524,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,  1945,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  1950,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1959,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2045,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2047,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2099,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2101,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2103,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2106,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2108,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2152,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2154,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2156,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2181,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2183,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2185,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     684,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   690,     0,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   699,     0,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,   916,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,   461,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,  2040,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     0,   185,
     186,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   693,     0,   694,   695,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -482,
    -482,     0,  -482,    46,     0,    47,     0,  -482,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    75,    75,     4,   166,    75,    75,   166,   182,   398,
     230,   182,     1,   251,   256,   302,   372,   345,   731,   203,
     343,     1,   177,   716,  1223,   932,   925,   488,     1,  1205,
     230,   647,   230,  1017,   400,  1165,   233,   360,   235,    77,
     218,   364,   638,  1361,  1362,   242,   360,  1798,    59,   230,
     364,   167,   775,   634,   634,    56,    57,   802,    59,   709,
    1797,   968,  1046,  1797,   181,   160,   802,     0,    75,   634,
      59,    84,   167,  1797,    75,   271,     0,    97,   230,    59,
     230,   545,   546,    84,   230,     1,    59,     1,  1205,     1,
       4,    92,    75,     1,   314,    84,    97,     1,  1441,   100,
     805,   166,   230,   104,  1939,   804,   811,    75,    97,   804,
    1676,   100,   309,   310,   314,   104,   314,  1190,   192,   893,
       1,   802,   192,   192,     1,  1109,   230,   909,   146,    89,
     230,   605,    72,   314,   802,     1,   147,   150,     4,   462,
     468,   142,    90,    59,   145,    59,   147,    59,   922,   154,
     168,    59,   153,   822,   159,    59,   230,   230,   147,   160,
     230,   230,   314,   134,   314,    97,   167,   147,   314,    97,
      84,   840,    75,    75,   147,    72,   556,  1928,    59,    75,
     821,   822,    77,    78,    72,   192,   314,   567,   100,   398,
     191,   192,   141,    59,    90,   152,   104,   168,     1,   840,
     160,     4,   191,   160,   152,   206,    98,   671,   804,   192,
     314,   231,   152,   909,   314,   216,   156,   206,  1943,   118,
     221,   802,   802,   230,   192,  1441,  1442,   141,   608,   230,
     231,   147,   240,   147,   100,   147,   150,   802,   104,   147,
     314,   314,   231,   147,   314,   314,   259,   230,    89,   918,
     251,     4,  2087,   202,    72,   293,    59,   156,   259,   156,
     268,   974,   230,  1672,   152,   160,   147,    84,   269,   270,
     259,   279,   273,   152,   177,   177,  1842,   918,   343,   280,
      97,   147,   159,   100,   273,   177,   506,   104,   202,   374,
    1003,   640,   377,   294,   295,   315,   297,   176,   626,   231,
     623,   104,  1319,   231,  2055,   543,   506,   594,   506,   623,
     323,   549,   668,   314,   315,    10,   494,   158,   782,   160,
     686,  1162,   650,   324,   647,   506,   315,  1072,  1169,   657,
     331,   332,  1055,   529,   152,   336,  1072,   660,   156,  1174,
     729,   537,   992,   257,   147,   259,   660,   821,   822,   110,
     466,   104,    89,   949,   506,    89,   506,  1700,   133,  1476,
     506,   273,  1479,  1480,     1,   666,   840,   551,  1073,  2120,
    1174,  1278,   133,   374,   191,    62,   377,   378,   506,  1286,
    1079,   601,  2119,   315,  1079,  2119,   302,   315,   302,   206,
      84,  1072,  1166,    99,   302,  2119,    75,   462,   302,  1529,
    1182,   601,   506,   601,  1072,  1246,   506,   273,    10,   323,
     156,  2162,    91,   123,   231,   161,     4,  2142,   105,   432,
     601,   302,   109,   160,   133,   112,   160,   114,    72,   108,
    1212,  1174,   506,   345,    61,    62,   506,   506,  1847,  1848,
     624,   442,   259,   605,   918,   155,   152,   222,   486,   601,
       0,   601,  1121,   442,  2179,   601,   273,   806,   152,  1777,
    1778,   810,    77,    78,   465,   466,   175,   162,    56,    57,
     819,   820,   167,   601,   152,   684,   477,   478,   687,   688,
    1121,   690,   155,  1079,  1700,   486,  1182,   488,   118,  2090,
     699,  1072,  1072,   702,   703,   704,   134,   601,   518,   506,
      72,   601,   160,   176,    92,   506,   622,  1072,   152,   458,
     890,    72,   156,   239,   988,  2116,  1212,   518,   432,   177,
     246,   158,   159,   506,    75,   337,   827,   601,   303,   518,
     168,   601,   601,   160,  1710,  1552,  1553,  1554,   506,    90,
     605,   267,   543,  2144,   458,   160,  1955,  1956,   549,   151,
     177,    72,   278,   157,   142,   154,   468,   145,   566,   158,
     162,   158,    72,  1547,   376,   167,  1271,  1408,   106,   107,
     590,   168,   160,   901,   758,   262,  1411,  1412,  1413,   167,
     152,   154,   647,   603,   156,   158,   518,   588,  1049,   590,
     518,   152,    72,  1710,   160,   156,   607,   203,  1671,   152,
     601,   590,   603,  1676,   842,    72,   607,  1411,  1412,  1413,
     938,   177,   160,   936,   603,   799,   617,   155,   607,   167,
     621,   622,   936,   398,   152,   442,   154,   607,   156,  1845,
     868,   152,    72,   221,   607,   156,  1329,   821,   822,   756,
     158,   815,   152,    72,   815,  1350,   156,  1121,   152,  1265,
     110,   829,   826,   654,   109,    72,   840,   158,   590,   177,
     347,   348,   590,   350,   158,   352,   667,   168,  1411,  1412,
    1413,   603,   152,   133,   588,   603,   156,   132,   594,   591,
     594,   269,   270,   177,   592,   152,   594,  2005,   804,   156,
     594,   607,   280,   607,   154,   607,    20,    72,   151,   607,
      72,   518,   389,   607,   152,   158,   294,   295,   624,   297,
      72,    58,   152,   594,    61,    62,   156,    64,  1431,   757,
     721,   154,   723,   152,   725,   158,   607,   156,   729,   152,
     158,   732,  1631,   156,   918,   152,   324,   343,   650,   156,
     346,   607,   158,   331,   332,  1441,  1442,  1096,   336,   177,
      72,   280,   869,    72,   360,   156,   757,   532,   364,  1452,
     161,   177,    75,   152,   158,   540,   295,    18,    72,  1842,
      13,    14,    15,    16,    17,   152,  1760,   152,  1762,    92,
     152,   156,   557,   177,   156,  1984,   374,   158,  1987,   377,
     152,  1255,  1624,   568,   156,   108,   154,  1629,   485,   158,
     158,   802,   158,   804,   607,   152,   177,   533,  2024,   168,
      61,    62,    63,    64,   177,   816,  1205,   134,  1725,  1726,
    1294,   177,   823,   146,   147,   148,   988,   158,   829,    72,
     152,   832,   558,   152,   156,   177,  2052,   156,     3,   565,
     841,   842,   843,   569,   960,   168,   163,   164,   152,    69,
    1311,  1544,   156,   104,   128,   129,   462,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   158,   868,  1042,   158,
     154,   152,   152,   149,   154,  2091,   563,   465,   159,    62,
    1953,   165,   166,   799,   160,   177,     3,  1361,   177,   477,
     478,   134,  1965,    13,    14,    15,    16,    17,   172,   173,
     176,   158,   155,  1259,  1260,   156,   907,   908,   909,   684,
      72,   168,  1108,   158,   158,   690,   152,   100,    13,    14,
      15,    16,    17,   988,   699,   146,   147,   148,   111,   909,
     113,   160,   115,   177,   158,  1531,   909,  1121,  1116,   158,
     152,  1269,  1265,   718,  1118,   551,   160,   168,   160,   168,
    1147,   158,    72,   177,   176,   167,   177,  1258,   153,   960,
     132,   168,  1078,  1079,    58,   160,  2039,    61,    62,   151,
      64,   154,   973,   154,   157,   158,   158,    72,   159,   152,
     152,   154,  1889,   156,   156,   154,  1287,   174,  1449,   901,
     159,   163,   164,   909,   160,   909,  1205,   909,   154,   605,
     588,   909,   914,   159,  1700,   909,   106,   107,  1009,   154,
     924,  1249,   152,   158,   134,   154,  1017,   623,   624,   158,
     132,   155,   156,   206,   152,   159,   938,  1416,   909,   617,
    1504,   163,  1274,   621,   622,  1336,   152,   132,   170,   171,
     152,   647,   152,   909,   156,  1046,   154,   118,  1049,   154,
     158,   163,   164,   158,   660,   106,   107,   152,   924,    47,
      48,   156,    50,  1524,   176,   154,   654,    55,   163,   164,
     152,  1072,   154,     3,   156,   154,   152,  1078,  1079,   667,
     156,  1542,   154,    13,    14,    15,    16,    17,  1995,  1298,
     273,   154,   275,   276,    13,    14,    15,    16,    17,   154,
     875,  1256,  1257,   152,   154,   154,   909,   156,  1109,   154,
    1284,   156,   887,  1284,   132,     3,   891,   152,   154,   132,
     895,   924,   158,    22,   307,    13,    14,    15,    16,    17,
     313,  1293,  1294,   721,   152,   723,   152,   725,   156,   152,
     156,   729,    72,   156,   732,   163,   164,   154,   155,  1845,
     163,   164,   758,    72,  1441,   152,   154,   158,  1332,   156,
     158,  1174,   345,   154,   158,  1521,   892,   158,   351,   757,
     353,   924,   111,   112,   113,   114,   115,  1361,  1362,   152,
     158,   152,   160,  1184,    72,   156,  1187,  1188,  1189,   152,
     154,   165,   166,   799,   158,  1369,  1370,   158,  1369,  1370,
    1265,  1397,  1182,  1362,   154,   158,   389,   158,   158,  1182,
     154,  1212,   154,    99,   158,  2124,   158,  1218,  1519,  2128,
      13,    14,    15,    16,    17,  1526,   152,   154,   816,  1294,
    1231,   158,  1212,  1234,  1235,   823,   154,  1238,   152,  1212,
     158,   132,  1543,   154,   160,  1234,  1235,   158,  1249,    84,
     154,  1165,   154,   841,   158,   843,   158,    89,   154,   442,
    1174,   152,   158,   130,   131,   156,  1182,  1980,  1182,   151,
    1182,   154,   163,   164,  1182,   158,   154,   157,  1182,    72,
     158,  1282,   154,   126,   127,   468,   158,   470,   471,   154,
     160,   154,   160,   158,  1295,   158,  1212,   154,  1212,  1165,
    1212,  1182,   485,  1777,  1212,   736,   737,   738,  1212,   121,
    1311,   123,   124,   125,   167,   150,  1182,   176,  1319,   907,
     908,   909,  1234,   743,   744,   745,   746,  1235,  2024,   154,
     936,  1212,   118,   939,   152,   518,  1621,  1622,  1623,   132,
     152,   152,  1504,   155,   156,   152,  1212,   152,   160,   161,
    1351,  1171,  1172,  1173,   158,   190,  2052,  1269,   164,   152,
     543,   169,  1165,   156,   162,   548,   132,   550,  1234,  1235,
     163,   164,   960,   174,   146,   147,   148,   157,   158,  1182,
    1681,   155,   988,  1739,   157,   158,   158,   154,   571,   157,
     573,   574,   154,  1168,   154,  2091,   168,   154,  1411,  1412,
    1413,   156,  1422,  1416,  1417,   177,  1181,   154,   591,  1212,
     163,   164,  1165,  1700,   154,  1635,   134,  1234,  1235,   134,
     603,  1422,   159,  1198,   259,  1426,   157,   158,  1429,   159,
    1205,   158,  1235,   157,   158,  1635,   152,  1635,   152,  1504,
     157,   158,  1743,   626,   154,   628,   629,   154,  1449,  1642,
    1643,  1644,   154,  1754,  1635,    13,    14,    15,    16,    17,
      18,  1441,  1442,   154,  1648,   157,   158,   650,   651,   158,
     159,  1472,  1473,   154,   657,    13,    14,    15,    16,    17,
      18,  1482,  1235,  1635,    78,  1635,   157,   158,   323,  1635,
    1422,    91,    92,  1482,  1422,   157,   158,  1411,  1412,  1413,
    1414,  1415,  1416,  1417,   157,   158,   154,  1635,   343,   154,
     104,   154,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   154,  1524,   176,  1441,  1442,  1441,  1442,  1441,
    1442,  1635,   157,  1441,  1442,  1635,   156,  1441,  1442,   157,
     158,  1542,    70,  1752,   157,   158,  1547,   157,  1414,   157,
     158,  1552,  1553,  1554,   157,   158,  1718,   158,   159,  1718,
    1441,  1442,  1635,    13,    14,    15,    16,    17,    18,   160,
    1482,   157,   158,   157,   158,  1441,  1442,    13,  1879,  1880,
     104,  1698,   160,   177,   108,   109,   110,   111,   112,   113,
     114,   115,  1812,  1777,  1778,   160,  1184,   432,   160,  1187,
    1188,  1189,   157,   158,  1624,   157,   158,   157,   158,  1629,
     152,  1414,  1812,   160,  1812,  1529,  1482,  1637,    78,  1778,
     157,   158,   157,  1624,  1212,   157,   158,   462,  1629,   153,
    1218,  1812,   156,    18,  1635,   176,  1637,   158,  1441,  1442,
    1878,   157,   158,  1231,  1645,   158,   159,    77,    78,  1972,
    1238,   160,    88,  1718,  1320,  1321,  1657,  1432,  1433,  1265,
    1812,  1414,  1812,  1529,   152,  1482,  1812,   177,   104,  1670,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     739,   740,   747,   748,  1812,   741,   742,   154,  1294,   154,
    2069,  2070,  1624,   177,  1282,  1996,  1624,  1629,  1479,  1480,
      72,  1629,  1643,  1644,   160,  1637,  1481,  1295,  1812,  1637,
     545,   546,  1812,   160,  1715,  1522,  1523,   157,   901,   157,
    1700,  1256,  1257,    18,   151,   154,  1529,   151,  2056,   154,
    2053,   914,   104,   154,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1861,   154,   154,   154,   146,   147,
     148,   154,  1664,   154,   154,   938,    22,  1665,   154,  1760,
     158,  1762,   154,   151,   154,  1939,   949,   160,  1939,   160,
     168,   160,    70,  2074,   176,   958,  1529,   154,   177,   177,
     152,   153,   176,   154,  1700,   154,  1700,   151,  1700,   154,
     160,   154,  1700,   160,   154,  1815,  1700,   158,  1664,  1665,
      13,    14,    15,    16,    17,    18,   158,  1721,   154,   158,
     241,  1812,   647,   154,  1815,   154,   154,  2055,   154,  1700,
     154,  2005,   154,   151,   157,  1826,   157,   154,   154,  1830,
     154,   154,   154,   154,  1700,   154,   671,   154,  1426,   157,
     154,  1429,   154,  1844,   154,   158,  2005,   176,   151,    18,
     154,   158,   152,  1854,   216,  1721,   152,    14,   152,  1976,
     152,   152,  1665,   152,    74,  1845,  1867,   157,  1869,  1870,
    1871,  1872,  1055,   159,   158,   177,  1877,  1878,   159,   157,
     177,   151,   158,  1815,  1472,  1473,   160,  1815,    57,    58,
      59,    60,    61,    62,    63,    64,  1079,  1700,  1504,  2119,
       4,     5,     6,     7,     8,     9,    10,    11,    12,   177,
     177,  2085,  1665,  2087,  2085,  1690,  2087,  1937,  1721,  2119,
    1832,  2119,   177,   154,   157,   154,   158,   154,   158,  1845,
     158,  1845,   157,  1845,   154,   157,  1937,  1845,  2119,   154,
     151,  1845,  1943,   151,    80,   152,  1947,   782,   152,    92,
     152,  1952,  2126,   177,   177,  2126,   177,   151,   177,  1972,
     177,    65,   152,   177,  1845,   177,  1832,  2119,  1721,  2119,
     152,   177,   177,  2119,    90,   154,  1977,   151,   151,  1845,
     158,   158,   154,   151,   160,   160,  2160,   157,   151,   154,
     157,  2119,   104,   157,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   435,  1937,   157,  2124,   121,  1937,
     159,  2128,  2129,   159,   151,  2119,   154,   157,   157,  2119,
     451,   154,   154,   454,   154,   154,  2027,   151,   151,   177,
    2031,   159,   152,   154,  2208,   152,   158,  2208,   152,   157,
    2053,   151,  1845,  2044,  2024,  2119,  2119,  2164,   157,  2119,
    2119,  1234,   157,   160,  2055,   151,  2057,  1645,   151,   154,
     154,    75,   154,     1,   154,   177,     4,   154,   157,  1657,
     154,   154,  2052,   154,  2191,    75,   177,   177,  2195,   151,
     511,   152,  1670,   157,   154,   154,  1269,   154,   152,   177,
     925,   157,  1275,  2094,   151,   160,  2213,   151,   151,   154,
    2120,    75,   154,   154,   154,   154,   154,   154,  2024,   168,
    2024,  2091,  2024,   156,   155,   177,  2024,    75,  2119,  2120,
    2024,    59,   168,   159,   486,   151,   488,  1715,   151,  2130,
     154,  2120,  2133,   177,  2135,   177,  2052,    75,  2052,   154,
    2052,  2142,  2162,  2024,  2052,   154,    84,   154,  2052,   151,
     158,   583,    13,    14,    15,    16,    17,   153,  2024,    97,
     151,  2162,   100,   168,   168,   159,   104,   104,   152,   158,
    2171,  2052,  2173,  2162,    75,  2091,  2177,  2091,  2179,  2091,
     151,   168,   168,  2091,   153,   177,  2052,  2091,  2120,   104,
     157,    75,  2120,   108,   109,   110,   111,   112,   113,   114,
     115,  2202,   177,   141,    75,   151,   153,   159,   151,   147,
    2091,    72,   150,  2214,   154,   153,   154,   151,   154,   154,
     152,  2024,  2223,   177,   154,  2091,   154,   177,   166,   177,
    2162,   708,  1752,  1326,  2162,   749,   751,  1212,  1826,   750,
    1423,   753,  1830,  1200,   386,   752,   431,  2087,  2179,  2052,
    1700,  2116,   190,   191,   192,  1853,  1844,  1709,  1845,  2159,
      62,  1712,  2053,  2052,   202,   203,  1854,  1691,   206,   411,
     412,   132,  1691,  2129,  2195,    49,  1238,   112,  1937,  1867,
    1417,  1869,  1870,  1871,  1872,   264,  2014,    72,  2091,  1877,
     432,   152,   230,   231,   964,   156,  1231,   495,   100,  1482,
     829,   928,   163,   164,     0,   973,   617,  1721,   774,   111,
     112,   774,   774,   251,  1612,    -1,    -1,    -1,    -1,   104,
     462,   259,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   774,   775,    -1,    -1,   132,    -1,    -1,
      -1,    -1,   154,   784,    -1,  1943,   787,    -1,    -1,  1947,
      -1,    -1,    -1,    -1,  1952,    -1,  1972,   152,   153,   307,
      -1,    -1,    -1,    -1,    -1,   313,   314,   315,   163,   164,
      -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,   104,  1977,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,   206,   343,   344,   345,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2190,    -1,   848,    -1,  1184,
    1255,    -1,   360,    -1,   855,    -1,   364,    -1,   859,  2204,
    1265,    -1,   863,    -1,    -1,    -1,    -1,    -1,    -1,  2027,
      -1,    -1,    -1,  2031,   876,    -1,    -1,  2053,   876,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2044,    -1,    -1,    -1,
     398,   177,    -1,    -1,    -1,    -1,    -1,   829,   104,  2057,
     832,   273,   108,   109,   110,   111,   112,   113,   114,   115,
     116,  1664,    -1,    -1,   120,    -1,   122,    -1,    -1,    -1,
      65,    66,    67,    68,   432,    -1,    -1,   435,    -1,    -1,
      -1,    -1,    -1,    -1,   442,   307,  2094,    -1,    -1,   941,
      -1,   313,    -1,   941,    -1,    -1,    -1,   153,    -1,    -1,
     458,    -1,    -1,    -1,   462,    -1,    -1,    -1,   466,   104,
     468,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  2130,   345,    -1,  2133,    -1,  2135,    -1,    -1,
      -1,    -1,    -1,   104,  2142,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   104,    -1,   506,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     518,   156,   120,  2171,   122,  2173,    -1,   389,    -1,  2177,
      -1,  2179,    -1,   735,    -1,    -1,    -1,    -1,    -1,   174,
      -1,    -1,    -1,    -1,    -1,   543,    -1,   545,   546,    -1,
      -1,   549,   163,   551,  2202,   153,    -1,    -1,   156,  1051,
      -1,    -1,    -1,  1051,  1055,    -1,  2214,    -1,    -1,    -1,
      -1,    -1,  1815,    -1,    -1,  2223,  1068,  1069,    -1,    -1,
    1068,  1069,    -1,    -1,    -1,    -1,    -1,  1009,    -1,  1832,
      -1,    -1,   590,    -1,    -1,  1017,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   601,    -1,   603,   468,   605,    -1,   607,
      -1,   104,     3,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   485,  1046,   623,   624,  1049,   626,    -1,
      -1,    -1,    -1,    -1,    -1,  1126,   634,    -1,  1129,    -1,
     638,    -1,  1133,    -1,    -1,    -1,   104,  1472,  1473,   647,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   657,
      -1,    -1,   660,    -1,    -1,    -1,    -1,   160,    -1,    -1,
      -1,    -1,     1,   671,    -1,     4,     4,     5,     6,     7,
       8,     9,    10,    11,    12,  1928,   684,  1109,   550,   687,
     688,    -1,   690,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,   699,    -1,    -1,   702,   703,   704,    -1,    -1,   571,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,   591,
      59,    -1,    -1,    -1,    -1,    -1,  1631,    -1,    -1,    -1,
     104,   132,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,
     758,   152,   153,    -1,   626,    -1,    -1,    -1,   159,    -1,
      -1,   100,   163,   164,    -1,   104,   774,   775,    -1,    -1,
      -1,    -1,   984,    -1,   782,    -1,    -1,   989,   650,    -1,
      -1,   155,    -1,    -1,    -1,   657,   160,    -1,  1000,    -1,
      -1,   799,    -1,    -1,   802,    -1,   804,    -1,    -1,    -1,
      -1,    -1,   141,  2056,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,   821,   822,   154,    -1,    -1,    -1,    -1,
      -1,    -1,  1657,    -1,    -1,    -1,   165,   166,   167,    -1,
      -1,    -1,   840,    -1,   842,  1670,    -1,  1338,    -1,    -1,
    1342,  1343,    -1,    -1,  1342,  1343,  1347,    -1,    -1,    -1,
      -1,   190,    -1,    -1,  1356,  1357,    -1,    -1,  1356,  1357,
     868,    -1,    -1,   202,   203,    -1,   104,   206,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,  1311,
    1715,    -1,    -1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,
      -1,    -1,  1394,  1395,  1396,    -1,  1394,  1395,  1396,    -1,
      -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2162,
     918,    -1,    -1,    -1,    -1,    -1,   924,   925,   257,    -1,
     259,    -1,   160,    -1,    -1,    -1,    -1,    -1,   936,    -1,
     938,    -1,    -1,    -1,   273,    -1,    -1,    -1,   104,    -1,
      -1,   949,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,   132,    -1,   307,    -1,
    1182,    -1,    -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,
     988,    -1,    -1,    -1,   323,    -1,   152,   153,    -1,    -1,
     156,  1826,    -1,    -1,    -1,  1830,    -1,   163,   164,    -1,
     872,    -1,    -1,    -1,   343,    -1,   345,   346,    -1,  1844,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1449,    -1,  1854,
      -1,   360,    -1,    -1,    -1,   364,    -1,    -1,    -1,   901,
      -1,    -1,  1867,    -1,  1869,  1870,  1871,  1872,    -1,    -1,
      -1,    -1,   914,    -1,    -1,    -1,    -1,  1055,    -1,    -1,
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,   398,
      -1,    -1,    -1,    -1,  1072,    -1,   938,    -1,    -1,    -1,
     104,  1079,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    13,    14,    15,    16,    17,    -1,    -1,  1301,
    1302,  1303,  1524,   432,    -1,    -1,  1308,  1309,    -1,    -1,
    1602,  1603,  1604,    -1,  1602,  1603,  1604,    -1,  1943,    -1,
    1542,    -1,  1947,  1121,    -1,  1547,    -1,  1952,    -1,   458,
    1552,  1553,  1554,   462,  1625,    -1,   160,    -1,    -1,   468,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,    72,  1977,    -1,   149,   104,  1648,    -1,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,  1165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,    -1,
      -1,   176,    -1,   104,  1182,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2027,   152,   153,    -1,  2031,  1205,    -1,    -1,
      -1,   132,    -1,    -1,  1212,    -1,   545,   546,    -1,  2044,
      -1,   550,   551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,   156,  1234,  1235,    -1,    -1,
    1732,    -1,   163,   164,  1732,    -1,    -1,    -1,    -1,    -1,
      -1,  1249,    -1,    -1,   583,    -1,    -1,  1255,    -1,   588,
      -1,    -1,   591,   592,    -1,   594,    -1,  1265,    -1,  2094,
      -1,    -1,    -1,    -1,    -1,  1767,   605,    -1,   607,  1767,
    1772,  1773,    -1,    -1,  1772,  1773,    -1,    -1,    -1,    -1,
      -1,    -1,   621,    -1,   623,   624,  1294,   626,    -1,    -1,
    1298,    -1,    -1,    -1,    -1,  2130,  1797,  1798,  2133,    -1,
    2135,    -1,    -1,    -1,    -1,    -1,    -1,  2142,   647,    -1,
      -1,   650,    -1,    -1,    -1,   654,    -1,    -1,   657,    -1,
      -1,   660,    -1,   662,    -1,    -1,    -1,    -1,  1760,    -1,
    1762,    -1,   671,    -1,    -1,    -1,  2171,    -1,  2173,    -1,
      72,    -1,  2177,    -1,  2179,   684,    -1,    -1,   687,   688,
      -1,   690,    -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,
     699,    -1,  1234,   702,   703,   704,    -1,  2202,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,  1269,    -1,    -1,
     132,    -1,    -1,  1411,  1412,  1413,  1414,    -1,  1416,  1417,
      -1,    -1,    -1,    -1,  1422,  1423,    -1,    -1,    -1,   758,
     152,   153,    -1,    -1,   156,    -1,    -1,  1928,    -1,    -1,
      -1,   163,   164,  1441,  1442,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   782,    -1,    -1,   104,    -1,    72,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     799,    -1,   120,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1482,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   821,   822,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,  1705,  1706,   153,  1504,    -1,   156,    -1,
      -1,   840,    -1,    -1,    -1,    -1,    -1,    -1,   132,  2010,
      -1,    -1,    -1,  2014,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1529,    -1,  1531,    -1,    -1,    -1,    -1,   152,   153,
      -1,  1403,    -1,   872,    -1,    -1,    -1,   876,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1423,    -1,    -1,  2055,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     909,    -1,    -1,    -1,    -1,   914,    -1,     1,    -1,   918,
      -1,    -1,    -1,    -1,    -1,   924,   925,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,   936,    -1,   938,
     939,    -1,   941,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1482,    -1,    -1,    -1,    -1,    -1,  1624,  1625,  2119,  2120,
      -1,  1629,    -1,  1631,    -1,    -1,    -1,  1635,    -1,  1637,
      -1,    -1,    -1,    -1,    72,    59,    -1,  2139,    -1,    -1,
      -1,  2139,    -1,    -1,    -1,    -1,    -1,    72,    -1,   988,
      -1,    -1,    -1,    -1,    -1,    -1,  1664,  1665,    -1,    -1,
      -1,  2162,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   104,
     104,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  1700,   104,   132,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,   132,    -1,    -1,
    1718,    -1,  1051,  1721,   152,   153,    -1,    13,    14,    15,
      16,    17,    -1,   147,    -1,   163,   164,   152,   153,  1068,
    1069,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,    -1,   166,    -1,  1752,   156,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1979,    -1,  1777,
    1778,    -1,    -1,    -1,   132,    -1,    72,    -1,    -1,   203,
      -1,     1,  1121,    -1,     4,    -1,    -1,    -1,    -1,  1797,
    1798,    -1,  1664,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,    -1,    -1,  1812,   163,   164,  1815,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,  1832,    -1,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1174,   132,  1845,    -1,    59,
      -1,    -1,   104,  1182,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,   152,   153,   282,    -1,
      -1,    -1,    -1,    -1,    84,    -1,  1205,   163,   164,    -1,
    1878,    72,    -1,  1212,    -1,    -1,    -1,    -1,   302,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,   153,    -1,    -1,   156,  1234,  1235,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,  1255,    -1,    -1,   343,
    1928,   141,   346,    -1,    -1,    -1,  1265,   147,    -1,  1937,
    1269,   132,    -1,    -1,    -1,    -1,   360,    -1,    -1,    -1,
     364,    -1,  1281,    -1,    -1,    -1,   166,    -1,    -1,    -1,
      -1,   152,   153,    -1,  1293,  1294,    -1,    -1,    -1,  1298,
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
     624,    -1,    13,    14,    15,    16,    17,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   104,   647,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   132,   660,    -1,   458,    -1,
      -1,    -1,   462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    72,    -1,  1602,  1603,  1604,   163,   164,    -1,    -1,
     152,   153,    -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1631,   104,   176,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,   104,  1648,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   132,    -1,    -1,    -1,  1664,  1665,    -1,    -1,    -1,
      -1,   551,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,   155,
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
     113,   114,   115,   104,    59,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1832,   918,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,   156,    -1,    -1,  1845,    -1,    -1,   104,
     163,   164,   936,    -1,   155,   939,    -1,   941,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   758,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,   147,    -1,    -1,   775,    -1,    -1,    -1,    -1,
      56,    57,    -1,    -1,   988,    -1,   152,   153,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,   163,   164,   799,
      -1,    -1,    -1,   104,   804,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    92,    -1,    -1,    -1,
      -1,   821,   822,    -1,    -1,    -1,    -1,    -1,   203,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     840,    -1,    -1,    -1,    -1,    -1,    -1,  1051,    -1,    -1,
      -1,   152,   153,  1972,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,  1068,  1069,   142,    -1,    -1,   145,
      -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,  2005,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,    -1,    -1,    -1,   909,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1121,   918,    -1,
      -1,    -1,    -1,    -1,   924,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,  2052,  2053,    -1,   936,  2056,    -1,   939,
      -1,   941,    -1,    -1,    -1,   221,   946,    -1,    -1,    -1,
      59,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   343,    -1,
      -1,   346,  2091,    -1,    -1,    84,    -1,    -1,  1182,    -1,
      -1,    -1,    -1,    -1,    -1,   360,    -1,    -1,   988,   364,
      -1,    -1,    -1,   269,   270,   104,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   112,   280,   155,   156,   104,  1212,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   295,
    2139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1235,   141,    -1,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,   324,    -1,
      -1,  1051,    -1,    -1,    -1,   331,   332,    -1,    -1,    -1,
     336,  1265,    -1,    -1,    -1,    -1,    -1,    -1,  1068,  1069,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,    -1,    -1,    -1,    -1,   462,    -1,  1293,
    1294,    -1,    -1,   202,    -1,    -1,    -1,    -1,   374,    -1,
      -1,   377,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
     104,  1121,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,  1342,  1343,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,    -1,
     259,   152,  1356,  1357,    -1,   264,    -1,  1361,  1362,    -1,
      -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,   152,    -1,
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
      -1,    -1,    -1,    -1,  2091,    -1,  1705,  1706,    75,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1510,   152,
      -1,    -1,   155,   156,    -1,    -1,   758,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   774,   775,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,   784,   785,    -1,   787,   788,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,
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
    1072,  1073,    -1,    -1,    -1,    -1,    -1,  1079,    -1,    -1,
      -1,   134,   343,   136,   345,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,  1271,
    1272,    -1,  1700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   545,   546,    -1,   340,   341,    -1,
     551,    -1,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2080,    -1,
      -1,    -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1338,  1339,    -1,    -1,
      -1,    -1,    -1,    -1,   605,  1347,  1348,    -1,  1350,    -1,
      -1,    -1,    -1,  1600,    -1,    -1,    -1,    -1,    -1,  1361,
    1362,    -1,    -1,   624,    -1,   626,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   647,    -1,  1635,    -1,
      -1,    -1,    49,    -1,  1641,    52,    -1,    54,   451,    56,
    1828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,
     671,  1839,    -1,    -1,  1842,  1843,    73,  1845,    -1,    -1,
      -1,    -1,  1850,   684,    -1,    -1,   687,   688,    -1,   690,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,
      -1,   702,   703,   704,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,   522,
    1717,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
     147,   148,    -1,    -1,    -1,   152,   153,   758,   155,   156,
      -1,    -1,  1504,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1531,
    1958,   203,    -1,    -1,    -1,  1963,  1964,    -1,   799,    -1,
      -1,    -1,    -1,    -1,   216,    -1,   218,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1983,    -1,  1804,  1805,    -1,
     821,   822,    -1,    -1,    -1,  1812,    -1,    -1,    -1,  1816,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   642,
     643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   655,    -1,  2032,    -1,  2034,    -1,    -1,  2037,
    2038,    -1,    -1,    -1,  2042,  2043,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1625,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1641,
     312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   918,    -1,    -1,
      -1,    -1,    -1,    -1,   925,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,  1930,  2112,  2113,  2114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,
    2148,  2149,  2150,    -1,    -1,   778,   779,   988,    -1,    -1,
      -1,   784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,   808,   809,    -1,   811,    -1,
     813,   814,    -1,  2010,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1777,  1778,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,  1797,  1798,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,   859,    73,    -1,    -1,
     863,  1813,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,
      -1,    -1,   494,    -1,    -1,    -1,    -1,   499,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
    1121,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,  2119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     933,   934,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,   947,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     156,    -1,    -1,   159,    -1,    -1,   162,   163,   164,   165,
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
      -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,
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
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72
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
     217,   152,   185,   235,   201,   255,   201,    75,   108,   256,
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
#line 606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7976 "Parser/parser.cc"
    break;

  case 3:
#line 610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7982 "Parser/parser.cc"
    break;

  case 4:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( yylloc, *(yyvsp[0].tok) ) ); }
#line 7988 "Parser/parser.cc"
    break;

  case 5:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 7994 "Parser/parser.cc"
    break;

  case 6:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8000 "Parser/parser.cc"
    break;

  case 7:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( yylloc, *(yyvsp[0].tok) ) ); }
#line 8006 "Parser/parser.cc"
    break;

  case 8:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( yylloc, *(yyvsp[0].tok) ) ); }
#line 8012 "Parser/parser.cc"
    break;

  case 20:
#line 643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8018 "Parser/parser.cc"
    break;

  case 21:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( yylloc, *(yyvsp[0].str) ); }
#line 8024 "Parser/parser.cc"
    break;

  case 22:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8030 "Parser/parser.cc"
    break;

  case 23:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8040 "Parser/parser.cc"
    break;

  case 24:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8046 "Parser/parser.cc"
    break;

  case 25:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 8052 "Parser/parser.cc"
    break;

  case 26:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( yylloc, (yyvsp[0].tok) ) ); }
#line 8058 "Parser/parser.cc"
    break;

  case 28:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8064 "Parser/parser.cc"
    break;

  case 29:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::StmtExpr( yylloc, dynamic_cast<ast::CompoundStmt *>( maybeMoveBuild( (yyvsp[-1].sn) ) ) ) ); }
#line 8070 "Parser/parser.cc"
    break;

  case 30:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( yylloc, (yyvsp[-2].decl), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8076 "Parser/parser.cc"
    break;

  case 31:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8082 "Parser/parser.cc"
    break;

  case 32:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8092 "Parser/parser.cc"
    break;

  case 33:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 8098 "Parser/parser.cc"
    break;

  case 34:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 8104 "Parser/parser.cc"
    break;

  case 35:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 8110 "Parser/parser.cc"
    break;

  case 36:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 8116 "Parser/parser.cc"
    break;

  case 37:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 8122 "Parser/parser.cc"
    break;

  case 38:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 8128 "Parser/parser.cc"
    break;

  case 40:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			assert( 1 == (yyvsp[0].genexpr)->associations.size() );
			(yyvsp[-2].genexpr)->associations.push_back( (yyvsp[0].genexpr)->associations.front() );
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8140 "Parser/parser.cc"
    break;

  case 41:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuildType( (yyvsp[-2].decl) ), maybeMoveBuild( (yyvsp[0].en) ) } } );
		}
#line 8149 "Parser/parser.cc"
    break;

  case 42:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new ast::GenericExpr( yylloc, nullptr, { { maybeMoveBuild( (yyvsp[0].en) ) } } ); }
#line 8155 "Parser/parser.cc"
    break;

  case 44:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 45:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8167 "Parser/parser.cc"
    break;

  case 46:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8173 "Parser/parser.cc"
    break;

  case 47:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 8179 "Parser/parser.cc"
    break;

  case 48:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ast::ConstructorExpr( yylloc, build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 8189 "Parser/parser.cc"
    break;

  case 49:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( yylloc, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8195 "Parser/parser.cc"
    break;

  case 50:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 51:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 8208 "Parser/parser.cc"
    break;

  case 52:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 8214 "Parser/parser.cc"
    break;

  case 53:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 8220 "Parser/parser.cc"
    break;

  case 54:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].en), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8226 "Parser/parser.cc"
    break;

  case 55:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].en), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8232 "Parser/parser.cc"
    break;

  case 56:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8238 "Parser/parser.cc"
    break;

  case 57:
#line 768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].en), build_tuple( yylloc, (yyvsp[-1].en) ) ) ); }
#line 8244 "Parser/parser.cc"
    break;

  case 58:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 8250 "Parser/parser.cc"
    break;

  case 59:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].en), build_varref( yylloc, (yyvsp[0].tok) ) ) ); }
#line 8256 "Parser/parser.cc"
    break;

  case 60:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].en), build_constantInteger( yylloc, *(yyvsp[0].tok) ) ) ); }
#line 8262 "Parser/parser.cc"
    break;

  case 61:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].en), build_tuple( yylloc, (yyvsp[-1].en) ) ) ); }
#line 8268 "Parser/parser.cc"
    break;

  case 62:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( yylloc, OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 8274 "Parser/parser.cc"
    break;

  case 63:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( yylloc, OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 8280 "Parser/parser.cc"
    break;

  case 64:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 8286 "Parser/parser.cc"
    break;

  case 65:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( yylloc, (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 8292 "Parser/parser.cc"
    break;

  case 66:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( yylloc, new ExpressionNode( build_varref( yylloc, fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 8302 "Parser/parser.cc"
    break;

  case 67:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8308 "Parser/parser.cc"
    break;

  case 70:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8314 "Parser/parser.cc"
    break;

  case 71:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8320 "Parser/parser.cc"
    break;

  case 74:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8326 "Parser/parser.cc"
    break;

  case 76:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8332 "Parser/parser.cc"
    break;

  case 77:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( yylloc, *(yyvsp[-3].tok) ) ), build_tuple( yylloc, (yyvsp[-1].en) ) ) ); }
#line 8338 "Parser/parser.cc"
    break;

  case 78:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-2].en), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8344 "Parser/parser.cc"
    break;

  case 79:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-4].en), build_tuple( yylloc, (yyvsp[-1].en) ) ) ); }
#line 8350 "Parser/parser.cc"
    break;

  case 80:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-2].en), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8356 "Parser/parser.cc"
    break;

  case 81:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( yylloc, (yyvsp[-4].en), build_tuple( yylloc, (yyvsp[-1].en) ) ) ); }
#line 8362 "Parser/parser.cc"
    break;

  case 82:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_constantInteger( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8368 "Parser/parser.cc"
    break;

  case 83:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_field_name_FLOATINGconstant( yylloc, *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8374 "Parser/parser.cc"
    break;

  case 84:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( yylloc, build_varref( yylloc, (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8382 "Parser/parser.cc"
    break;

  case 85:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8388 "Parser/parser.cc"
    break;

  case 86:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			ast::Expr * constant = build_field_name_FLOATING_FRACTIONconstant( yylloc, *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( yylloc, (yyvsp[-1].en), constant ) ) : new ExpressionNode( constant );
		}
#line 8397 "Parser/parser.cc"
    break;

  case 89:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8403 "Parser/parser.cc"
    break;

  case 90:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8409 "Parser/parser.cc"
    break;

  case 91:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			switch ( (yyvsp[-1].op) ) {
			case OperKinds::AddressOf:
				(yyval.en) = new ExpressionNode( new ast::AddressExpr( maybeMoveBuild( (yyvsp[0].en) ) ) );
				break;
			case OperKinds::PointTo:
				(yyval.en) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].op), (yyvsp[0].en) ) );
				break;
			case OperKinds::And:
				(yyval.en) = new ExpressionNode( new ast::AddressExpr( new ast::AddressExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ) );
				break;
			default:
				assert( false );
			}
		}
#line 8429 "Parser/parser.cc"
    break;

  case 92:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( yylloc, (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8435 "Parser/parser.cc"
    break;

  case 93:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8441 "Parser/parser.cc"
    break;

  case 94:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( yylloc, OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8447 "Parser/parser.cc"
    break;

  case 95:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8453 "Parser/parser.cc"
    break;

  case 96:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::SizeofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8459 "Parser/parser.cc"
    break;

  case 97:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8465 "Parser/parser.cc"
    break;

  case 98:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::AlignofExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8471 "Parser/parser.cc"
    break;

  case 99:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( yylloc, (yyvsp[-3].decl), build_varref( yylloc, (yyvsp[-1].tok) ) ) ); }
#line 8477 "Parser/parser.cc"
    break;

  case 100:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8486 "Parser/parser.cc"
    break;

  case 101:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8492 "Parser/parser.cc"
    break;

  case 102:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8498 "Parser/parser.cc"
    break;

  case 103:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8504 "Parser/parser.cc"
    break;

  case 104:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8510 "Parser/parser.cc"
    break;

  case 105:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8516 "Parser/parser.cc"
    break;

  case 106:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8522 "Parser/parser.cc"
    break;

  case 107:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8528 "Parser/parser.cc"
    break;

  case 109:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( yylloc, (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8534 "Parser/parser.cc"
    break;

  case 110:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8540 "Parser/parser.cc"
    break;

  case 111:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( yylloc, (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8546 "Parser/parser.cc"
    break;

  case 112:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 113:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::VirtualCastExpr( yylloc, maybeMoveBuild( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 114:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8564 "Parser/parser.cc"
    break;

  case 115:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8570 "Parser/parser.cc"
    break;

  case 116:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8576 "Parser/parser.cc"
    break;

  case 124:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 126:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8588 "Parser/parser.cc"
    break;

  case 127:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8594 "Parser/parser.cc"
    break;

  case 128:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8600 "Parser/parser.cc"
    break;

  case 130:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 131:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8612 "Parser/parser.cc"
    break;

  case 133:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8618 "Parser/parser.cc"
    break;

  case 134:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8624 "Parser/parser.cc"
    break;

  case 136:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 137:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 138:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8642 "Parser/parser.cc"
    break;

  case 139:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8648 "Parser/parser.cc"
    break;

  case 141:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8654 "Parser/parser.cc"
    break;

  case 142:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8660 "Parser/parser.cc"
    break;

  case 144:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8666 "Parser/parser.cc"
    break;

  case 146:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8672 "Parser/parser.cc"
    break;

  case 148:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( yylloc, OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8678 "Parser/parser.cc"
    break;

  case 150:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].en), (yyvsp[0].en), ast::AndExpr ) ); }
#line 8684 "Parser/parser.cc"
    break;

  case 152:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( yylloc, (yyvsp[-2].en), (yyvsp[0].en), ast::OrExpr ) ); }
#line 8690 "Parser/parser.cc"
    break;

  case 154:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( yylloc, (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8696 "Parser/parser.cc"
    break;

  case 155:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( yylloc, (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8702 "Parser/parser.cc"
    break;

  case 158:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( yylloc, (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8714 "Parser/parser.cc"
    break;

  case 159:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8720 "Parser/parser.cc"
    break;

  case 160:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8726 "Parser/parser.cc"
    break;

  case 164:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8732 "Parser/parser.cc"
    break;

  case 165:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8738 "Parser/parser.cc"
    break;

  case 166:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8744 "Parser/parser.cc"
    break;

  case 167:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8750 "Parser/parser.cc"
    break;

  case 168:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8756 "Parser/parser.cc"
    break;

  case 169:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8762 "Parser/parser.cc"
    break;

  case 170:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8768 "Parser/parser.cc"
    break;

  case 171:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8774 "Parser/parser.cc"
    break;

  case 172:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8780 "Parser/parser.cc"
    break;

  case 173:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8786 "Parser/parser.cc"
    break;

  case 174:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8792 "Parser/parser.cc"
    break;

  case 175:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8798 "Parser/parser.cc"
    break;

  case 176:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8804 "Parser/parser.cc"
    break;

  case 177:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8810 "Parser/parser.cc"
    break;

  case 178:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( yylloc, (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8816 "Parser/parser.cc"
    break;

  case 180:
#line 1109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8822 "Parser/parser.cc"
    break;

  case 181:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8828 "Parser/parser.cc"
    break;

  case 182:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8834 "Parser/parser.cc"
    break;

  case 184:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::CommaExpr( yylloc, maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8840 "Parser/parser.cc"
    break;

  case 185:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8846 "Parser/parser.cc"
    break;

  case 198:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8852 "Parser/parser.cc"
    break;

  case 200:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ); }
#line 8858 "Parser/parser.cc"
    break;

  case 201:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( yylloc, (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8864 "Parser/parser.cc"
    break;

  case 202:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8875 "Parser/parser.cc"
    break;

  case 203:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( yylloc, (StatementNode *)0 ) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 204:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( yylloc, (yyvsp[-2].sn) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 206:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8893 "Parser/parser.cc"
    break;

  case 207:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8899 "Parser/parser.cc"
    break;

  case 208:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8905 "Parser/parser.cc"
    break;

  case 209:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8911 "Parser/parser.cc"
    break;

  case 210:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8917 "Parser/parser.cc"
    break;

  case 213:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8923 "Parser/parser.cc"
    break;

  case 214:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8929 "Parser/parser.cc"
    break;

  case 215:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( yylloc, (yyvsp[-1].en) ) ); }
#line 8935 "Parser/parser.cc"
    break;

  case 216:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8941 "Parser/parser.cc"
    break;

  case 217:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( yylloc, true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8947 "Parser/parser.cc"
    break;

  case 218:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8961 "Parser/parser.cc"
    break;

  case 219:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8967 "Parser/parser.cc"
    break;

  case 220:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( yylloc, false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8973 "Parser/parser.cc"
    break;

  case 221:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( yylloc, false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( yylloc, (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8982 "Parser/parser.cc"
    break;

  case 222:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8988 "Parser/parser.cc"
    break;

  case 223:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].sn) ), nullptr ) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 224:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].sn) ), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 225:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 226:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9012 "Parser/parser.cc"
    break;

  case 227:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9018 "Parser/parser.cc"
    break;

  case 228:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 9024 "Parser/parser.cc"
    break;

  case 229:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9030 "Parser/parser.cc"
    break;

  case 230:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 232:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 233:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 234:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 9054 "Parser/parser.cc"
    break;

  case 235:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 9060 "Parser/parser.cc"
    break;

  case 236:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 9066 "Parser/parser.cc"
    break;

  case 237:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default( yylloc ) ); }
#line 9072 "Parser/parser.cc"
    break;

  case 238:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 9078 "Parser/parser.cc"
    break;

  case 240:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 9084 "Parser/parser.cc"
    break;

  case 241:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( yylloc, (yyvsp[0].sn) ) ); }
#line 9090 "Parser/parser.cc"
    break;

  case 242:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 9096 "Parser/parser.cc"
    break;

  case 244:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].sn) ) ) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 245:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( yylloc, (yyvsp[0].sn) ) ) ) ) ); }
#line 9108 "Parser/parser.cc"
    break;

  case 246:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ) ); }
#line 9114 "Parser/parser.cc"
    break;

  case 247:
#line 1306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( yylloc, new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( yylloc, (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9123 "Parser/parser.cc"
    break;

  case 248:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( yylloc, (yyvsp[-2].ifctl), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ) ); }
#line 9129 "Parser/parser.cc"
    break;

  case 249:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( yylloc, (yyvsp[-4].ifctl), maybe_build_compound( yylloc, (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 250:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-4].sn) ) ) ); }
#line 9141 "Parser/parser.cc"
    break;

  case 251:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( yylloc, NEW_ONE, maybe_build_compound( yylloc, (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9150 "Parser/parser.cc"
    break;

  case 252:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( yylloc, (yyvsp[-2].en), maybe_build_compound( yylloc, (yyvsp[-5].sn) ) ) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 253:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( yylloc, (yyvsp[-3].en), maybe_build_compound( yylloc, (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 254:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 255:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( yylloc, new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( yylloc, (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9177 "Parser/parser.cc"
    break;

  case 256:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( yylloc, (yyvsp[-2].fctl), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ) ); }
#line 9183 "Parser/parser.cc"
    break;

  case 257:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( yylloc, (yyvsp[-4].fctl), maybe_build_compound( yylloc, (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 9189 "Parser/parser.cc"
    break;

  case 259:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].fctl)->init->set_last( (yyvsp[0].fctl)->init );
			if ( (yyvsp[-2].fctl)->condition ) {
				if ( (yyvsp[0].fctl)->condition ) {
					(yyvsp[-2].fctl)->condition->expr.reset( new ast::LogicalExpr( yylloc, (yyvsp[-2].fctl)->condition->expr.release(), (yyvsp[0].fctl)->condition->expr.release(), ast::AndExpr ) );
				} // if
			} else (yyvsp[-2].fctl)->condition = (yyvsp[0].fctl)->condition;
			if ( (yyvsp[-2].fctl)->change ) {
				if ( (yyvsp[0].fctl)->change ) {
					(yyvsp[-2].fctl)->change->expr.reset( new ast::CommaExpr( yylloc, (yyvsp[-2].fctl)->change->expr.release(), (yyvsp[0].fctl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].fctl)->change = (yyvsp[0].fctl)->change;
			(yyval.fctl) = (yyvsp[-2].fctl);
		}
#line 9208 "Parser/parser.cc"
    break;

  case 260:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9214 "Parser/parser.cc"
    break;

  case 261:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].en) ? new StatementNode( new ast::ExprStmt( yylloc, maybeMoveBuild( (yyvsp[-4].en) ) ) ) : nullptr;
			(yyval.fctl) = new ForCtrl( init, (yyvsp[-2].en), (yyvsp[0].en) );
		}
#line 9223 "Parser/parser.cc"
    break;

  case 262:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9229 "Parser/parser.cc"
    break;

  case 263:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[0].en), nullptr ); }
#line 9235 "Parser/parser.cc"
    break;

  case 264:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9241 "Parser/parser.cc"
    break;

  case 265:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9247 "Parser/parser.cc"
    break;

  case 266:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9253 "Parser/parser.cc"
    break;

  case 267:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9259 "Parser/parser.cc"
    break;

  case 268:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9268 "Parser/parser.cc"
    break;

  case 269:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9277 "Parser/parser.cc"
    break;

  case 270:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9283 "Parser/parser.cc"
    break;

  case 271:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9292 "Parser/parser.cc"
    break;

  case 272:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9301 "Parser/parser.cc"
    break;

  case 273:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9307 "Parser/parser.cc"
    break;

  case 274:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9313 "Parser/parser.cc"
    break;

  case 275:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9319 "Parser/parser.cc"
    break;

  case 276:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9325 "Parser/parser.cc"
    break;

  case 277:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9331 "Parser/parser.cc"
    break;

  case 278:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9337 "Parser/parser.cc"
    break;

  case 279:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9343 "Parser/parser.cc"
    break;

  case 280:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9349 "Parser/parser.cc"
    break;

  case 281:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9358 "Parser/parser.cc"
    break;

  case 282:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9368 "Parser/parser.cc"
    break;

  case 283:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9374 "Parser/parser.cc"
    break;

  case 284:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9380 "Parser/parser.cc"
    break;

  case 285:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9389 "Parser/parser.cc"
    break;

  case 286:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9399 "Parser/parser.cc"
    break;

  case 287:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9405 "Parser/parser.cc"
    break;

  case 288:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9414 "Parser/parser.cc"
    break;

  case 289:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9424 "Parser/parser.cc"
    break;

  case 290:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9430 "Parser/parser.cc"
    break;

  case 291:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9436 "Parser/parser.cc"
    break;

  case 292:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9442 "Parser/parser.cc"
    break;

  case 293:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9448 "Parser/parser.cc"
    break;

  case 294:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9457 "Parser/parser.cc"
    break;

  case 295:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9467 "Parser/parser.cc"
    break;

  case 296:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9473 "Parser/parser.cc"
    break;

  case 297:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9482 "Parser/parser.cc"
    break;

  case 298:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9492 "Parser/parser.cc"
    break;

  case 299:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( yylloc, (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9498 "Parser/parser.cc"
    break;

  case 300:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9507 "Parser/parser.cc"
    break;

  case 301:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( yylloc, (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9517 "Parser/parser.cc"
    break;

  case 302:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9523 "Parser/parser.cc"
    break;

  case 303:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9532 "Parser/parser.cc"
    break;

  case 304:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9541 "Parser/parser.cc"
    break;

  case 305:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9547 "Parser/parser.cc"
    break;

  case 306:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9553 "Parser/parser.cc"
    break;

  case 307:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9559 "Parser/parser.cc"
    break;

  case 308:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9565 "Parser/parser.cc"
    break;

  case 309:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9571 "Parser/parser.cc"
    break;

  case 311:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9577 "Parser/parser.cc"
    break;

  case 312:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9583 "Parser/parser.cc"
    break;

  case 313:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Goto ) ); }
#line 9589 "Parser/parser.cc"
    break;

  case 314:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9595 "Parser/parser.cc"
    break;

  case 315:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThrough ) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 316:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::FallThrough ) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 317:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, ast::BranchStmt::FallThroughDefault ) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 318:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Continue ) ); }
#line 9619 "Parser/parser.cc"
    break;

  case 319:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Continue ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 320:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, ast::BranchStmt::Break ) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 321:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( yylloc, (yyvsp[-1].tok), ast::BranchStmt::Break ) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 322:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( yylloc, (yyvsp[-1].en) ) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 323:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9649 "Parser/parser.cc"
    break;

  case 324:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::None ) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 325:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( yylloc, (yyvsp[0].sn), ast::SuspendStmt::None ) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 326:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Coroutine ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 327:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( yylloc, (yyvsp[0].sn), ast::SuspendStmt::Coroutine ) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 328:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( yylloc, nullptr, ast::SuspendStmt::Generator ) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 329:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( yylloc, (yyvsp[0].sn), ast::SuspendStmt::Generator ) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 330:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( yylloc, (yyvsp[-1].en) ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 331:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( yylloc, (yyvsp[-1].en) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 332:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 335:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( yylloc, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9709 "Parser/parser.cc"
    break;

  case 336:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].en) ) { SemanticError( yylloc, "mutex argument list cannot be empty." ); (yyval.sn) = nullptr; }
			(yyval.sn) = new StatementNode( build_mutex( yylloc, (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 9718 "Parser/parser.cc"
    break;

  case 337:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9724 "Parser/parser.cc"
    break;

  case 338:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9730 "Parser/parser.cc"
    break;

  case 341:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9736 "Parser/parser.cc"
    break;

  case 342:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9742 "Parser/parser.cc"
    break;

  case 345:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9748 "Parser/parser.cc"
    break;

  case 346:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9754 "Parser/parser.cc"
    break;

  case 347:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, new ast::WaitForStmt( yylloc ), (yyvsp[-2].en), (yyvsp[-1].en), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ); }
#line 9760 "Parser/parser.cc"
    break;

  case 348:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].en), (yyvsp[-1].en), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ); }
#line 9766 "Parser/parser.cc"
    break;

  case 349:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].en), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ); }
#line 9772 "Parser/parser.cc"
    break;

  case 350:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( yylloc, (yyvsp[-4].wfs), (yyvsp[-2].en), (yyvsp[-1].en), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 351:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9784 "Parser/parser.cc"
    break;

  case 352:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( yylloc, build_waitfor_timeout( yylloc, (yyvsp[-8].wfs), (yyvsp[-6].en), (yyvsp[-5].en), maybe_build_compound( yylloc, (yyvsp[-4].sn) ) ), (yyvsp[-2].en), maybe_build_compound( yylloc, (yyvsp[0].sn) ) ); }
#line 9790 "Parser/parser.cc"
    break;

  case 353:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9796 "Parser/parser.cc"
    break;

  case 356:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9802 "Parser/parser.cc"
    break;

  case 357:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9808 "Parser/parser.cc"
    break;

  case 358:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9814 "Parser/parser.cc"
    break;

  case 359:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wand_waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9820 "Parser/parser.cc"
    break;

  case 360:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wand_waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9826 "Parser/parser.cc"
    break;

  case 361:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9832 "Parser/parser.cc"
    break;

  case 362:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9838 "Parser/parser.cc"
    break;

  case 363:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 3\n" ); (yyval.wfs) = nullptr; }
#line 9844 "Parser/parser.cc"
    break;

  case 364:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 4\n" ); (yyval.wfs) = nullptr; }
#line 9850 "Parser/parser.cc"
    break;

  case 365:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9856 "Parser/parser.cc"
    break;

  case 366:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 6\n" ); (yyval.wfs) = nullptr; }
#line 9862 "Parser/parser.cc"
    break;

  case 367:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( yylloc, nullptr ) ); }
#line 9868 "Parser/parser.cc"
    break;

  case 368:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( yylloc, (yyvsp[-1].sn), (yyvsp[0].sn), nullptr ) ); }
#line 9874 "Parser/parser.cc"
    break;

  case 369:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( yylloc, (yyvsp[-1].sn), nullptr, (yyvsp[0].sn) ) ); }
#line 9880 "Parser/parser.cc"
    break;

  case 370:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( yylloc, (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9886 "Parser/parser.cc"
    break;

  case 371:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( yylloc, (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9892 "Parser/parser.cc"
    break;

  case 372:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( yylloc, (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9898 "Parser/parser.cc"
    break;

  case 373:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9904 "Parser/parser.cc"
    break;

  case 374:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9910 "Parser/parser.cc"
    break;

  case 375:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = ast::Terminate; }
#line 9916 "Parser/parser.cc"
    break;

  case 376:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = ast::Terminate; }
#line 9922 "Parser/parser.cc"
    break;

  case 377:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = ast::Resume; }
#line 9928 "Parser/parser.cc"
    break;

  case 378:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = ast::Resume; }
#line 9934 "Parser/parser.cc"
    break;

  case 379:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( yylloc, (yyvsp[0].sn) ) ); }
#line 9940 "Parser/parser.cc"
    break;

  case 381:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9946 "Parser/parser.cc"
    break;

  case 382:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9952 "Parser/parser.cc"
    break;

  case 383:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9958 "Parser/parser.cc"
    break;

  case 388:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( yylloc, (yyvsp[-4].flag), (yyvsp[-2].constant), nullptr ) ); }
#line 9964 "Parser/parser.cc"
    break;

  case 389:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( yylloc, (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9970 "Parser/parser.cc"
    break;

  case 390:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( yylloc, (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9976 "Parser/parser.cc"
    break;

  case 391:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( yylloc, (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9982 "Parser/parser.cc"
    break;

  case 392:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( yylloc, (yyvsp[-12].flag), (yyvsp[-9].constant), nullptr, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9988 "Parser/parser.cc"
    break;

  case 393:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9994 "Parser/parser.cc"
    break;

  case 394:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 10000 "Parser/parser.cc"
    break;

  case 395:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10006 "Parser/parser.cc"
    break;

  case 398:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10012 "Parser/parser.cc"
    break;

  case 399:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::AsmExpr( yylloc, "", (yyvsp[-3].constant), maybeMoveBuild( (yyvsp[-1].en) ) ) ); }
#line 10018 "Parser/parser.cc"
    break;

  case 400:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( new ast::AsmExpr( yylloc, *(yyvsp[-5].tok).str, (yyvsp[-3].constant), maybeMoveBuild( (yyvsp[-1].en) ) ) );
			delete (yyvsp[-5].tok).str;
		}
#line 10027 "Parser/parser.cc"
    break;

  case 401:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10033 "Parser/parser.cc"
    break;

  case 402:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 10039 "Parser/parser.cc"
    break;

  case 403:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 10045 "Parser/parser.cc"
    break;

  case 404:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10054 "Parser/parser.cc"
    break;

  case 405:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.emplace_back( yylloc, *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10063 "Parser/parser.cc"
    break;

  case 406:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10069 "Parser/parser.cc"
    break;

  case 409:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10075 "Parser/parser.cc"
    break;

  case 410:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10081 "Parser/parser.cc"
    break;

  case 412:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10087 "Parser/parser.cc"
    break;

  case 413:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 10093 "Parser/parser.cc"
    break;

  case 420:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10104 "Parser/parser.cc"
    break;

  case 423:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 10110 "Parser/parser.cc"
    break;

  case 424:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( yylloc, *new string( "\"\"" ) ) ); }
#line 10116 "Parser/parser.cc"
    break;

  case 428:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10122 "Parser/parser.cc"
    break;

  case 430:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 10128 "Parser/parser.cc"
    break;

  case 431:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10134 "Parser/parser.cc"
    break;

  case 432:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 10140 "Parser/parser.cc"
    break;

  case 433:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10146 "Parser/parser.cc"
    break;

  case 434:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10152 "Parser/parser.cc"
    break;

  case 435:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10158 "Parser/parser.cc"
    break;

  case 437:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10164 "Parser/parser.cc"
    break;

  case 438:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10170 "Parser/parser.cc"
    break;

  case 439:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10176 "Parser/parser.cc"
    break;

  case 440:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10187 "Parser/parser.cc"
    break;

  case 441:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10193 "Parser/parser.cc"
    break;

  case 442:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10199 "Parser/parser.cc"
    break;

  case 443:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10205 "Parser/parser.cc"
    break;

  case 444:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10211 "Parser/parser.cc"
    break;

  case 445:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10220 "Parser/parser.cc"
    break;

  case 446:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10229 "Parser/parser.cc"
    break;

  case 447:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10238 "Parser/parser.cc"
    break;

  case 448:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10249 "Parser/parser.cc"
    break;

  case 449:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10258 "Parser/parser.cc"
    break;

  case 450:
#line 2032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10264 "Parser/parser.cc"
    break;

  case 451:
#line 2034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10270 "Parser/parser.cc"
    break;

  case 452:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10276 "Parser/parser.cc"
    break;

  case 453:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10284 "Parser/parser.cc"
    break;

  case 454:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10292 "Parser/parser.cc"
    break;

  case 455:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 458:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10313 "Parser/parser.cc"
    break;

  case 459:
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10319 "Parser/parser.cc"
    break;

  case 460:
#line 2075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10325 "Parser/parser.cc"
    break;

  case 461:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10331 "Parser/parser.cc"
    break;

  case 462:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10337 "Parser/parser.cc"
    break;

  case 463:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 10343 "Parser/parser.cc"
    break;

  case 469:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Missing ';' after end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : ast::AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration" ) );
			(yyval.decl) = nullptr;
		}
#line 10354 "Parser/parser.cc"
    break;

  case 482:
#line 2140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10360 "Parser/parser.cc"
    break;

  case 485:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 488:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10372 "Parser/parser.cc"
    break;

  case 489:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10378 "Parser/parser.cc"
    break;

  case 490:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10384 "Parser/parser.cc"
    break;

  case 491:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10390 "Parser/parser.cc"
    break;

  case 492:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10396 "Parser/parser.cc"
    break;

  case 493:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10402 "Parser/parser.cc"
    break;

  case 495:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 496:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10414 "Parser/parser.cc"
    break;

  case 498:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 499:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10426 "Parser/parser.cc"
    break;

  case 500:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10432 "Parser/parser.cc"
    break;

  case 501:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10438 "Parser/parser.cc"
    break;

  case 502:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10444 "Parser/parser.cc"
    break;

  case 503:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10450 "Parser/parser.cc"
    break;

  case 504:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10456 "Parser/parser.cc"
    break;

  case 505:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10462 "Parser/parser.cc"
    break;

  case 506:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10468 "Parser/parser.cc"
    break;

  case 507:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10474 "Parser/parser.cc"
    break;

  case 508:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10480 "Parser/parser.cc"
    break;

  case 509:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10486 "Parser/parser.cc"
    break;

  case 510:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10492 "Parser/parser.cc"
    break;

  case 511:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10498 "Parser/parser.cc"
    break;

  case 512:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10504 "Parser/parser.cc"
    break;

  case 513:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 514:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10516 "Parser/parser.cc"
    break;

  case 515:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10522 "Parser/parser.cc"
    break;

  case 516:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10528 "Parser/parser.cc"
    break;

  case 517:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10534 "Parser/parser.cc"
    break;

  case 518:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10540 "Parser/parser.cc"
    break;

  case 519:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10546 "Parser/parser.cc"
    break;

  case 520:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10552 "Parser/parser.cc"
    break;

  case 521:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10558 "Parser/parser.cc"
    break;

  case 522:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10564 "Parser/parser.cc"
    break;

  case 523:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10570 "Parser/parser.cc"
    break;

  case 524:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10576 "Parser/parser.cc"
    break;

  case 525:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10582 "Parser/parser.cc"
    break;

  case 526:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10588 "Parser/parser.cc"
    break;

  case 527:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10594 "Parser/parser.cc"
    break;

  case 528:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10600 "Parser/parser.cc"
    break;

  case 529:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10606 "Parser/parser.cc"
    break;

  case 530:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10612 "Parser/parser.cc"
    break;

  case 531:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10618 "Parser/parser.cc"
    break;

  case 532:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10624 "Parser/parser.cc"
    break;

  case 533:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10630 "Parser/parser.cc"
    break;

  case 534:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10636 "Parser/parser.cc"
    break;

  case 536:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10642 "Parser/parser.cc"
    break;

  case 538:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 539:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10654 "Parser/parser.cc"
    break;

  case 540:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10660 "Parser/parser.cc"
    break;

  case 542:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10666 "Parser/parser.cc"
    break;

  case 543:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 544:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 545:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 547:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 549:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 550:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 551:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 552:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10714 "Parser/parser.cc"
    break;

  case 553:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 554:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10726 "Parser/parser.cc"
    break;

  case 555:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10732 "Parser/parser.cc"
    break;

  case 556:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10738 "Parser/parser.cc"
    break;

  case 557:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10744 "Parser/parser.cc"
    break;

  case 558:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10755 "Parser/parser.cc"
    break;

  case 559:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10761 "Parser/parser.cc"
    break;

  case 560:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10767 "Parser/parser.cc"
    break;

  case 561:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 562:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10784 "Parser/parser.cc"
    break;

  case 563:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10790 "Parser/parser.cc"
    break;

  case 564:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 565:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10805 "Parser/parser.cc"
    break;

  case 567:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10811 "Parser/parser.cc"
    break;

  case 568:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10817 "Parser/parser.cc"
    break;

  case 569:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10823 "Parser/parser.cc"
    break;

  case 571:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 572:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10835 "Parser/parser.cc"
    break;

  case 574:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 575:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 576:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 578:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 579:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 580:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 581:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10877 "Parser/parser.cc"
    break;

  case 582:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 584:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 585:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 586:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10901 "Parser/parser.cc"
    break;

  case 587:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10907 "Parser/parser.cc"
    break;

  case 588:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10913 "Parser/parser.cc"
    break;

  case 589:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10924 "Parser/parser.cc"
    break;

  case 593:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10930 "Parser/parser.cc"
    break;

  case 594:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10936 "Parser/parser.cc"
    break;

  case 595:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10945 "Parser/parser.cc"
    break;

  case 596:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10953 "Parser/parser.cc"
    break;

  case 597:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10962 "Parser/parser.cc"
    break;

  case 598:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10971 "Parser/parser.cc"
    break;

  case 599:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10980 "Parser/parser.cc"
    break;

  case 600:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10989 "Parser/parser.cc"
    break;

  case 602:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10995 "Parser/parser.cc"
    break;

  case 603:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 11001 "Parser/parser.cc"
    break;

  case 604:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11011 "Parser/parser.cc"
    break;

  case 605:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11030 "Parser/parser.cc"
    break;

  case 608:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Struct; }
#line 11036 "Parser/parser.cc"
    break;

  case 609:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Union; }
#line 11042 "Parser/parser.cc"
    break;

  case 610:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Exception; }
#line 11048 "Parser/parser.cc"
    break;

  case 611:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11054 "Parser/parser.cc"
    break;

  case 612:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Monitor; }
#line 11060 "Parser/parser.cc"
    break;

  case 613:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Generator; }
#line 11066 "Parser/parser.cc"
    break;

  case 614:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor generator is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11075 "Parser/parser.cc"
    break;

  case 615:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Coroutine; }
#line 11081 "Parser/parser.cc"
    break;

  case 616:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor coroutine is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11090 "Parser/parser.cc"
    break;

  case 617:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = ast::AggregateDecl::Thread; }
#line 11096 "Parser/parser.cc"
    break;

  case 618:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "monitor thread is currently unimplemented." );
			(yyval.aggKey) = ast::AggregateDecl::NoAggregate;
		}
#line 11105 "Parser/parser.cc"
    break;

  case 619:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11111 "Parser/parser.cc"
    break;

  case 620:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11117 "Parser/parser.cc"
    break;

  case 621:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11130 "Parser/parser.cc"
    break;

  case 622:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11136 "Parser/parser.cc"
    break;

  case 623:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11142 "Parser/parser.cc"
    break;

  case 624:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11155 "Parser/parser.cc"
    break;

  case 625:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11161 "Parser/parser.cc"
    break;

  case 628:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11167 "Parser/parser.cc"
    break;

  case 629:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11173 "Parser/parser.cc"
    break;

  case 632:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11179 "Parser/parser.cc"
    break;

  case 634:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 635:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 11191 "Parser/parser.cc"
    break;

  case 636:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 637:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 638:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 639:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11215 "Parser/parser.cc"
    break;

  case 641:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11221 "Parser/parser.cc"
    break;

  case 643:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 644:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 646:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 647:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11245 "Parser/parser.cc"
    break;

  case 649:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11251 "Parser/parser.cc"
    break;

  case 650:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 651:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 652:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 653:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 654:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11286 "Parser/parser.cc"
    break;

  case 655:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11294 "Parser/parser.cc"
    break;

  case 656:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 11303 "Parser/parser.cc"
    break;

  case 657:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11311 "Parser/parser.cc"
    break;

  case 658:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11319 "Parser/parser.cc"
    break;

  case 659:
#line 2702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11327 "Parser/parser.cc"
    break;

  case 660:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11335 "Parser/parser.cc"
    break;

  case 662:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11341 "Parser/parser.cc"
    break;

  case 663:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 11347 "Parser/parser.cc"
    break;

  case 664:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 665:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 666:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 667:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11371 "Parser/parser.cc"
    break;

  case 668:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 669:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 671:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11389 "Parser/parser.cc"
    break;

  case 672:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11395 "Parser/parser.cc"
    break;

  case 673:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 674:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11407 "Parser/parser.cc"
    break;

  case 675:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11413 "Parser/parser.cc"
    break;

  case 676:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11419 "Parser/parser.cc"
    break;

  case 679:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 680:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11431 "Parser/parser.cc"
    break;

  case 681:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11437 "Parser/parser.cc"
    break;

  case 683:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 684:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 685:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 687:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 688:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11467 "Parser/parser.cc"
    break;

  case 689:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11473 "Parser/parser.cc"
    break;

  case 691:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11479 "Parser/parser.cc"
    break;

  case 694:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 695:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11491 "Parser/parser.cc"
    break;

  case 697:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 698:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 699:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 704:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 706:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11521 "Parser/parser.cc"
    break;

  case 707:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11527 "Parser/parser.cc"
    break;

  case 708:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11533 "Parser/parser.cc"
    break;

  case 709:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11539 "Parser/parser.cc"
    break;

  case 710:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 711:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11551 "Parser/parser.cc"
    break;

  case 717:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11557 "Parser/parser.cc"
    break;

  case 720:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11563 "Parser/parser.cc"
    break;

  case 721:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11569 "Parser/parser.cc"
    break;

  case 722:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11575 "Parser/parser.cc"
    break;

  case 723:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11581 "Parser/parser.cc"
    break;

  case 724:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11587 "Parser/parser.cc"
    break;

  case 725:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11593 "Parser/parser.cc"
    break;

  case 726:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11599 "Parser/parser.cc"
    break;

  case 728:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11605 "Parser/parser.cc"
    break;

  case 729:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11611 "Parser/parser.cc"
    break;

  case 730:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11617 "Parser/parser.cc"
    break;

  case 732:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( yylloc, (yyvsp[-1].tok) ) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 734:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11629 "Parser/parser.cc"
    break;

  case 735:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( yylloc, (yyvsp[0].tok) ) ); }
#line 11635 "Parser/parser.cc"
    break;

  case 736:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11641 "Parser/parser.cc"
    break;

  case 737:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11647 "Parser/parser.cc"
    break;

  case 738:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-4].en) ), maybeMoveBuild( (yyvsp[-2].en) ) ) ); }
#line 11653 "Parser/parser.cc"
    break;

  case 739:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11659 "Parser/parser.cc"
    break;

  case 741:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11665 "Parser/parser.cc"
    break;

  case 742:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11671 "Parser/parser.cc"
    break;

  case 743:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11677 "Parser/parser.cc"
    break;

  case 744:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == ast::TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11688 "Parser/parser.cc"
    break;

  case 745:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11694 "Parser/parser.cc"
    break;

  case 746:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11700 "Parser/parser.cc"
    break;

  case 747:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 748:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11715 "Parser/parser.cc"
    break;

  case 749:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( ast::TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 750:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11727 "Parser/parser.cc"
    break;

  case 751:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11733 "Parser/parser.cc"
    break;

  case 752:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::DStype; }
#line 11739 "Parser/parser.cc"
    break;

  case 753:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11745 "Parser/parser.cc"
    break;

  case 754:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Otype; }
#line 11751 "Parser/parser.cc"
    break;

  case 755:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Dtype; }
#line 11757 "Parser/parser.cc"
    break;

  case 756:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ftype; }
#line 11763 "Parser/parser.cc"
    break;

  case 757:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = ast::TypeDecl::Ttype; }
#line 11769 "Parser/parser.cc"
    break;

  case 758:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11775 "Parser/parser.cc"
    break;

  case 761:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 762:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 763:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11793 "Parser/parser.cc"
    break;

  case 764:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 766:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11805 "Parser/parser.cc"
    break;

  case 767:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11811 "Parser/parser.cc"
    break;

  case 768:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11817 "Parser/parser.cc"
    break;

  case 769:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 770:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11829 "Parser/parser.cc"
    break;

  case 771:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 772:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11841 "Parser/parser.cc"
    break;

  case 773:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11850 "Parser/parser.cc"
    break;

  case 774:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11859 "Parser/parser.cc"
    break;

  case 775:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11868 "Parser/parser.cc"
    break;

  case 776:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11874 "Parser/parser.cc"
    break;

  case 777:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11883 "Parser/parser.cc"
    break;

  case 778:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11889 "Parser/parser.cc"
    break;

  case 780:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11895 "Parser/parser.cc"
    break;

  case 785:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11901 "Parser/parser.cc"
    break;

  case 786:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11907 "Parser/parser.cc"
    break;

  case 787:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 789:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11919 "Parser/parser.cc"
    break;

  case 790:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11925 "Parser/parser.cc"
    break;

  case 791:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11931 "Parser/parser.cc"
    break;

  case 792:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11937 "Parser/parser.cc"
    break;

  case 794:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11943 "Parser/parser.cc"
    break;

  case 795:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11949 "Parser/parser.cc"
    break;

  case 796:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( yylloc, (yyvsp[0].tok) ) ) ); }
#line 11955 "Parser/parser.cc"
    break;

  case 797:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 11972 "Parser/parser.cc"
    break;

  case 798:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11978 "Parser/parser.cc"
    break;

  case 799:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11984 "Parser/parser.cc"
    break;

  case 800:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11990 "Parser/parser.cc"
    break;

  case 801:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11996 "Parser/parser.cc"
    break;

  case 802:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12002 "Parser/parser.cc"
    break;

  case 803:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 12008 "Parser/parser.cc"
    break;

  case 805:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12017 "Parser/parser.cc"
    break;

  case 806:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( yylloc, false, (yyvsp[-2].constant), nullptr ) ) ); }
#line 12023 "Parser/parser.cc"
    break;

  case 807:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12032 "Parser/parser.cc"
    break;

  case 808:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12042 "Parser/parser.cc"
    break;

  case 809:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = ast::Linkage::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12051 "Parser/parser.cc"
    break;

  case 810:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12061 "Parser/parser.cc"
    break;

  case 811:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12070 "Parser/parser.cc"
    break;

  case 812:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12080 "Parser/parser.cc"
    break;

  case 813:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12089 "Parser/parser.cc"
    break;

  case 814:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12099 "Parser/parser.cc"
    break;

  case 815:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12108 "Parser/parser.cc"
    break;

  case 816:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12118 "Parser/parser.cc"
    break;

  case 818:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 12124 "Parser/parser.cc"
    break;

  case 819:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 12130 "Parser/parser.cc"
    break;

  case 820:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 12136 "Parser/parser.cc"
    break;

  case 821:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 12148 "Parser/parser.cc"
    break;

  case 822:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12159 "Parser/parser.cc"
    break;

  case 823:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 12168 "Parser/parser.cc"
    break;

  case 824:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 12177 "Parser/parser.cc"
    break;

  case 825:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12183 "Parser/parser.cc"
    break;

  case 826:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12189 "Parser/parser.cc"
    break;

  case 827:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12195 "Parser/parser.cc"
    break;

  case 828:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 12204 "Parser/parser.cc"
    break;

  case 829:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12210 "Parser/parser.cc"
    break;

  case 830:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12216 "Parser/parser.cc"
    break;

  case 831:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12222 "Parser/parser.cc"
    break;

  case 836:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::RangeExpr( yylloc, maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 12228 "Parser/parser.cc"
    break;

  case 837:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12234 "Parser/parser.cc"
    break;

  case 838:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12244 "Parser/parser.cc"
    break;

  case 839:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12250 "Parser/parser.cc"
    break;

  case 842:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12256 "Parser/parser.cc"
    break;

  case 843:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12262 "Parser/parser.cc"
    break;

  case 845:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12268 "Parser/parser.cc"
    break;

  case 846:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12274 "Parser/parser.cc"
    break;

  case 847:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12280 "Parser/parser.cc"
    break;

  case 848:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 12286 "Parser/parser.cc"
    break;

  case 853:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12292 "Parser/parser.cc"
    break;

  case 854:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12298 "Parser/parser.cc"
    break;

  case 855:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12304 "Parser/parser.cc"
    break;

  case 856:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12310 "Parser/parser.cc"
    break;

  case 857:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12316 "Parser/parser.cc"
    break;

  case 859:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12322 "Parser/parser.cc"
    break;

  case 860:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12328 "Parser/parser.cc"
    break;

  case 861:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12334 "Parser/parser.cc"
    break;

  case 862:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12340 "Parser/parser.cc"
    break;

  case 863:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12346 "Parser/parser.cc"
    break;

  case 864:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12352 "Parser/parser.cc"
    break;

  case 865:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12358 "Parser/parser.cc"
    break;

  case 866:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12364 "Parser/parser.cc"
    break;

  case 867:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12370 "Parser/parser.cc"
    break;

  case 868:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12376 "Parser/parser.cc"
    break;

  case 869:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12382 "Parser/parser.cc"
    break;

  case 870:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12388 "Parser/parser.cc"
    break;

  case 871:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12394 "Parser/parser.cc"
    break;

  case 872:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12400 "Parser/parser.cc"
    break;

  case 873:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12406 "Parser/parser.cc"
    break;

  case 874:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12412 "Parser/parser.cc"
    break;

  case 875:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12418 "Parser/parser.cc"
    break;

  case 876:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12424 "Parser/parser.cc"
    break;

  case 878:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12430 "Parser/parser.cc"
    break;

  case 879:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12436 "Parser/parser.cc"
    break;

  case 880:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12442 "Parser/parser.cc"
    break;

  case 881:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12448 "Parser/parser.cc"
    break;

  case 882:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12454 "Parser/parser.cc"
    break;

  case 883:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12460 "Parser/parser.cc"
    break;

  case 884:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12466 "Parser/parser.cc"
    break;

  case 885:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12472 "Parser/parser.cc"
    break;

  case 886:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12478 "Parser/parser.cc"
    break;

  case 887:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12484 "Parser/parser.cc"
    break;

  case 888:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12490 "Parser/parser.cc"
    break;

  case 889:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12496 "Parser/parser.cc"
    break;

  case 890:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12502 "Parser/parser.cc"
    break;

  case 891:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12508 "Parser/parser.cc"
    break;

  case 892:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12514 "Parser/parser.cc"
    break;

  case 893:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12520 "Parser/parser.cc"
    break;

  case 897:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12526 "Parser/parser.cc"
    break;

  case 898:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12532 "Parser/parser.cc"
    break;

  case 899:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12538 "Parser/parser.cc"
    break;

  case 900:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12544 "Parser/parser.cc"
    break;

  case 901:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12550 "Parser/parser.cc"
    break;

  case 902:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12556 "Parser/parser.cc"
    break;

  case 903:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12562 "Parser/parser.cc"
    break;

  case 904:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12568 "Parser/parser.cc"
    break;

  case 905:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 906:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12580 "Parser/parser.cc"
    break;

  case 907:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12586 "Parser/parser.cc"
    break;

  case 908:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12592 "Parser/parser.cc"
    break;

  case 909:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12598 "Parser/parser.cc"
    break;

  case 910:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12604 "Parser/parser.cc"
    break;

  case 911:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12610 "Parser/parser.cc"
    break;

  case 912:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12619 "Parser/parser.cc"
    break;

  case 913:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12625 "Parser/parser.cc"
    break;

  case 914:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12631 "Parser/parser.cc"
    break;

  case 916:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12637 "Parser/parser.cc"
    break;

  case 917:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12643 "Parser/parser.cc"
    break;

  case 918:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12649 "Parser/parser.cc"
    break;

  case 919:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12655 "Parser/parser.cc"
    break;

  case 920:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12661 "Parser/parser.cc"
    break;

  case 921:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12667 "Parser/parser.cc"
    break;

  case 922:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12673 "Parser/parser.cc"
    break;

  case 923:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12679 "Parser/parser.cc"
    break;

  case 924:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12685 "Parser/parser.cc"
    break;

  case 925:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12691 "Parser/parser.cc"
    break;

  case 926:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 927:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12703 "Parser/parser.cc"
    break;

  case 928:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12709 "Parser/parser.cc"
    break;

  case 929:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 930:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12721 "Parser/parser.cc"
    break;

  case 931:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12727 "Parser/parser.cc"
    break;

  case 932:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 933:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 935:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 936:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 937:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 938:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 939:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12769 "Parser/parser.cc"
    break;

  case 940:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 941:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 942:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 943:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 944:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 945:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12805 "Parser/parser.cc"
    break;

  case 946:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12811 "Parser/parser.cc"
    break;

  case 947:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 948:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 949:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12829 "Parser/parser.cc"
    break;

  case 950:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12835 "Parser/parser.cc"
    break;

  case 951:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12841 "Parser/parser.cc"
    break;

  case 952:
#line 3689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12847 "Parser/parser.cc"
    break;

  case 954:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 955:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 956:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 957:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 958:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 959:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 960:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 961:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 962:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12901 "Parser/parser.cc"
    break;

  case 963:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 964:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 965:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12919 "Parser/parser.cc"
    break;

  case 966:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 967:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 969:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12937 "Parser/parser.cc"
    break;

  case 970:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12943 "Parser/parser.cc"
    break;

  case 971:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12949 "Parser/parser.cc"
    break;

  case 972:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12955 "Parser/parser.cc"
    break;

  case 973:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12961 "Parser/parser.cc"
    break;

  case 974:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12967 "Parser/parser.cc"
    break;

  case 975:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12973 "Parser/parser.cc"
    break;

  case 976:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12979 "Parser/parser.cc"
    break;

  case 977:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12985 "Parser/parser.cc"
    break;

  case 978:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12991 "Parser/parser.cc"
    break;

  case 979:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12997 "Parser/parser.cc"
    break;

  case 981:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13003 "Parser/parser.cc"
    break;

  case 982:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13009 "Parser/parser.cc"
    break;

  case 983:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 13015 "Parser/parser.cc"
    break;

  case 984:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13021 "Parser/parser.cc"
    break;

  case 985:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13027 "Parser/parser.cc"
    break;

  case 986:
#line 3805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13033 "Parser/parser.cc"
    break;

  case 987:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13039 "Parser/parser.cc"
    break;

  case 989:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13045 "Parser/parser.cc"
    break;

  case 990:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13051 "Parser/parser.cc"
    break;

  case 991:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13057 "Parser/parser.cc"
    break;

  case 992:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13063 "Parser/parser.cc"
    break;

  case 993:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13069 "Parser/parser.cc"
    break;

  case 994:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13075 "Parser/parser.cc"
    break;

  case 995:
#line 3832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13081 "Parser/parser.cc"
    break;

  case 996:
#line 3834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13087 "Parser/parser.cc"
    break;

  case 997:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), nullptr, false ) ); }
#line 13093 "Parser/parser.cc"
    break;

  case 998:
#line 3840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13099 "Parser/parser.cc"
    break;

  case 1000:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13105 "Parser/parser.cc"
    break;

  case 1001:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13111 "Parser/parser.cc"
    break;

  case 1003:
#line 3851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13117 "Parser/parser.cc"
    break;

  case 1004:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new ast::TypeExpr( yylloc, maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13123 "Parser/parser.cc"
    break;

  case 1006:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 13129 "Parser/parser.cc"
    break;

  case 1007:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 13135 "Parser/parser.cc"
    break;

  case 1008:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ); }
#line 13141 "Parser/parser.cc"
    break;

  case 1009:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13147 "Parser/parser.cc"
    break;

  case 1010:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ) ); }
#line 13153 "Parser/parser.cc"
    break;

  case 1011:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13159 "Parser/parser.cc"
    break;

  case 1012:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13165 "Parser/parser.cc"
    break;

  case 1015:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13171 "Parser/parser.cc"
    break;

  case 1016:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13177 "Parser/parser.cc"
    break;

  case 1017:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13183 "Parser/parser.cc"
    break;

  case 1018:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 13189 "Parser/parser.cc"
    break;

  case 1019:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13195 "Parser/parser.cc"
    break;

  case 1020:
#line 3926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13201 "Parser/parser.cc"
    break;

  case 1021:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13207 "Parser/parser.cc"
    break;

  case 1022:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13213 "Parser/parser.cc"
    break;

  case 1024:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13219 "Parser/parser.cc"
    break;

  case 1025:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13225 "Parser/parser.cc"
    break;

  case 1026:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13231 "Parser/parser.cc"
    break;

  case 1027:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13237 "Parser/parser.cc"
    break;

  case 1028:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13243 "Parser/parser.cc"
    break;

  case 1029:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13249 "Parser/parser.cc"
    break;

  case 1031:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13255 "Parser/parser.cc"
    break;

  case 1033:
#line 3967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13261 "Parser/parser.cc"
    break;

  case 1034:
#line 3970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13267 "Parser/parser.cc"
    break;

  case 1035:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13273 "Parser/parser.cc"
    break;

  case 1036:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 13279 "Parser/parser.cc"
    break;

  case 1037:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 13285 "Parser/parser.cc"
    break;

  case 1038:
#line 3979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 13291 "Parser/parser.cc"
    break;

  case 1040:
#line 3994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13297 "Parser/parser.cc"
    break;

  case 1041:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13303 "Parser/parser.cc"
    break;

  case 1042:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 13309 "Parser/parser.cc"
    break;

  case 1043:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13315 "Parser/parser.cc"
    break;

  case 1044:
#line 4005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13321 "Parser/parser.cc"
    break;

  case 1045:
#line 4007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13327 "Parser/parser.cc"
    break;

  case 1046:
#line 4009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13333 "Parser/parser.cc"
    break;

  case 1048:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13339 "Parser/parser.cc"
    break;

  case 1049:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13345 "Parser/parser.cc"
    break;

  case 1050:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13351 "Parser/parser.cc"
    break;

  case 1051:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13357 "Parser/parser.cc"
    break;

  case 1052:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13363 "Parser/parser.cc"
    break;

  case 1055:
#line 4036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13369 "Parser/parser.cc"
    break;

  case 1058:
#line 4047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13375 "Parser/parser.cc"
    break;

  case 1059:
#line 4049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13381 "Parser/parser.cc"
    break;

  case 1060:
#line 4051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13387 "Parser/parser.cc"
    break;

  case 1061:
#line 4053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13393 "Parser/parser.cc"
    break;

  case 1062:
#line 4055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13399 "Parser/parser.cc"
    break;

  case 1063:
#line 4057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13405 "Parser/parser.cc"
    break;

  case 1064:
#line 4064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13411 "Parser/parser.cc"
    break;

  case 1065:
#line 4066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13417 "Parser/parser.cc"
    break;

  case 1066:
#line 4068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13423 "Parser/parser.cc"
    break;

  case 1067:
#line 4070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13429 "Parser/parser.cc"
    break;

  case 1068:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13435 "Parser/parser.cc"
    break;

  case 1069:
#line 4075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13441 "Parser/parser.cc"
    break;

  case 1070:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13447 "Parser/parser.cc"
    break;

  case 1071:
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13453 "Parser/parser.cc"
    break;

  case 1072:
#line 4081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13459 "Parser/parser.cc"
    break;

  case 1073:
#line 4083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13465 "Parser/parser.cc"
    break;

  case 1074:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13471 "Parser/parser.cc"
    break;

  case 1075:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 13477 "Parser/parser.cc"
    break;

  case 1076:
#line 4095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 13483 "Parser/parser.cc"
    break;

  case 1077:
#line 4097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13489 "Parser/parser.cc"
    break;

  case 1079:
#line 4124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13495 "Parser/parser.cc"
    break;

  case 1083:
#line 4135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13501 "Parser/parser.cc"
    break;

  case 1084:
#line 4137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13507 "Parser/parser.cc"
    break;

  case 1085:
#line 4139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13513 "Parser/parser.cc"
    break;

  case 1086:
#line 4141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13519 "Parser/parser.cc"
    break;

  case 1087:
#line 4143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13525 "Parser/parser.cc"
    break;

  case 1088:
#line 4145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13531 "Parser/parser.cc"
    break;

  case 1089:
#line 4152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13537 "Parser/parser.cc"
    break;

  case 1090:
#line 4154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13543 "Parser/parser.cc"
    break;

  case 1091:
#line 4156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13549 "Parser/parser.cc"
    break;

  case 1092:
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13555 "Parser/parser.cc"
    break;

  case 1093:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13561 "Parser/parser.cc"
    break;

  case 1094:
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13567 "Parser/parser.cc"
    break;

  case 1095:
#line 4167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13573 "Parser/parser.cc"
    break;

  case 1096:
#line 4169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13579 "Parser/parser.cc"
    break;

  case 1097:
#line 4171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13585 "Parser/parser.cc"
    break;

  case 1098:
#line 4178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13591 "Parser/parser.cc"
    break;

  case 1099:
#line 4180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13597 "Parser/parser.cc"
    break;

  case 1102:
#line 4204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13603 "Parser/parser.cc"
    break;

  case 1103:
#line 4206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13609 "Parser/parser.cc"
    break;


#line 13613 "Parser/parser.cc"

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
#line 4209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
