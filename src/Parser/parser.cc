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
#include <stack>
using namespace std;

#include "SynTree/Declaration.h"
#include "ParseNode.h"
#include "TypedefTable.h"
#include "TypeData.h"
#include "SynTree/LinkageSpec.h"
#include "Common/SemanticError.h"						// error_str
#include "Common/utility.h"								// for maybeMoveBuild, maybeBuild, CodeLo...

#include "SynTree/Attribute.h"							// for Attribute

// lex uses __null in a boolean context, it's fine.
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wparentheses-equality"
#endif

extern DeclarationNode * parseTree;
extern LinkageSpec::Spec linkage;
extern TypedefTable typedefTable;

stack<LinkageSpec::Spec> linkageStack;

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

#define NEW_ZERO new ExpressionNode( build_constantInteger( *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( *new string( "1" ) ) )
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan ? left : right)
#define MISSING_ANON_FIELD "Missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "Missing low value for up-to range so index is uninitialized."
#define MISSING_HIGH "Missing high value for down-to range so index is uninitialized."

static ForCtrl * makeForCtrl(
		DeclarationNode * init,
		enum OperKinds compop,
		ExpressionNode * comp,
		ExpressionNode * inc ) {
	// Wrap both comp/inc if they are non-null.
	if ( comp ) comp = new ExpressionNode( build_binary_val(
		compop,
		new ExpressionNode( build_varref( new string( *init->name ) ) ),
		comp ) );
	if ( inc ) inc = new ExpressionNode( build_binary_val(
		// choose += or -= for upto/downto
		compop == OperKinds::LThan || compop == OperKinds::LEThan ? OperKinds::PlusAssn : OperKinds::MinusAssn,
		new ExpressionNode( build_varref( new string( *init->name ) ) ),
		inc ) );
	// The StatementNode call frees init->name, it must happen later.
	return new ForCtrl( new StatementNode( init ), comp, inc );
}

ForCtrl * forCtrl( DeclarationNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "Direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "Multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ConstantExpr * constant = dynamic_cast<ConstantExpr *>(type->expr.get());
	if ( constant && (constant->get_constant()->get_value() == "0" || constant->get_constant()->get_value() == "1") ) {
		type = new ExpressionNode( new CastExpr( maybeMoveBuild( type ), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distAttr(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( ExpressionNode * type, ExpressionNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( NameExpr * identifier = dynamic_cast<NameExpr *>(index->expr.get()) ) {
		return forCtrl( type, new string( identifier->name ), start, compop, comp, inc );
	} else if ( CommaExpr * commaExpr = dynamic_cast<CommaExpr *>(index->expr.get()) ) {
		if ( NameExpr * identifier = dynamic_cast<NameExpr *>(commaExpr->arg1 ) ) {
			return forCtrl( type, new string( identifier->name ), start, compop, comp, inc );
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

static bool TypedefForall( DeclarationNode * decl ) {
	if ( decl->type->forall || (decl->type->kind == TypeData::Aggregate && decl->type->aggregate.params) ) {
		SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." );
		return true;
	} // if
	return false;
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

#line 331 "Parser/parser.cc"

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
    QUOTED_IDENTIFIER = 360,
    TYPEDIMname = 361,
    TYPEDEFname = 362,
    TYPEGENname = 363,
    TIMEOUT = 364,
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
#define QUOTED_IDENTIFIER 360
#define TYPEDIMname 361
#define TYPEDEFname 362
#define TYPEGENname 363
#define TIMEOUT 364
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
#line 303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	Token tok;
	ParseNode * pn;
	ExpressionNode * en;
	DeclarationNode * decl;
	AggregateDecl::Aggregate aggKey;
	TypeDecl::Kind tclass;
	StatementNode * sn;
	WaitForStmt * wfs;
	Expression * constant;
	CondCtl * ifctl;
	ForCtrl * fctl;
	OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	EnumHiding hide;
	CatchStmt::Kind catch_kind;
	GenericExpr * genexpr;

#line 706 "Parser/parser.cc"

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
#define YYFINAL  145
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   22847

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  302
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1086
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2193

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
     626,   627,   628,   629,   630,   631,   635,   636,   640,   641,
     646,   650,   651,   662,   664,   666,   670,   671,   673,   675,
     677,   679,   689,   691,   693,   695,   697,   699,   704,   705,
     715,   720,   725,   726,   731,   737,   739,   741,   747,   749,
     753,   755,   757,   759,   761,   763,   765,   767,   769,   771,
     773,   775,   777,   779,   781,   783,   793,   794,   798,   799,
     804,   807,   811,   812,   816,   817,   819,   821,   823,   825,
     827,   832,   834,   836,   844,   845,   853,   856,   857,   859,
     864,   880,   882,   884,   886,   888,   890,   892,   894,   896,
     904,   905,   907,   911,   912,   913,   914,   918,   919,   921,
     923,   925,   927,   929,   931,   933,   940,   941,   942,   943,
     947,   948,   952,   953,   958,   959,   961,   963,   968,   969,
     971,   976,   977,   979,   984,   985,   987,   989,   991,   996,
     997,   999,  1004,  1005,  1010,  1011,  1016,  1017,  1022,  1023,
    1028,  1029,  1034,  1035,  1038,  1043,  1048,  1049,  1057,  1063,
    1064,  1068,  1069,  1073,  1074,  1078,  1079,  1080,  1081,  1082,
    1083,  1084,  1085,  1086,  1087,  1088,  1098,  1100,  1105,  1106,
    1108,  1110,  1115,  1116,  1122,  1123,  1129,  1130,  1131,  1132,
    1133,  1134,  1135,  1136,  1137,  1138,  1139,  1141,  1142,  1148,
    1150,  1160,  1162,  1170,  1171,  1176,  1178,  1180,  1182,  1184,
    1188,  1189,  1191,  1196,  1203,  1205,  1207,  1217,  1219,  1221,
    1226,  1231,  1234,  1239,  1241,  1243,  1245,  1253,  1254,  1256,
    1260,  1262,  1266,  1268,  1269,  1271,  1273,  1278,  1279,  1283,
    1288,  1289,  1293,  1295,  1300,  1302,  1307,  1309,  1311,  1313,
    1318,  1320,  1322,  1324,  1329,  1331,  1336,  1337,  1359,  1361,
    1366,  1369,  1371,  1374,  1376,  1379,  1381,  1386,  1391,  1393,
    1398,  1403,  1405,  1407,  1409,  1411,  1414,  1416,  1419,  1421,
    1426,  1432,  1435,  1437,  1442,  1448,  1450,  1455,  1461,  1464,
    1466,  1469,  1471,  1476,  1483,  1485,  1490,  1496,  1498,  1503,
    1509,  1512,  1517,  1525,  1527,  1529,  1534,  1536,  1541,  1542,
    1544,  1549,  1551,  1556,  1558,  1560,  1562,  1565,  1569,  1572,
    1576,  1578,  1580,  1582,  1584,  1586,  1588,  1590,  1592,  1594,
    1596,  1601,  1602,  1606,  1612,  1620,  1625,  1626,  1630,  1634,
    1639,  1640,  1646,  1650,  1652,  1654,  1656,  1659,  1661,  1666,
    1668,  1673,  1675,  1677,  1682,  1684,  1690,  1691,  1695,  1696,
    1697,  1698,  1702,  1707,  1708,  1710,  1712,  1714,  1718,  1722,
    1723,  1727,  1729,  1731,  1733,  1735,  1741,  1742,  1748,  1749,
    1753,  1754,  1759,  1761,  1767,  1768,  1770,  1775,  1780,  1791,
    1792,  1796,  1797,  1803,  1804,  1808,  1810,  1814,  1816,  1820,
    1821,  1825,  1826,  1830,  1837,  1838,  1842,  1844,  1859,  1860,
    1861,  1862,  1864,  1868,  1870,  1874,  1881,  1883,  1885,  1890,
    1891,  1893,  1895,  1897,  1929,  1932,  1937,  1939,  1945,  1950,
    1955,  1966,  1972,  1977,  1983,  1989,  1999,  2003,  2010,  2012,
    2013,  2014,  2030,  2032,  2035,  2037,  2040,  2045,  2046,  2050,
    2051,  2052,  2053,  2063,  2064,  2065,  2074,  2075,  2076,  2080,
    2081,  2082,  2091,  2092,  2093,  2098,  2099,  2108,  2109,  2114,
    2115,  2119,  2121,  2123,  2125,  2127,  2132,  2137,  2138,  2140,
    2150,  2151,  2156,  2158,  2160,  2162,  2164,  2166,  2169,  2171,
    2173,  2178,  2180,  2182,  2184,  2186,  2188,  2190,  2192,  2194,
    2196,  2198,  2200,  2202,  2204,  2206,  2208,  2210,  2212,  2214,
    2216,  2218,  2220,  2222,  2224,  2226,  2228,  2230,  2232,  2237,
    2238,  2242,  2249,  2250,  2256,  2257,  2259,  2261,  2263,  2268,
    2270,  2275,  2276,  2278,  2280,  2285,  2287,  2289,  2291,  2293,
    2295,  2300,  2307,  2309,  2311,  2316,  2324,  2323,  2327,  2335,
    2336,  2338,  2340,  2345,  2346,  2348,  2353,  2354,  2356,  2358,
    2363,  2364,  2366,  2371,  2373,  2375,  2377,  2378,  2380,  2385,
    2387,  2389,  2394,  2401,  2405,  2406,  2411,  2410,  2415,  2414,
    2424,  2423,  2434,  2433,  2443,  2448,  2449,  2454,  2460,  2478,
    2479,  2483,  2485,  2487,  2493,  2495,  2497,  2499,  2501,  2503,
    2505,  2507,  2513,  2514,  2519,  2528,  2530,  2532,  2541,  2543,
    2544,  2545,  2547,  2549,  2550,  2555,  2556,  2557,  2562,  2564,
    2567,  2570,  2577,  2578,  2579,  2585,  2590,  2592,  2598,  2599,
    2605,  2606,  2610,  2615,  2618,  2617,  2621,  2624,  2631,  2636,
    2635,  2644,  2649,  2654,  2659,  2664,  2665,  2670,  2672,  2677,
    2679,  2681,  2683,  2688,  2689,  2695,  2696,  2697,  2704,  2705,
    2707,  2708,  2709,  2711,  2713,  2720,  2721,  2723,  2725,  2730,
    2731,  2737,  2738,  2740,  2741,  2746,  2747,  2748,  2750,  2758,
    2759,  2761,  2764,  2766,  2770,  2771,  2772,  2774,  2776,  2781,
    2783,  2788,  2790,  2799,  2801,  2806,  2807,  2808,  2812,  2813,
    2814,  2819,  2820,  2825,  2826,  2827,  2828,  2832,  2833,  2838,
    2839,  2840,  2841,  2842,  2856,  2857,  2862,  2863,  2869,  2871,
    2874,  2876,  2878,  2901,  2902,  2908,  2909,  2915,  2914,  2924,
    2923,  2927,  2933,  2939,  2940,  2942,  2946,  2951,  2953,  2955,
    2957,  2963,  2964,  2968,  2969,  2974,  2976,  2983,  2985,  2986,
    2988,  2993,  2995,  2997,  3002,  3004,  3009,  3014,  3022,  3027,
    3029,  3034,  3039,  3040,  3045,  3046,  3050,  3051,  3052,  3057,
    3059,  3065,  3067,  3072,  3074,  3080,  3081,  3085,  3089,  3093,
    3095,  3108,  3110,  3112,  3114,  3116,  3118,  3120,  3121,  3126,
    3129,  3128,  3140,  3139,  3152,  3151,  3163,  3162,  3174,  3173,
    3187,  3193,  3195,  3201,  3202,  3213,  3220,  3225,  3231,  3234,
    3237,  3241,  3247,  3250,  3253,  3258,  3259,  3260,  3261,  3265,
    3271,  3272,  3282,  3283,  3287,  3288,  3293,  3298,  3299,  3305,
    3306,  3308,  3313,  3314,  3315,  3316,  3317,  3319,  3354,  3356,
    3361,  3363,  3364,  3366,  3371,  3373,  3375,  3377,  3382,  3384,
    3386,  3388,  3390,  3392,  3394,  3399,  3401,  3403,  3405,  3414,
    3416,  3417,  3422,  3424,  3426,  3428,  3430,  3435,  3437,  3439,
    3441,  3446,  3448,  3450,  3452,  3454,  3456,  3468,  3469,  3470,
    3474,  3476,  3478,  3480,  3482,  3487,  3489,  3491,  3493,  3498,
    3500,  3502,  3504,  3506,  3508,  3520,  3525,  3530,  3532,  3533,
    3535,  3540,  3542,  3544,  3546,  3551,  3553,  3555,  3557,  3559,
    3561,  3563,  3568,  3570,  3572,  3574,  3583,  3585,  3586,  3591,
    3593,  3595,  3597,  3599,  3604,  3606,  3608,  3610,  3615,  3617,
    3619,  3621,  3623,  3625,  3635,  3637,  3639,  3640,  3642,  3647,
    3649,  3651,  3656,  3658,  3660,  3662,  3667,  3669,  3671,  3685,
    3687,  3689,  3690,  3692,  3697,  3699,  3704,  3706,  3708,  3713,
    3715,  3720,  3722,  3739,  3740,  3742,  3747,  3749,  3751,  3753,
    3755,  3760,  3761,  3763,  3765,  3770,  3772,  3774,  3780,  3782,
    3785,  3788,  3790,  3794,  3796,  3798,  3799,  3801,  3803,  3807,
    3809,  3814,  3816,  3818,  3820,  3855,  3856,  3860,  3861,  3863,
    3865,  3870,  3872,  3874,  3876,  3878,  3883,  3884,  3886,  3888,
    3893,  3895,  3897,  3903,  3904,  3906,  3915,  3918,  3920,  3923,
    3925,  3927,  3941,  3942,  3944,  3949,  3951,  3953,  3955,  3957,
    3962,  3963,  3965,  3967,  3972,  3974,  3982,  3983,  3984,  3989,
    3990,  3995,  3997,  3999,  4001,  4003,  4005,  4012,  4014,  4016,
    4018,  4020,  4023,  4025,  4027,  4029,  4031,  4036,  4038,  4040,
    4045,  4071,  4072,  4074,  4078,  4079,  4083,  4085,  4087,  4089,
    4091,  4093,  4100,  4102,  4104,  4106,  4108,  4110,  4115,  4117,
    4119,  4126,  4128,  4146,  4148,  4153,  4154
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
  "QUOTED_IDENTIFIER", "TYPEDIMname", "TYPEDEFname", "TYPEGENname",
  "TIMEOUT", "WOR", "CATCH", "RECOVER", "CATCHRESUME", "FIXUP", "FINALLY",
  "INTEGERconstant", "CHARACTERconstant", "STRINGliteral", "DIRECTIVE",
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
  "mutex_statement", "when_clause", "when_clause_opt", "waitfor",
  "cast_expression_list", "timeout", "waitfor_clause", "waitfor_statement",
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
     405,   125,    40,    64,    41,    46,    91,    93,    44,    58,
     123,    96,    94,    42,    38,    43,    45,    33,   126,    92,
      47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1833)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1085)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     163, 12693,   188,   191, 17492,   135, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,   132,   773,
     155, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,   103,   299,
   -1833, -1833, -1833, -1833, -1833, -1833,  4596,  4596,   190, 12693,
     232,   241, 15676, -1833,   259, -1833, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833, -1833,  3030, -1833,   964,   263, -1833, -1833,
   -1833, -1833, -1833, 17340, -1833, -1833,   300,   303,   420,    63,
   -1833,  4596,   303,   303,   303,   379,  4359,   620,   802, 12855,
   -1833, -1833,   574, 17188,  1149, -1833, -1833, -1833,  2529,   648,
    7764,  8486,  1101,  2529,  1266,   511, -1833, -1833, -1833, -1833,
     624, -1833, -1833, -1833, -1833,   527, -1833, -1833, -1833, -1833,
   -1833,   550,   548,   624, -1833,   624,   605, -1833, -1833, -1833,
   18358,  4596, -1833, -1833,  4596, -1833, 12693, -1833,   600, 18411,
   -1833, -1833,  4372, 19438, -1833, -1833,   871,   871,   627,  2393,
   -1833, -1833, -1833, -1833,   345, 14818,  3094,   624, -1833, -1833,
   -1833, -1833, -1833, -1833,   676, -1833,   631,   702,   769, -1833,
     677, 22130, -1833, -1833, -1833, -1833, -1833, -1833, -1833, 16143,
    2895,  3030,   663,   787,   795,   822,   828,   844,   855, -1833,
   -1833, 18563, 11704,   817, -1833, 17939, -1833, -1833, -1833, -1833,
     827, -1833, -1833,   834, -1833, 20206,   932, 20354, -1833,   833,
    4596,   548,   864,   861,   869,   872, -1833, -1833, -1833,  2736,
    3341,   888,   961,    44,   961, -1833,   624,   624,    81,    91,
      75,   961, -1833,   624,   624,    81,   624, -1833,   624, -1833,
    4032, -1833, -1833,   946,   949,   871, 14710, -1833, 17340, -1833,
   -1833,  2529, -1833,  2405,   511,   970,  1019,    91,  4596,  4596,
     420, -1833, 14335, -1833,   871,   871,   975,  1019,    91,  4596,
   -1833,  8215, -1833, -1833, -1833,   871, -1833, -1833, -1833, -1833,
     871, -1833,   976,  3002,  4596, -1833,  1404,   987, -1833, -1833,
   -1833, 17039,   548,   192, -1833, -1833, 19489, -1833,   961,   -64,
   -1833, 22130, 19438,  3466,  4032, -1833,   375, -1833, -1833, -1833,
   -1833, -1833, 18411,  4596, -1833,   981, -1833, -1833, -1833, -1833,
    4596,  3077,   385,   653, -1833,  4596,   631, -1833,   418,   624,
     624,  1006, 18616,   825, 15301, 14871,  2529,  2529, -1833,  2529,
     871,  2529,   871, -1833, -1833,   624, -1833,   967, -1833, 18768,
   -1833, -1833, -1833, 18821,   827, -1833,   262,   430,   161,   557,
     511,  1002, -1833,  2393,   999,   631,  2393,  1668, -1833,  1029,
    1078, 22204,  1046,  1056,  1059, 22130, 22278,  1076, 22739, -1833,
   -1833, -1833, -1833, -1833, -1833, 22352, 22352, 15987,  1043,  4266,
   -1833, -1833, -1833, -1833,   356, -1833,   507, -1833,  2108, -1833,
   22130, 22130, -1833,  1047,   697,  1068,  1170,   613,  1185,  1079,
    1083,  1084,  1118,    20, -1833,   737, -1833,  1097, -1833,  1166,
    4412, 16455, -1833, -1833,   680,  1097, -1833, -1833,   759, -1833,
   -1833,  2895,  1106,  1124,  1164,  1172,  1176,  1182, -1833, -1833,
     439,  1135, -1833,   776,  1135, -1833, -1833, 18358, -1833,  1183,
    1190, 16611, -1833, -1833,  3953,  4170,  1215, 15301,  1227,   766,
     857, -1833, -1833, -1833, -1833, -1833,  4596,  4297, -1833, -1833,
   -1833, -1833, -1833, -1833, 16932,  3571,  1043, 20206,  1208,  1251,
   -1833, -1833,  1207, 20354,   796, -1833, -1833, -1833, 20428,  1245,
   -1833, -1833, -1833, -1833, -1833,  2736,   726,  1254,  1262,  1288,
     770,  1292,  1306,  1308,  1310,  1325,  1328,  3341, -1833, -1833,
   -1833,   624,  1314,  1327,  1263, -1833, -1833,  1330,   420, -1833,
   -1833,   548,  1019, -1833, -1833, -1833,   420, -1833, -1833,   548,
   -1833, -1833,  4032, -1833, 16455, 16455, -1833,   871,  4372, 20042,
   15462, -1833, -1833, -1833, -1833, -1833,   548,  1019,   -64,  1324,
   -1833, -1833,  2529,  1341,  1019,    91, -1833,   548,  1019, -1833,
    8955, -1833,   871,   871, -1833, -1833,  1360,   475,  1362,   511,
    1364, -1833, 17653, -1833,   839, -1833,  1418, 19939, -1833,  4372,
   17128, 14710, -1833, 17039, 22426, -1833, -1833, -1833, -1833, -1833,
    3466,   818,  4032, -1833, 15462,   961, 12693, -1833,  1375, -1833,
    1386, -1833, -1833, -1833, -1833, -1833,  2393, -1833, -1833,  1470,
    3723,  3443, 18821, 11704, -1833, 18973, -1833,   871,   871, -1833,
   -1833,   827, -1833,   747,  1397,  1538, 22130,   848,  1330,  1395,
   -1833,   624,   624, -1833,  1135, -1833, 18616, -1833, -1833, 18100,
     871,   871, -1833,  3723,   624, -1833, 19293, -1833, -1833, 18768,
   -1833,   345, -1833, -1833, -1833,  1415,  4596,  1002,  1419,   874,
   18411,   895, -1833, -1833, -1833, -1833, -1833, -1833,   898, -1833,
    1451,  1431, -1833, 16299, -1833,  4266, 19026, 19026, -1833, 16299,
   -1833, 22130, -1833, -1833, -1833, -1833, -1833, -1833, 16299, -1833,
   -1833, 18153, 19026, 19026,  1166,  1356,  1422,   524,  1438, -1833,
     903,  1459,  1181,  1463, -1833, 20428, 22130, 20502,  1460, 22130,
    1404, 22130,  1404, -1833,  1814, -1833, -1833, 20576,   978, 22130,
   20576,  1404, -1833, -1833, 22130, 22130, 22130, 22130, 22130, 22130,
   22130, 22130, 22130, 22130, 22130, 22130, 22130, 22130, 22130, 22130,
   22130, 22130, 22130, 20650,  1452,   677,  2277, 11704, -1833, -1833,
   -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,  1468,
   22130, -1833, -1833,   680,  2094, -1833, -1833,   624,   624, -1833,
   -1833, 16455, -1833,   442,  1135, -1833,   913,  1135, -1833, -1833,
   -1833,  1330, -1833, -1833,  1330, 22500, -1833, -1833, 11704,  1473,
    1474,  3752,  1616,  3217,   448,  1395, -1833,   624,   624,  1395,
     465, -1833,   624,   624, 22130,  4596,  1219,  1230,  1395,   -71,
   14657, 14657,  4596, -1833, -1833, 22130,  1207, -1833, 20206,  1484,
   -1833,  2951, -1833, -1833, -1833, -1833, -1833,   905, -1833, 14657,
    1404,  4372,  1404,   922,  1482,  1483,  1485,   944,  1488,  1490,
    1491,  1493,  1495,  1496,   472,  1135, -1833, -1833,   531,  1135,
   -1833, -1833,   536,  1135, -1833, -1833, -1833,  4372,   677,  1630,
    1135, 19634, -1833, -1833,   548, 17653, -1833, -1833, -1833,   948,
    1501,   969,  1504, -1833,  1508, -1833,   548, -1833,  1509, -1833,
     548,  1019,  1508, -1833,   548,  1502,  1503,  1506, -1833, -1833,
   18100, -1833,  1515, -1833, -1833, -1833,  1404,  4596, 10848,  1600,
    1497, -1833, -1833, 19187, -1833,  1190, -1833, 14657,   977, -1833,
   -1833,  1508, -1833, 18411, 16455,  1500, -1833,  1500, -1833, -1833,
   -1833,   161,   624,   624, -1833, 18768, -1833, 11869, 16767, -1833,
   17653,  1519,  1523,  1524, -1833,  9659,   624, -1833,   848, -1833,
   -1833, -1833, -1833,  1330, -1833, -1833, -1833,   871, -1833,  3599,
   -1833, -1833,   511,   508,  1529,  1507,  1522,   161, -1833, -1833,
    1525,  1533,  1668, 20576, -1833,  1535,  1534,   263,  1537,  1539,
    1542,  1540,  1546, 22130,  1547,  1548,  1549, 11704, 22130, -1833,
   -1833,  1699, -1833, -1833, -1833, 22130, -1833,  1550,  1552, 20280,
    1236, -1833, 20576,  1551, -1833,  1563, -1833, -1833,  3981, -1833,
   -1833,  1018, -1833, -1833, -1833, -1833,  3981, -1833, -1833,  1238,
     577, -1833, -1833,  1047,  1047,  1047,   697,   697,  1068,  1068,
    1170,  1170,  1170,  1170,   613,   613,  1185,  1079,  1083,  1084,
    1118, 22130,  1241, -1833,  1568,  3981, -1833, -1833, 20206, -1833,
   17653,  1570,  1571,  1572,  2094, -1833, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833,  1330, -1833, -1833,  1330, 17653, 17653, -1833,
   -1833,  3752,   870,  1574,  1575,  1576,  1577,  2594,  3217, -1833,
   -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833,  1579, -1833,  1395, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833, -1833,  1578,  1580, -1833,   420,  3981,  1244,
      50, -1833, -1833,  1586, -1833, 20354, -1833, 22130,   624, 20724,
   14657, -1833, -1833, -1833,  1562,   542,  1135, -1833,   560,  1135,
   -1833, -1833,   570,  1135, -1833, -1833, -1833,  1330, -1833, -1833,
   -1833,  1330, -1833, -1833, -1833,  1330,   961,  1587, -1833,  1330,
      62, -1833,  1097,  1582, -1833, -1833, -1833, -1833, -1833, -1833,
    1591, -1833, -1833, -1833, 18411,  1508, -1833,   548, -1833, -1833,
   -1833, -1833, -1833, 13496,  1589,  1590, -1833,   288, -1833,   496,
     365, 11539,  1592, 15822,  1593,  1598,  1432,  2241,  2505, 20798,
    1599, -1833, -1833,  1601,  1602, -1833, -1833,   548, 22130, 22130,
    1738,  1596,   647, -1833,  1684,  1603,  1583, -1833, -1833, -1833,
   10673, -1833, -1833, -1833, -1833, -1833,  1035, -1833, -1833, -1833,
    1672, -1833, -1833, -1833,  1404, -1833, -1833, 13341, 17340,  1607,
   -1833,  4596, -1833,  1597,  1610,  1612, -1833,  1247, -1833, -1833,
   -1833, -1833,  4372, -1833, -1833,  1613,  1614,  1026, 18411,   631,
     631,  1415,  1002,  1002, -1833, -1833,  1043,  1190, 16611, -1833,
    1097, -1833, 12034, -1833,   621,  1135, -1833,   871, 10055, -1833,
   -1833,   161,   624,   624,   345,  4596, -1833, 20872, -1833,   161,
    1415,  1608, -1833, -1833,  1036,   643, 18100, 11704,  1404, -1833,
     643, 18206,   643, -1833, 22130, 22130, 22130, -1833, -1833, -1833,
   -1833, 22130, 22130,  1629, 20206, -1833, -1833,  1634,   662, -1833,
   -1833, -1833,  2676, -1833, -1833,  1249, -1833,   281, -1833, 20576,
    1265, -1833, 20428, -1833, -1833, 22130,  1617,  1268,  1271,  1207,
   -1833,   632,  1135, -1833, -1833, 17653, 17653, -1833, -1833,  1642,
     637,  1135, -1833,   666,  2072,   624,   624, -1833, -1833, 17653,
   17653, -1833,  1640, -1833, 15462, 15462,  1644,  1643,  1646,  1648,
   -1833,  1647, 22130, 22130,  1273,  1649, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833,  1653, 22130, -1833, -1833, -1833,  1330, -1833,
   -1833, -1833,  1330, -1833, -1833, -1833,  1330, 17653, 17653, 17653,
     420,   624, -1833, -1833,  1301, 22130, 19783,  1651,  1655,  1619,
   -1833, -1833, -1833,  1659, 13651, 13806, 13961, 18411, 14710, 19026,
   19026,  1660, -1833,  1623,  1637,  2416, 14174, -1833,   347,  4596,
   -1833, -1833,  4596, -1833, 20576,   247,   274, -1833, -1833, -1833,
   -1833, 22130,  1663,  1739, 11373, 11023, -1833,  1641, -1833,  1645,
   22130,  1656, 20206,  1657, 22130, 20428, 22130,  1270, -1833,  1661,
     -28, -1833,   128,  1671, -1833, -1833,  1669, -1833,  1664, -1833,
    1666,  1674, 15822,   784, 14496,   624,   453, -1833, -1833, -1833,
    1670, -1833,  1677, -1833,  1681, -1833,  1678, -1833,  1689, -1833,
   -1833, -1833, -1833,  1697,  1680,  1690, 12199,  1692,  1695,  1696,
   -1833,  1700, -1833, -1833, -1833,  1330, 22130, 22130,  1190,  1701,
   -1833,  1415, -1833,  1002,   170,  1507, 20206, -1833,  1415,  1705,
   -1833, 18411, -1833,   831,  1703,  1702,  1049, -1833,  1706, -1833,
   -1833, -1833, -1833, -1833, 20206,  1207, 20428, -1833,  1741,  3981,
   -1833,  1741,  1741, -1833,  3981,  3194,  4579, -1833, -1833,  1312,
   -1833, -1833, -1833,  1709,  1712, -1833, -1833, -1833,  1330, -1833,
   -1833,  1713,  1716,   624, -1833, -1833, -1833,  1330, -1833, -1833,
   -1833,  1719, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,
   -1833, -1833, -1833, -1833, -1833,  1717, -1833, -1833, -1833, -1833,
    1718,  1723,   624, -1833, 17653, 17653, 17653, -1833, -1833, -1833,
   -1833, -1833, 22130, -1833,    62, -1833,  1097, -1833, -1833, -1833,
    1727,  1728, -1833,  1660,  1660,  1660,  4203,  1092,  1708,   476,
   -1833,  4203,   518, 16455, -1833, -1833, -1833,  3660, 22130,  4101,
     523, -1833, -1833,    45,  1721,  1721,  1721,  4596, -1833, -1833,
   17805, -1833,  1052, -1833, -1833, -1833, -1833,  1063,  1730, 15822,
    1603,  1729, 22130,   300,  1731,   379, 14123, 18411, -1833, -1833,
   -1833,  1186, 15822, 22130,  1020,   722, -1833, 22130, 20052, -1833,
   -1833,   526, -1833,  1207, -1833,  1064,  1081,  1105, -1833, -1833,
   -1833, -1833,   548,  1270,  1734, -1833, -1833, 22130, -1833,  1736,
     677, 11539, -1833, -1833, -1833, -1833, 22130,  1784, -1833, 10498,
   -1833,   624, 15462, -1833, -1833, 18411, -1833, -1833, -1833,   161,
     161, -1833, -1833, -1833,  1740, -1833, 17653, -1833, -1833,  1743,
   -1833,  1744,  1745,  1002,  1746, -1833, -1833,  1207,  1752, -1833,
   -1833,  1751, -1833, -1833, 22130, -1833, 18206, 22130,  1207,  1756,
    1315, -1833,  1318, -1833,  3981, -1833,  3981, -1833, -1833, -1833,
   -1833, 17653,  1754,  1755, -1833, -1833, 17653, 17653,  1757,  1758,
    1335, 14979, 15140, -1833,  1763, -1833, -1833, -1833, -1833, -1833,
    1759,  1761,  1779,  1338, 22130, -1833, -1833, -1833, -1833, -1833,
     555,  1092,  2274,   567, -1833, -1833, -1833, -1833,   624,   624,
   -1833, -1833, -1833,   576, -1833,  1107,  3660,   968, -1833,  4101,
     624, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,
   15822,   458, 20946,  1859, 15822,  1603, 15623, -1833, -1833, -1833,
   -1833, 22130, -1833, 21020,  1860,  1764, 20129, 21094, 15822, 11198,
    1603,   800,  1154,  1765, 22130, -1833,  1785,   485, 15822, -1833,
   -1833,  1796, -1833, -1833,  1772,   677,   735,  1797,  1798,  1339,
    1864, -1833, -1833, -1833, -1833,  4596,  4372,  1415,  1415, -1833,
   -1833,  1793,  1803, -1833, -1833, -1833,  1801,   161,  1811, -1833,
    1813, -1833, -1833, -1833, -1833,  1815, -1833, -1833, -1833,  1369,
    1393, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833, -1833,
   -1833,  1817, -1833, -1833,  1820,  1822,  1824, -1833, -1833, -1833,
   -1833, -1833, -1833,  1825,  1827,  1829,  2274, -1833,   624, -1833,
   -1833, -1833, -1833, -1833,  1831,  4203, -1833,  9407,   111, 12367,
   -1833, 15717, -1833,     0,  1119, 15822,  1894,   579,  1823,   468,
   15822, 22130,  1830,   800,  1154,  1818, 22574,  1832,   582,  1913,
   -1833, 21168, 21242, 22130,  1603,  1821, 12531, -1833, -1833, -1833,
   19240, -1833,  1833,  1828,   217, 15822, -1833, 22130, 20576,   383,
   -1833, -1833, -1833,  1839,  1848,  1847, -1833, -1833,   161,  1415,
   -1833, -1833, -1833, -1833, -1833,  1850,  1855,  1856, 15462,  1844,
   -1833, -1833, -1833,   673,  1135, -1833, -1833,  1092, -1833, -1833,
     406, -1833,   407, -1833, -1833, -1833,  1862, 13017, -1833, -1833,
   15822, -1833,     9, -1833, 15822, 22130,  1863, 21316, -1833, -1833,
   21390, 21464, 22130,  1830,  1603, 21538, 21612, 15822,  1852,   590,
    1861,   599, -1833, -1833,  1867, 13017, 19240, -1833,  2823, 18973,
    1404,  1865, -1833,  1919,  1883,   738,  1878, -1833,  1963, -1833,
    1130, 15822,  1887, 15822, 15822, -1833, -1833, -1833,  1415,  1889,
   -1833, -1833, -1833, -1833, -1833, -1833, -1833,  1330, -1833, 22130,
   -1833, 22130, -1833, -1833,  1481, 13179, -1833, -1833, 15822, -1833,
   -1833,  1603, -1833, -1833,  1603,  1873,   633,  1875,   636, -1833,
   -1833,  1603, -1833,  1603, -1833,  1891, 21686, 21760, 21834, -1833,
    1481, -1833,  1868,  2517,  3738, -1833, -1833, -1833,   217,  1892,
   22130,  1870,   217,   217, 15822, -1833, -1833, 22130,  1941,  1943,
    1903, -1833, 17653, -1833, -1833, 15717, -1833,  1481, -1833, -1833,
    1906, 21908, 21982, 22056, -1833, -1833,  1603, -1833,  1603, -1833,
    1603, -1833,  1868, 22130,  1907,  3738,  1901,   677,  1908, -1833,
     750, -1833, -1833,  1131,  1864,   419, -1833, -1833, -1833, 10244,
    1916, 15717, -1833, -1833,  1603, -1833,  1603, -1833,  1603,  1917,
    1910, -1833,   548,   677,  1918, -1833,  1895,   677, -1833, -1833,
   15822,  1994,  1921, -1833, -1833, -1833, 10376, -1833,   548, -1833,
   -1833,  1409, 22130, -1833,  1141, -1833, 15822, -1833, -1833,   677,
    1404,  1923,  1896, -1833, -1833, -1833,  1155, -1833, -1833,  1902,
    1404, -1833, -1833
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   465,     0,     2,   465,   482,   483,   484,   485,   486,
     487,   488,   489,   490,   471,   473,   472,   474,     0,     0,
       0,   491,   493,   514,   494,   515,   497,   498,   512,   513,
     492,   510,   511,   495,   496,   499,   500,   501,   502,   503,
     504,   505,   506,   507,   508,   509,   516,   517,   822,   519,
     592,   593,   596,   598,   594,   600,     0,     0,     0,   465,
       0,     0,    16,   563,   569,     9,    10,    11,    12,    13,
      14,    15,   779,   102,     0,    19,     0,     2,   100,   101,
      17,    18,   838,   465,   780,   405,     0,   408,   703,   410,
     419,     0,   409,   439,   440,     0,     0,     0,     0,   546,
     467,   469,   475,   465,   477,   480,   531,   518,   449,   524,
     529,   451,   541,   450,   556,   560,   566,   545,   572,   584,
     822,   589,   590,   573,   644,   411,   412,     3,   787,   800,
     470,     0,     0,   822,   860,   822,     2,   877,   878,   879,
     465,     0,  1064,  1065,     0,     1,   465,    16,     0,   465,
     428,   429,     0,   546,   475,   459,   460,   461,   790,     0,
     595,   597,   599,   601,     0,   465,     0,   823,   824,   591,
     520,   696,   697,   695,   756,   751,   741,     0,     0,   788,
       0,     0,   482,   781,   785,   786,   782,   783,   784,   465,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   564,
     567,   465,   465,     0,  1066,   546,   867,   885,  1070,  1063,
    1061,  1068,   404,     0,   164,   709,   163,     0,   413,     0,
       0,     0,     0,     0,     0,     0,   403,   954,   955,     0,
       0,   438,   820,   822,   820,   841,   822,   822,   448,     2,
     822,   820,   898,   822,   822,   447,   822,   917,   822,   895,
       0,   539,   540,     0,     0,   465,   465,     2,   465,   420,
     468,   478,   532,     0,   561,     0,   803,     2,     0,     0,
     703,   421,   546,   525,   542,   557,     0,   803,     2,     0,
     481,   526,   533,   534,   452,   543,   454,   455,   453,   548,
     558,   562,     0,   576,     0,   773,     2,     2,   801,   859,
     861,   465,     0,     2,     2,  1074,   546,  1077,   820,   820,
       3,     0,   546,     0,     0,   431,   822,   815,   817,   816,
     818,     2,   465,     0,   777,     0,   737,   739,   738,   740,
       0,     0,   733,     0,   723,     0,   732,   743,     0,   822,
     822,     2,   465,  1085,   466,   465,   477,   456,   524,   457,
     549,   458,   556,   553,   574,   822,   575,     0,   684,   465,
     685,  1039,  1040,   465,   686,   688,   563,   569,   645,   647,
     648,   645,   825,     0,   754,   742,     0,   829,    21,     0,
      20,     0,     0,     0,     0,     0,     0,     0,    23,    25,
       4,     8,     5,     6,     7,     0,     0,   465,     2,     0,
     103,   104,   105,   106,    87,    24,    88,    42,    86,   107,
       0,     0,   122,   124,   128,   131,   134,   139,   142,   144,
     146,   148,   150,   152,   155,     0,    26,     0,   570,     2,
     107,   465,   156,   748,   699,   560,   701,   747,     0,   698,
     702,     0,     0,     0,     0,     0,     0,     0,   839,   865,
     822,   875,   883,   887,   893,     2,  1072,   465,  1075,     2,
     100,   465,     3,   683,     0,  1085,     0,   466,   524,   549,
     556,     3,     3,   665,   669,   679,   685,   686,     2,   868,
     886,  1062,     2,     2,    23,     0,     2,   709,    24,     0,
     707,   710,  1083,     0,     0,   716,   705,   704,     0,     0,
     805,     2,     2,     2,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   844,   901,
     924,   822,     0,   444,     2,   840,   848,   982,   703,   842,
     843,     0,   803,     2,   897,   905,   703,   899,   900,     0,
     916,   918,     0,   434,   465,   465,   530,   466,     0,   546,
     465,  1067,  1071,  1069,   547,   777,     0,   803,   820,     0,
     414,   422,   479,     0,   803,     2,   777,     0,   803,   752,
     527,   528,   544,   559,   565,   568,   563,   569,   587,   588,
       0,   753,   465,   693,     0,   201,   397,   465,     3,     0,
     546,   465,   802,   465,     0,   416,     2,   417,   774,   436,
       0,     0,     0,     2,   465,   820,   465,   777,     0,     2,
       0,   736,   735,   734,   729,   476,     0,   727,   744,   522,
       0,     0,   465,   465,  1041,   466,   462,   463,   464,  1045,
    1036,  1037,  1043,     2,     2,   101,     0,  1001,  1015,  1085,
     997,   822,   822,  1006,  1013,   691,   465,   554,   687,   466,
     550,   551,   555,     0,   822,  1051,   466,  1056,  1048,   465,
    1053,     0,   654,   646,   653,  1083,     0,   645,     0,     0,
     465,     0,   837,   836,   832,   834,   835,   833,     0,   827,
     830,     0,    22,   465,    94,     0,   465,   465,    89,   465,
      96,     0,    32,    36,    37,    33,    34,    35,   465,    92,
      93,   465,   465,   465,     2,   103,   104,     0,     0,   182,
       0,     0,   590,     0,  1061,     0,     0,     0,     0,     0,
       0,     0,     0,    55,     0,    61,    62,    66,     0,     0,
      66,     0,    90,    91,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   465,   165,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   163,
       0,   161,   162,     2,   966,   700,   963,   822,   822,   971,
     571,   465,   866,   822,   876,   884,   888,   894,     2,   869,
     871,   873,     2,   889,   891,     0,  1073,  1076,   465,     0,
       0,     2,   101,  1001,   822,  1085,   936,   822,   822,  1085,
     822,   951,   822,   822,     3,   687,     0,     0,  1085,  1085,
     465,   465,     0,     2,   718,     0,  1083,   715,  1084,     0,
     711,     0,     2,   714,   717,   179,   178,     0,     2,   465,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   822,   853,   857,   896,   822,   910,
     914,   922,   822,   932,   845,   902,   925,     0,     0,     0,
     978,     0,   442,   806,     0,   465,   443,   807,   435,     0,
       0,     0,     0,   433,     2,   808,     0,   418,     2,   777,
       0,   803,     2,   809,     0,     0,     0,     0,   602,   672,
     466,     3,     3,   676,   675,   880,     0,     0,   465,   398,
       0,   448,   447,   546,     3,   100,     3,   465,     0,     3,
     778,     2,   731,   465,   465,   725,   724,   725,   523,   521,
     647,   645,   822,   822,  1047,   465,  1052,   466,   465,  1038,
     465,     0,     0,     0,  1016,     0,   822,  1086,  1002,  1003,
     692,   999,  1000,  1014,  1042,  1046,  1044,   552,   587,     0,
    1050,  1055,   650,   645,     0,   655,     0,   645,   757,   755,
       0,     0,   829,    66,   789,     0,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   465,     0,   121,
     120,     0,   117,   116,    27,     0,    28,     0,     0,     0,
       0,     3,    66,     0,    51,     0,    52,    59,     0,    58,
      70,     0,    67,    68,    71,    54,     0,    53,    57,     0,
       0,    50,   123,   125,   126,   127,   129,   130,   132,   133,
     137,   138,   135,   136,   140,   141,   143,   145,   147,   149,
     151,     0,     0,   407,     0,     0,    29,     3,   709,   157,
     465,     0,     0,     0,   967,   968,   964,   965,   750,   749,
       2,   870,   872,   874,     2,   890,   892,   465,   465,   992,
     991,     2,     0,     0,     0,     0,     0,   822,  1002,   939,
     956,     2,   934,   942,   689,   937,   938,   690,     2,   949,
     959,   952,   953,     0,     3,  1085,   426,     2,  1078,     2,
     680,   681,   659,     3,     3,     3,     3,   703,     0,   155,
       0,     3,     3,     0,   712,     0,   706,     0,   822,     0,
     465,     3,   430,   432,     0,   822,   854,   858,   822,   911,
     915,   923,   822,   933,     2,   846,   849,   851,     2,   903,
     906,   908,     2,   926,   928,   930,   820,     0,   445,   979,
       3,   983,   984,     3,   811,     3,   536,   535,   538,   537,
       2,   778,   812,   759,   465,     2,   810,     0,   778,   813,
     602,   602,   602,   465,     0,     0,   694,     0,   401,     0,
       0,   465,     0,     2,     0,     0,     0,     0,     0,   184,
       0,   331,   332,     0,     0,   370,   369,     0,   159,   159,
     376,   563,   569,   198,     0,   185,     0,   209,   186,   187,
     465,   203,   188,   189,   190,   191,     0,   192,   193,   337,
       0,   194,   195,   196,     0,   197,   205,   546,   465,     0,
     207,     0,   395,     0,     0,     0,     3,     0,   791,   778,
     766,   767,     0,     3,   762,     3,     3,     0,   465,   741,
     741,  1083,   645,   645,  1049,  1054,     2,   100,   465,     3,
     561,     3,   466,     3,   822,  1009,  1012,   465,     3,   998,
    1004,   645,   822,   822,     0,     0,   633,     0,   649,   645,
    1083,     2,   826,   828,     0,    95,   465,   465,     0,    99,
      97,   465,     0,   111,     0,     0,     0,   115,   119,   118,
     183,     0,     0,     0,   709,   108,   176,     0,     0,    45,
      46,    84,     0,    84,    84,     0,    72,    74,    48,     0,
       0,    44,     0,    47,   154,     0,     0,     0,     0,  1083,
       3,   822,   974,   977,   969,   465,   465,     3,     3,     0,
     822,   945,   948,   822,     0,   822,   822,   940,   957,   465,
     465,  1079,     0,   682,   465,   465,     0,     0,     0,     0,
     415,     3,     0,     0,     0,     0,   708,   713,     3,   804,
     181,   180,     3,     0,     0,     2,   847,   850,   852,     2,
     904,   907,   909,     2,   927,   929,   931,   465,   465,   465,
     703,   822,   990,   989,     0,     0,     0,     0,     0,     0,
       3,   778,   814,     0,   465,   465,   465,   465,   465,   465,
     465,   585,   614,     3,     3,   615,   546,   603,     0,     0,
     862,     2,     0,   399,    66,     0,     0,   322,   323,   206,
     208,     0,     0,     0,   465,   465,   318,     0,   316,     0,
       0,     0,   709,     0,     0,     0,     0,     0,   160,     0,
       0,   377,     0,     0,     3,   213,     0,   204,     0,   313,
       0,     0,     2,     0,   546,   822,     0,   396,   994,   993,
       0,     2,     0,   769,     2,   764,     0,   765,     0,   745,
     726,   730,   728,     0,     0,     0,   465,     0,     0,     0,
       3,     0,     2,  1005,  1007,  1008,     0,     0,   100,     0,
       3,  1083,   639,   645,   655,   655,   709,   656,  1083,     0,
     758,   465,   831,   995,     0,     0,     0,    38,     0,   112,
     114,   113,   110,   109,   709,  1083,     0,    65,    81,     0,
      75,    82,    83,    60,     0,     0,     0,    69,    56,     0,
     153,   406,    30,     0,     0,     2,   970,   972,   973,     3,
       3,     0,     0,   822,     2,   941,   943,   944,     2,   958,
     960,     0,   935,   950,     3,     3,  1080,     3,   667,   666,
     670,  1082,     2,     2,  1081,     0,     3,   819,   719,   720,
       0,     0,   822,   437,   465,   465,   465,     3,     3,     3,
     446,   821,     0,   985,     0,   986,   987,   981,   919,   795,
       2,     0,   797,   585,   585,   585,   615,   622,   590,     0,
     628,   615,     0,   465,   577,   613,   609,     0,     0,     0,
       0,   616,   618,   822,   630,   630,   630,     0,   610,   626,
     465,   402,     0,   326,   327,   324,   325,     0,     0,     2,
     223,     0,     0,   225,   410,   224,   546,   465,   304,   303,
     305,     0,     2,   184,   263,     0,   256,     0,   184,   319,
     317,     0,   311,  1083,   320,     0,     0,     0,   358,   359,
     360,   361,     0,   351,     0,   352,   328,     0,   329,     0,
       0,   465,   214,   202,   315,   314,     0,   349,   368,     0,
     400,   822,   465,   793,   746,   465,     2,     2,   638,   645,
     645,  1057,  1058,  1059,     0,  1010,   465,     3,     3,     0,
    1018,     0,     0,   645,     0,   652,   651,  1083,     0,   636,
       3,     0,   996,    98,     0,    31,   465,     0,  1083,     0,
       0,    85,     0,    73,     0,    79,     0,    77,    43,   158,
     975,   465,     0,     0,   863,   881,   465,   465,     0,     0,
       0,   465,   465,   722,     0,   423,   425,     3,     3,     3,
       0,     0,     0,     0,     0,   761,   799,   581,   583,   579,
       0,     0,  1025,     0,   623,  1030,   625,  1022,   822,   822,
     608,   629,   612,     0,   611,     0,     0,     0,   632,     0,
     822,   604,   619,   631,   620,   621,   627,   674,   678,   677,
       2,     0,     0,   244,     2,   226,   546,   309,   307,   310,
     306,     0,   308,     0,   252,     0,   184,     0,     2,   465,
     264,     0,   289,     0,     0,   312,     0,     0,     2,   335,
     362,     0,   353,     2,     0,     0,     0,     0,   340,     0,
     336,   200,   199,   424,   763,     0,     0,  1083,  1083,  1060,
       3,     0,     0,  1017,  1019,   637,     0,   645,     0,   635,
       2,    49,    41,    39,    40,     0,    63,   177,    76,     0,
       0,     3,   864,   882,     3,     3,   946,   961,   427,     2,
     664,     3,   663,   721,     0,     0,     0,   855,   912,   920,
     980,   988,   606,     0,     0,     0,  1026,  1027,   822,   607,
    1023,  1024,   605,   586,     0,     0,   334,     0,     0,     0,
     237,     2,   215,     0,     0,     2,   246,   261,   272,   266,
       2,   184,   301,     0,   276,     0,     0,   267,   265,   254,
     257,     0,     0,   184,   290,     0,     0,   218,   333,     2,
     465,   330,     0,     0,   378,     2,   338,     0,    66,     0,
     350,   768,   770,     0,     0,     0,  1020,  1021,   645,  1083,
     657,   760,    64,    80,    78,     0,     0,     0,   465,     0,
     856,   913,   921,   822,  1033,  1035,  1028,     0,   617,   232,
     227,   230,     0,   229,   236,   235,     0,   465,   239,   238,
       2,   248,     0,   245,     2,     0,     0,     0,   253,   258,
       0,     0,   184,   302,   277,     0,     0,     2,     0,   292,
     293,   291,   260,   321,     0,   465,   465,     3,   363,   466,
     367,     0,   371,     0,     0,     0,   379,   380,   221,   341,
       0,     2,     0,     2,     2,   641,   643,  1011,  1083,     0,
     976,   947,   962,   668,     2,  1029,  1031,  1032,   624,     0,
     234,     0,   233,   217,   240,   465,   391,   249,     2,   250,
     247,   262,   275,   273,   269,   281,   279,   280,   278,   259,
     274,   270,   271,   268,   255,     0,     0,     0,     0,   220,
     240,     3,   356,     0,  1025,   364,   365,   366,   378,     0,
       0,     0,   378,     0,     2,   339,   346,     0,   343,   345,
       0,   642,   465,   228,   231,     2,     3,   241,   392,   251,
       0,     0,     0,     0,   300,   298,   295,   299,   296,   297,
     294,     3,   356,     0,     0,  1026,     0,     0,     0,   372,
       0,   381,   222,     0,   336,     0,   640,     3,   210,     0,
       0,     2,   288,   286,   283,   287,   284,   285,   282,     0,
       0,   357,     0,   384,     0,   382,     0,   384,   342,   344,
       2,     0,     0,   212,   211,   216,     0,   219,     0,   354,
     385,     0,     0,   373,     0,   347,     2,  1034,   355,     0,
       0,     0,     0,   348,   386,   387,     0,   383,   374,     0,
       0,   375,   388
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1833,  6805,  6331, -1833,    -1,   203,  2121,  -171, -1833,  -346,
   -1833,   352, -1833,  -723, -1833,   752,  -883, -1006, -1833,   256,
    5765,  2054, -1833,   644, -1833,  1374,   225,   838,   840,   293,
     836,  1336,  1337,  1340,  1345,  1346, -1833,   -96,  -158,  8620,
     890, -1833,  1662, -1833, -1833,  -693,  3004, -1155,  2619, -1833,
      22, -1833,   877,   -42, -1833, -1833, -1833,   416,    49, -1833,
   -1832, -1585,   275,    21, -1833, -1833, -1833,   286, -1539, -1833,
   -1416, -1833, -1833, -1833, -1833,   -20, -1803,   157, -1833, -1833,
     -22, -1833, -1833, -1833,    -6,   444,   446,   105, -1833, -1833,
   -1833, -1833,  -956, -1833,    29,   -32, -1833,   115, -1833,   150,
   -1833, -1833, -1833,   901,  -884, -1116, -1380, -1833,   100, -1406,
      10,  2255,  -967,  -938, -1833,  -283, -1833, -1833,    32, -1833,
    -161,   156,    40,  -253,  4288,  1137,  -643,   165,   140,    65,
    1206,  2168, -1833,  2084, -1833,   154,  4300, -1833,  2025, -1833,
     284, -1833, -1833,    18,   158,  5198,  3332,   -50,  1876,  -298,
   -1833, -1833, -1833, -1833, -1833,  -333,  5714,  5163, -1833,  -396,
     136, -1833,  -692,   237, -1833,   169,   739, -1833,   -40,  -266,
   -1833, -1833, -1833,  -351,  6013,  -891,  1184,   119,  -780,  -429,
    -556,  1682, -1833, -1261,  -155,   260,  1890,   927,  2824,   -29,
    -494,  -262,  -178,  -456,  1326, -1833,  1667,   563,  1229,  1544,
   -1833, -1833, -1833, -1833,   346,  -164,  -188,  -903, -1833,   276,
   -1833, -1833, -1132,   469, -1833, -1833, -1833,  2163,  -795,  -506,
    -976,   -23, -1833, -1833, -1833, -1833, -1833, -1833,   204,  -836,
    -234, -1797,  -126,  8101,   -68,  7511, -1833,  1196, -1833,  2179,
      42,  -214,  -202,  -185,     4,   -61,   -57,   -55,   907,    70,
     107,   142,  -184,   253,  -167,  -146,  -140,   257,  -115,  -111,
     -87,  -682,  -754,  -674,  -670,  -678,   -44,  -669, -1833, -1833,
    -713,  1399,  1405,  1423,  2036, -1833,   583,  7953, -1833,  -563,
    -591,  -569,  -565,  -718, -1833, -1652, -1733, -1728, -1719,  -621,
     -94,  -166, -1833, -1833,   -53,    76,   -62, -1833,  8506,   709,
     815,  -572
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1204,   223,   404,   405,    81,    82,   406,   380,   407,
    1516,  1517,   408,  1011,  1012,  1013,  1315,  1316,  1317,  1528,
     430,   410,   411,   412,   707,   708,   413,   414,   415,   416,
     417,   418,   419,   420,   421,   422,   423,   432,  1110,   709,
    1449,   770,   217,   772,   426,   837,  1205,  1206,  1207,  1208,
    1209,  1210,  1211,  2139,  1212,  1213,  1454,  1641,  1981,  1982,
    1910,  1911,  1912,  2106,  2107,  1214,  1655,  1656,  1657,  1812,
    1813,  1215,  1216,  1217,  1218,  1219,  1220,  1462,  1839,  2034,
    1950,  1221,  1222,  1673,  2124,  1674,  1675,  2017,  1223,  1224,
    1225,  1452,  2025,  2026,  2027,  2171,  2186,  2054,  2055,   302,
     303,   908,   909,  1177,    84,    85,    86,    87,    88,    89,
     463,    91,    92,    93,    94,    95,   231,   232,   589,   284,
     465,   434,   466,    98,   312,   100,   101,   154,   345,   346,
     105,   106,   169,   107,   929,   347,   155,   110,   255,   111,
     156,   263,   349,   350,   351,   157,   427,   116,   117,   353,
     118,   580,   897,   895,   896,  1614,   354,   355,   121,   122,
    1173,  1417,  1620,  1621,  1773,  1774,  1418,  1609,  1792,  1622,
     123,   667,  1713,   664,   356,   665,   666,  1278,  1103,   471,
     472,   901,   902,   473,   474,   903,   358,   584,  1229,   436,
     437,   218,   491,   492,   493,   494,   495,   333,  1249,   334,
     927,   925,   614,   335,   374,   336,   337,   438,   125,   175,
     176,   126,  1243,  1244,  1245,  1246,     2,  1160,  1161,   606,
    1238,   127,   324,   325,   265,   276,   563,   128,   221,   129,
     315,  1112,   887,   525,   167,   130,   678,   679,   680,   131,
     317,   235,   236,   237,   318,   133,   134,   135,   136,   137,
     138,   139,   240,   319,   242,   243,   244,   320,   246,   247,
     248,   805,   806,   807,   808,   809,   249,   811,   812,   813,
     775,   776,   777,   778,   526,  1153,  1395,   140,  1721,   639,
     640,   641,   642,   643,   644,  1776,  1777,  1778,  1779,   629,
     476,   361,   362,   363,   439,   209,   142,   143,   144,   365,
     829,   645
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   712,   546,    80,   343,   132,   191,  1020,   560,   379,
     357,    90,   375,   193,   151,   507,   543,   194,   588,   195,
     668,  1247,  1000,   425,  1226,   955,   200,   508,  1644,  1644,
     208,   826,  1400,    96,  1443,   522,   179,  1949,  1893,   497,
    1251,  1106,   941,  1894,   509,   510,   647,  1073,   919,   884,
     880,   882,  1895,   718,  1643,    80,    80,  1412,    80,  1121,
     892,  1055,   511,   132,   942,   993,   104,   950,   943,    90,
    1677,   360,  1230,    80,   949,  1990,  1280,   141,   307,  1989,
     141,   596,    80,   512,  2058,   424,  1083,   206,   596,   513,
      80,    96,  1090,  1568,  1569,    80,   208,  1168,    80,   507,
     238,   921,    80,   266,   150,   636,   523,   277,   528,   259,
    1983,   508,  1984,   271,   514,   536,    58,    58,   515,  1824,
    1897,  1079,   371,   310,   104,  1080,  1239,  1074,   509,   510,
     442,  1075,  1076,  1320,   443,   141,   444,  1236,   234,   458,
      80,   103,   516,    80,   196,    80,   511,    58,   618,  1678,
     132,    80,   219,   752,   298,   108,    90,    97,    80,   113,
     152,   505,  1327,  -771,   261,    80,   102,   512,   193,   647,
     219,   661,   194,   513,   195,    58,   969,  1991,    96,   141,
    -393,   197,   595,   597,  1362,  1399,  2059,   618,   145,    80,
      80,  -772,  1403,   657,   206,   753,   296,   660,   514,   103,
     524,   524,   515,   270,    80,   552,  1413,   148,  1392,   479,
     941,   104,  1679,   108,   488,    97,   198,   113,  1363,    80,
     469,   220,   141,  -803,   102,  1361,   516,   533,    80,    80,
    1393,   524,   942,  1084,   206,  1414,   943,  1087,  1817,   204,
     949,  -803,   575,   500,  1976,   600,  1100,  1101,   630,    80,
    1284,  -393,   193,   158,  1983,   165,   194,    80,   195,   173,
     173,   445,   206,  1472,  1105,  1105,   872,    80,    80,   552,
    1985,    80,   518,  1989,   876,  1644,   564,  1926,    80,  1308,
    1680,  -394,   561,  1105,   159,   112,   103,  1059,  1412,  1412,
    1412,   844,    80,    80,   173,    80,   305,  1429,   446,   196,
     108,  1643,    97,   845,   113,   204,  1530,   164,   878,  1989,
     914,   102,    80,    80,   883,   830,   966,  1073,   206,    20,
     846,   847,    80,   662,   592,   292,  1226,   647,   663,    80,
      80,  1949,  1018,   447,    80,   378,   197,   562,   848,   976,
    1372,  1334,   178,   112,   173,  1430,  1277,   173,  1299,   241,
    1893,   647,  -394,   245,  1083,  1894,   518,   456,   647,   849,
     627,  1105,   173,   650,  1895,   850,  2086,   630,   713,   369,
    1401,   198,    80,  2023,  1230,    80,   677,   627,   208,  1720,
    1501,   627,   624,  1165,   180,  1270,   844,   274,  1508,   532,
     851,  1104,  1104,   181,   852,   797,  1347,  1074,   845,   655,
    1348,  1075,  1339,   658,  1535,  1931,  1932,   297,  2050,   261,
    1104,   189,   548,  1644,   551,   846,   847,   557,   853,   711,
     201,   810,  -954,   173,  1633,  1601,  1425,  1426,   568,  -954,
     112,   919,  1897,   848,   297,   200,  1536,  1413,  1413,  1413,
      80,   591,   531,   343,   442,   479,  1422,    58,   443,   539,
     444,  1635,    63,    64,   849,   108,   936,    97,  2031,   113,
     850,    -3,   475,    80,    80,  1423,  1414,  1414,  1414,  2105,
     556,   173,   173,  1976,  1461,    80,    80,   212,   551,   627,
     941,   567,   173,   519,    80,   851,   488,   520,  1104,   852,
    1568,  1569,  2032,   961,  2160,  2105,   578,   173,  1815,   583,
      76,   624,   942,  1823,    80,  1627,   943,  2000,  2001,  1194,
     360,    58,   719,   853,    58,  1270,    80,   720,  1274,   611,
      58,   479,  2141,  1353,  1628,   297,   173,   603,  1733,  1735,
    1737,   524,   261,   173,   173,  1907,  1908,    58,   173,   442,
    2049,    80,  1427,   443,    58,   444,  1494,    80,   612,   613,
    1642,  1658,   630,   873,   160,   569,   226,   161,   162,   864,
     163,   877,  1907,  1908,  1105,  2051,  2052,   519,   469,   214,
     581,   520,   619,   292,  1363,   445,   173,   933,   885,   173,
     215,  1644,   189,   920,  1044,   112,    80,   204,    80,   893,
    -955,   788,  1329,   911,  1060,   524,   216,  -955,   524,    80,
    1081,    80,   647,    58,   634,    80,   479,  1123,    58,  1644,
     132,  1422,   446,   796,    58,    80,    90,  1088,  1909,    80,
      80,   634,   469,   250,  1134,  1560,   995,   189,   524,  1539,
    1690,   234,    58,  1146,  1781,  -582,  1997,   955,    96,   647,
     627,   469,    58,   269,   864,  1936,  1732,   447,  1424,  1644,
    1114,  -459,    80,  1782,   975,   261,  1094,   978,   979, -1084,
     980,  1925,   934,   721,   627,    80,   292,   173,   722,   982,
     662,   104,   984,   985,   986,   663,  1627,   627,   988,   173,
     173,  1790,   141,  1138,   995,   294,   954,   524,  1142,   989,
     990,  1104,   524,    58,  1375,  1784,    58,  1147,   524,   960,
    1791,  1632,   296,  1825,    58,   711,   575,  2043,   297,    58,
    1785,   711,  1379,  1790,   562,   891,   524,  -634,   424,    80,
     711,    80,  1383,    80,  -634,  1898,   524,    80,  1323,  1109,
      80,   919,  1892,   591,  1790,  1319,   874,   995,    58,   711,
     995,   742,   743,   152,  1899,    58,   103,   108,   995,    97,
    2006,   113,  1666,  1902,  1480,    80,  1995,   995,  2076,   810,
     108,   886,    97,   938,   113,  -393,  1999,  2078,   890,  1255,
     865,   102,   894,  1492,   866,   469,   311,   634,  2012,    14,
      15,    16,    17,    18,  1545,   744,   745,  -792,   524,  1554,
     148,   995,   211,   524,   995,   378,  1303,  1642,  1567,   189,
      80,  2111,    80,  1304,  2113,   331,  -697,   615,  1847,  1848,
     475,   616,    73,  1527,    80,   296,   469,   448,  1558,   173,
    1319,    80,   634,   930,   932,  2044,   343,   488,   373,   524,
      80,   160,   773,  1730,   161,   162,   524,   163,    58,    80,
      80,    80,   241,    78,    79,  1360,   912,  2069,  1525,   251,
     252,  1869,   253,  1870,   376,   865,   958,   254,   211,   866,
     735,    14,    15,    16,    17,    18,    80,   736,   737,   173,
      14,    15,    16,    17,    18,   274,  1818,   112,   296,    73,
     448,  1819,   524,   475,    14,    15,    16,    17,    18,  1943,
     112,   754,  2091,   360,  1944,   755,  1154,  2092,   440,   633,
    -463,  1484,  1485,   634,  2156,    80,    80,   488,  1162,  2157,
      78,    79,  1166,   780,  1770,  1273,  1169,   781,    90,  1783,
      58,   377,   533,  1004,   857,  1006,   524,  1009,   792,    58,
     919,  1017,   524,  1241,  1021,  1658,   669,  1367,   938,   671,
      96,   449,   906,    58,   297,  1324,  1807,  1808,  1809,   450,
    1722,   831,   832,   627,   496,   833,   650,    73,    80,  1046,
    1023,  1024,  1025,    73,   647,   261,  1959,   553,  1810,   478,
     603,   677,   448,   104,   524,  1254,   451,   633,   562,   482,
      73,   634,   452,   633,   141,   498,  1663,   634,    78,   635,
     207,  -464,   483,   905,    78,   635,    19,   906,   453,   141,
     633,   636,   261,   239,   634,   469,   267,    80,  1473,   454,
     278,    78,   635,  1608,  1496,    80,   501,   475,   173,   502,
    1390,  1109,  1081,  1240,   448,   173,   634,   503,   968,   688,
     504,   553,   616,  1346,   810,  1030,  1031,  1032,  1033,    52,
      53,    54,    55,  1122,    80,  1124,   521,   488,  1228,   970,
    1717,   632,   971,   616,   732,   733,   972,   994,   475,  1118,
     522,   995,   108,  1119,    97,  1064,   113,  2038,  1728,   524,
      80,   199,    64,   102,   296,   732,    80,    80,   524,  1242,
     475,   475,   147,   574,    64,   375,   375,    65,    66,    67,
      68,    69,    70,    71,  1015,  1167,   533,   207,   544,   475,
     524,   545,  1156,  2056,  -460,   732,   995,    80,   219,  1176,
     173,   173,   480,  1458,    14,    15,    16,    17,    18,  1507,
    1419,   654,   448,  1158,   524,  1513,   343,   995,  1590,   603,
     555,  2056,  2126,   524,  1016,   566,  2130,   207,   585,   147,
     211,   607,   171,   172,    65,    66,    67,    68,    69,    70,
      71,  1645,  1714,   182,     6,     7,     8,     9,    10,    11,
      12,    13,  1272,   622,   630,   207,  1807,  1808,  1809,   663,
     632,  2108,  1318,    58,  1241,   670,  1319,   475,   995,   565,
    1479,   424,   488,   681,   781,    80,    80,    80,  1810,  1402,
    1512,    90,   112,   360,  1319,  1881,   682,  1816,   685,   186,
    1570,   715,  1428,  1725,  1576,  1577,  1800,  1726,   686,   488,
    1319,   687,  1459,    96,   279,    80,   734,  1801,  1827,  1447,
      90,   995,   995,    80,    73,  2030,    80,    80,   691,  1540,
      80,   266,   277,   738,   739,  1828,   260,   259,   271,  1119,
     141,    80,    96,   748,  1771,   749,   104,   282,   524,   289,
     751,   291,   756,   440,   440,    78,    79,   141,   750,  1829,
     782,  1903,  1518,   995,  1240,   781,   424,   424,   187,  -461,
    1767,  1768,  1769,  1992,    80,   104,   627,   995,   783,    14,
      15,    16,    17,    18,  2095,  2158,   141,    80,  1319,   995,
     260,   455,   261,   289,   291,  2182,   740,   741,   954,  2179,
    1807,  1808,  1809,   488,   141,   469,  1404,  1405,  1406,  2189,
     280,    80,   995,  2190,   281,   746,   747,   285,   784,   290,
    1242,  1228,  1810,   201,   715,   343,   785,   562,   270,  1415,
     786,  -185,  1807,  1808,  1809,   108,   787,    97,    58,   113,
     622,   715,   260,    80,   997,   998,   102,    -3,   480,   814,
    1228,  1419,  1419,  1419,  1810,   956,  1610,  1419,  1793,  1793,
    1793,  -462,  1856,  1811,   108,   828,    97,   -17,   113,  -121,
    -121,  -121,  -121,  -121,  -121,   102,  1096,  1097,  1022,   440,
     475,  1668,  1669,  1670,  1671,  1672,   173,  1098,  1099,   173,
     173,   173,   360,  1306,  1119,  1321,  1322,   838,  1645,   995,
    1325,  -156,  -156,   507,  1098,  1471,  1533,  1534,   854,   260,
     827,   289,   291,   173,    80,   508,   855,   151,    80,   173,
     870,    80,  1538,  1534,   480,  1542,  1534,   583,  1070,  1526,
    1578,  1526,   509,   510,   173,  -120,  -120,  -120,  -120,  -120,
    -120,   488,   856,   260,    90,    90,   858,  1634,  1636,   260,
     511,    14,    15,    16,    17,    18,   992,  1624,  1070,  1592,
     859,   488,   860,    80,   861,   112,   868,   280,   564,  1738,
    1119,   512,  1867,  1119,   561,  1868,  1534,   513,   173,   862,
     964,   260,   863,   141,   888,  1688,   304,   652,   907,   291,
     440,  1514,  1878,  1879,   112,  1890,   995,  1947,  1948,   104,
     104,   889,   514,   869,   469,   488,   515,   150,   147,  1836,
     141,   141,   274,    65,    66,    67,    68,    69,    70,    71,
    -580,  1241,  -578,   488,   898,   695,  1963,  1534,    80,   562,
     516,   343,   922,    80,    80,    80,   147,  1570,   924,   171,
     172,    65,    66,    67,    68,    69,    70,    71,   928,  1786,
    1964,  1534,   280,   281,   944,   651,   946,   290,  1907,  1908,
    1415,  1415,  1415,   152,  1606,  1607,  1611,  2179,  2180,  1531,
    1532,   636,   844,   963,  1647,  1647,  1026,  1027,   260,   967,
    1028,  1029,  1034,  1035,   845,  1794,  1795,   141,   108,   108,
      97,    97,   113,   113,   696,  1481,  1482,  1570,   360,   102,
     102,   846,   847,   973,   260,    80,   652,   291,   974,  1436,
      80,  1240,  1952,   996,   475,   475,    80,   999,    80,   848,
    1002,   695,   173,  1715,  1716,   173,    80,  1293,  1048,  1043,
    1069,  1070,  1297,   440,  1077,  1116,  1125,  1126,   488,  1127,
     849,  1113,  1128,  1305,  1129,  1130,   850,  1131,  1624,  1132,
    1133,   488,  1148,  1624,   260,  1157,   259,   271,  1159,  -775,
    1163,   518,  1170,  1171,  1942,   173,  1172,  1242,  1625,  -673,
    1231,   851,  1626,  1264,  1232,   852,  1248,  1265,  1266,   260,
    1276,   672,  1279,  1277,   260,  1281,   260,  1282,   488,  1285,
     696,    90,  1286,  1289,  1830,  1288,  1290,  1518,  1291,   853,
    1292,  1294,  1295,  1296,  1301,  1241,  1302,   260,  1309,   260,
     260,   261,    14,    15,    16,    17,    18,  1298,   112,   112,
    1310,   647,  1326,   141,  1331,  1332,  1333,   260,  1340,  1341,
    1342,  1343,  -661,    80,  -660,    80,  1351,  1366,  1374,   260,
    1396,  1391,  -776,  1420,  1431,  1434,   104,   270,  1421,  1980,
    1435,  1444,  1451,  1445,  1446,  -696,   673,   141,  1453,  1510,
    1455,   995,   260,  1461,   652,   291,  1465,  1468,   280,  1469,
    1599,   141,   674,  2024,  1467,   675,   676,    65,    66,    67,
      68,    69,    70,    71,  2085,    80,   260,   652,    80,  1524,
    1475,  1477,  1526,   260,  1541,  1240,  1553,  1566,  1571,   488,
    1615,  1572,  1574,   488,  1573,  1534,  1579,  1582,  1597,  1598,
    1602,   424,  1613,  1570,  1616,  1424,   561,   488,  1659,  1638,
    1683,  1647,  1660,  1681,  1691,   688,  1686,   488,  1693,    90,
     173,   864,  1694,  1662,  1664,   108,  1696,    97,  1676,   113,
    1699,  1684,   173,  1685,    80,    80,   102,  1697,  1698,  1701,
    1700,  1242,  1702,  1703,  1705,   173,  1719,  1723,  1710,  1625,
    1739,  1724,  1731,  1626,  1625,  1727,  1740,  1744,  1626,   507,
    1745,   562,   519,   448,  1753,  1578,   520,  1755,  1765,  1766,
    1618,   508,  1802,  1804,   104,  1780,  1833,  2020,  1835,   220,
    1798,  2103,   173,  1980,  1840,   141,  1855,  1849,   509,   510,
    1853,  1854,   732,  1859,    80,  1861,  1857,  1866,  1872,  1873,
     488,  1876,  1877,  1887,   488,  1888,   511,  2024,   147,   488,
    1883,  2024,  2024,    65,    66,    67,    68,    69,    70,    71,
    1007,   274,  2128,  1889,  1915,  1920,  1935,   512,  1519,  1520,
    1521,  1921,  1933,   513,   488,  1522,  1523,  1624,  1939,  1941,
    1956,  1945,  1946,   424,  1194,   424,  2154,   440,   627,  1647,
    1957,  1958,  1960,  2020,  1961,   112,  1962,   956,   514,  1994,
    1008,  -662,   515,   108,  1970,    97,  1971,   113,  1972,  1973,
    2018,  1974,  2170,  1975,   102,  -563,  2170,   524,  2007,   488,
    2035,  1996,  2021,   488,   424,  2002,   516,    90,  2013,  2036,
    2005,  2037,  1879,   173,  2040,  2022,   488,   173,  2184,  2041,
    2042,   475,   475,  2053,  2181,   600,  2062,    80,  2079,    80,
    2075,   173,   193,  2089,  2088,    90,   194,  2151,   195,  2077,
     488,   173,   488,   488,   627,  2090,  2093,   260,  2094,  2097,
    2101,  2110,   865,  2112,  2114,  2123,   866,  2129,   173,  2127,
     260,  2134,   104,  2135,  2136,    83,  2018,   488,   149,  2142,
    2153,  2152,  2155,   141,  2168,    90,  1483,  2165,  2167,  2176,
    2172,  1537,  2173,  2188,   260,  2177,   424,  2187,  1863,  2191,
     104,   991,    80,    80,  1036,   260,  1037,  1457,   206,  1450,
    1038,   141,   771,   488,   260,  1509,  1039,  1837,  1040,  2166,
    2104,  2121,  1937,   112,   488,  1930,  2033,    14,    15,    16,
      17,    18,  2159,    83,   173,  2161,  2150,  1831,   173,  1832,
     104,  2081,  2131,   173,    80,  2174,   518,  1647,   190,   479,
    2080,   141,  1466,   170,   267,   278,   287,    83,   488,   554,
     488,   108,  1978,    97,  1543,   113,  2048,  1275,   173,  1612,
     230,  1463,   102,   258,  1115,  1647,  1250,    83,  1625,   488,
     926,   834,  1626,     3,  1844,   488,    58,   864,  1283,   108,
       0,    97,  1051,   113,  2169,   488,   147,  1764,  1052,    80,
     102,    65,    66,    67,    68,    69,    70,    71,     0,    80,
    2178,   260,     0,   173,   149,  1647,  1053,   173,     0,     0,
      83,     0,     0,   149,     0,     0,   314,   322,     0,   108,
     173,    97,     0,   113,     0,   260,     0,     0,     0,   342,
     102,     0,     0,  2087,  1344,    75,    73,     0,   475,   723,
     188,   724,   725,   726,   173,     0,   173,   173,     0,     0,
       0,     0,     0,   431,   190,   190,   773,     0,     0,     0,
     524,     0,     0,   192,     0,   149,   461,    78,    79,   258,
     727,   173,     0,   728,   729,     0,     0,   262,   730,   731,
       0,   112,     0,     0,     0,   233,   535,     0,   283,   286,
       0,     0,     0,   230,   230,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,   173,     0,   112,
       0,     0,     0,     0,   314,     0,     0,     0,   173,     0,
       0,     0,    83,     0,     0,     0,  1712,     0,     0,     0,
       0,   262,   440,  1718,     0,     0,   258,     0,     0,     0,
    1838,   316,     0,     0,     0,     0,   489,   519,   203,   112,
    1729,   520,   173,     0,   173,   147,    58,     0,   171,   172,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
     322,     0,     0,   173,   260,     0,   322,   314,   314,   173,
       0,   565,     0,   262,     0,     0,   149,     0,   865,   173,
       0,   147,   866,  2185,   574,    64,    65,    66,    67,    68,
      69,    70,    71,  2192,     0,     0,   342,   637,   646,   260,
       0,     0,     0,     0,   203,   260,    73,     0,   506,   233,
       0,     0,     0,   342,     0,     0,     0,   342,  1438,   250,
     203,     0,     0,    19,     0,     0,  1771,     0,     0,   316,
     524,     0,     0,  1045,     0,     0,     0,    78,    79,     0,
     262,     0,     0,     0,   203,     0,     0,     0,     0,     0,
       0,   431,     0,     0,     0,     0,     0,   464,   326,   327,
     328,   329,    48,    49,    50,    51,    52,    53,    54,    55,
     779,     0,   904,     0,   262,     0,     0,     0,  1826,     0,
     262,     0,     0,     0,     0,   431,   790,     0,   774,   793,
       0,     0,   601,   316,     0,   190,     0,   147,     0,     0,
     171,   172,    65,    66,    67,    68,    69,    70,    71,     0,
       0,   149,   262,   203,     0,   461,     0,     0,     0,   803,
     147,   646,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,  1858,   182,     6,     7,     8,     9,    10,    11,
      12,    13,     0,  1865,     0,     0,   535,     0,    73,   330,
       0,     0,     0,   260,     0,     0,   697,  1155,     0,   230,
       0,     0,     0,     0,     0,     0,     0,   331,  1617,    75,
       0,   230,     0,     0,     0,  1618,     0,   203,     0,    78,
      79,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,  2029,     0,     0,     0,     0,   314,   203,   431,   431,
       0,   260,   314,     0,   342,     0,   824,     0,   489,   147,
       0,     0,   171,   172,    65,    66,    67,    68,    69,    70,
      71,   147,  1263,     0,   227,   228,    65,    66,    67,    68,
      69,    70,    71,     0,     0,   262,     0,     0,     0,     0,
       0,     0,     0,   314,   804,     0,     0,     0,     0,    73,
       0,     0,   697,     0,   314,     0,   314,     0,   342,     0,
      83,     0,  1953,  1954,     0,     0,    58,     0,  1440,  2083,
      75,     0,     0,   524,     0,     0,   342,   461,     0,   646,
      78,    79,     0,     0,   843,     0,   203,   637,     0,     0,
       0,   637,     0,     0,     0,     0,   233,     0,   147,     0,
     342,   227,   228,    65,    66,    67,    68,    69,    70,    71,
     646,     0,     0,   342,     0,     0,   203,   262,     0,     0,
       0,   316,     0,     0,   149,     0,     0,   316,     0,     0,
       0,     0,  1330,     0,     0,     0,     0,   431,   262,     0,
     149,   149,     0,   431,     0,     0,  1344,    75,     0,  1337,
    1338,     0,   431,     0,     0,   149,   149,   149,   262,     0,
       0,     0,     0,     0,     0,   904,     0,     0,   316,     0,
       0,     0,     0,     0,  2039,     0,     0,     0,     0,   918,
     147,   316,     0,   260,     0,    65,    66,    67,    68,    69,
      70,    71,  1311,   262,     0,     0,  1312,     0,  1313,   203,
     203,     0,     0,     0,     0,   464,     0,     0,    58,   779,
     779,   461,     0,     0,     0,     0,     0,   262,     0,  1062,
       0,     0,  1065,     0,   262,     0,     0,   774,   774,    75,
     904,     0,  1529,     0,     0,   431,     0,     0,     0,     0,
     147,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,   461,  2100,     0,   803,     0,   803,     0,   203,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,   342,   342,     0,     0,   464,     0,
     174,   177,     0,   535,     0,     0,     0,     0,   229,    75,
    1136,     0,     0,   342,  1140,   314,     0,     0,  1144,    78,
      79,   203,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,   222,     0,     0,     0,     0,
       0,   314,     0,     0,     0,   203,   260,   147,     0,     0,
     227,   228,    65,    66,    67,    68,    69,    70,    71,     0,
     904,     0,     0,   260,     0,     0,     0,     0,     0,   489,
       0,     0,   824,     0,     0,    73,     0,   904,   904,     0,
       0,     0,   431,     0,     0,   308,     0,    58,   309,     0,
       0,   342,     0,     0,     0,  2083,    75,   149,   431,   524,
    1072,     0,   804,   332,     0,     0,    78,    79,     0,   342,
       0,  1258,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,   637,     0,    65,    66,    67,    68,    69,    70,
      71,     0,   464,     0,     0,     0,     0,  1549,  1550,     0,
     316,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,  1564,  1565,   260,     0,     0,   203,     0,     0,  1151,
       0,   461,     0,     0,   499,     0,   316,    74,    75,     0,
       0,     0,     0,   464,     0,   147,     0,     0,    78,    79,
      65,    66,    67,    68,    69,    70,    71,     0,   262,  1587,
    1588,  1589,     0,     0,     0,   464,   464,     0,     0,     0,
       0,   262,     0,     0,     0,     0,     0,     0,     0,     0,
     779,     0,   558,   559,   464,     0,     0,     0,     0,     0,
       0,     0,    58,   174,    75,   262,   147,   823,   774,   576,
     577,    65,    66,    67,    68,    69,    70,    71,   174,     0,
       0,     0,     0,     0,     0,   803,     0,     0,     0,  1314,
       0,     0,   803,     0,   147,     0,     0,  1314,     0,    65,
      66,    67,    68,    69,    70,    71,     0,   605,     0,     0,
       0,     0,     0,     0,   608,   610,   260,    76,     0,   617,
       0,  1377,    73,     0,  1381,     0,  1314,     0,  1385,   489,
       0,     0,   464,     0,   342,     0,     0,     0,     0,   203,
       0,   147,    74,    75,   171,   172,    65,    66,    67,    68,
      69,    70,    71,    78,    79,     0,     0,   332,   147,     0,
     332,   366,   367,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,   149,     0,
       0,     0,     0,     0,     0,   904,   904,   149,     0,  1314,
      14,    15,    16,    17,    18,   431,     0,   609,     0,   904,
     904,     0,   203,     0,     0,     0,     0,     0,     0,    76,
    1072,     0,     0,     0,   368,     0,  1345,   804,     0,     0,
       0,     0,   260,     0,   431,     0,  1757,  1758,  1759,     0,
       0,     0,     0,     0,     0,     0,     0,   904,   904,   904,
       0,   258,    83,     0,     0,     0,     0,     0,   222,    58,
       0,     0,     0,     0,     0,     0,   314,     0,   147,     0,
     818,   819,   149,    65,    66,    67,    68,    69,    70,    71,
    1311,     0,   461,     0,  1312,     0,  1313,     0,     0,     0,
       0,   147,     0,     0,   227,   228,    65,    66,    67,    68,
      69,    70,    71,   115,     0,     0,   115,     0,     0,     0,
       0,   461,     0,     0,     0,   149,     0,    75,     0,    73,
    1734,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,  1547,     0,   801,
      75,     0,     0,   634,     0,   464,  1556,     0,     0,     0,
      78,   802,     0,     0,     0,     0,     0,     0,  1850,     0,
       0,   115,     0,     0,     0,   262,     0,     0,     0,     0,
       0,   710,     0,     0,     0,     0,     0,     0,   342,   342,
       0,     0,     0,    58,     0,   115,     0,     0,     0,     0,
       0,   316,     0,  1871,     0,   489,     0,     0,  1874,  1875,
     262,   264,     0,  1314,     0,   115,     0,     0,     0,     0,
     332,     0,     0,     0,     0,   147,     0,     0,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,   149,   149,
     149,   149,     0,   149,   149,     0,     0,     0,     0,  1619,
     322,     0,   115,    73,   904,   904,   904,     0,   115,     0,
       0,   115,     0,     0,     0,   264,     0,     0,   431,   431,
     965,     0,     0,   229,    75,     0,   338,   115,   370,     0,
       0,     0,     0,   203,    78,    79,     0,     0,     0,     0,
       0,     0,     0,   203,     0,     0,     0,     0,   258,     0,
    1799,   435,     0,  1561,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   115,   435,     0,     0,   264,    58,     0,
     461,     0,   203,     0,     0,     0,     0,   147,   879,   881,
     227,   228,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,   489,  1595,   149,     0,   637,     0,     0,
     147,     0,     0,   227,   228,    65,    66,    67,    68,    69,
      70,    71,     0,     0,   262,     0,     0,     0,   115,     0,
     115,     0,     0,     0,  1623,     0,   904,     0,    73,     0,
       0,     0,     0,   931,   264,     0,     0,     0,     0,   464,
     464,     0,     0,     0,     0,     0,     0,     0,   313,    75,
       0,     0,     0,     0,     0,   579,     0,   489,     0,    78,
      79,   904,   262,   115,     0,     0,   904,   904,   264,  1095,
       0,     0,     0,  1775,   264,   489,  1107,     0,     0,     0,
    1314,     0,     0,     0,   115,  1314,  1314,  1314,     0,     0,
    1619,  1772,     0,     0,     0,  1619,     0,   431,     0,     0,
       0,  1619,     0,  1619,   115,   147,   264,   115,   199,    64,
      65,    66,    67,    68,    69,    70,    71,   710,     0,     0,
       0,   115,     0,   710,     0,   115,     0,     0,     0,     0,
     322,   149,   710,   147,     0,     0,   227,   228,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,   710,     0,     0,    75,     0,     0,   823,     0,   435,
       0,  1178,    58,     0,     0,   431,     0,     0,     0,     0,
       0,   203,     0,     0,     0,     0,   342,     0,     0,   149,
       0,    14,    15,    16,    17,    18,     0,  1042,     0,  1271,
       0,     0,     0,   435,   147,     0,     0,   227,   228,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
     149,     0,     0,     0,  2137,  1623,     0,     0,     0,   115,
    1623,     0,    73,   435,     0,     0,  1787,     0,  1623,   264,
       0,     0,  1433,     0,     0,   342,   342,  1775,  1775,     0,
      58,     0,  1617,    75,   262,     0,     0,     0,     0,     0,
       0,     0,     0,    78,    79,  1772,  1772,   147,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
    1619,     0,   147,  1619,     0,   227,   228,    65,    66,    67,
      68,    69,    70,    71,     0,  1314,   147,  1314,     0,     0,
     322,    65,    66,    67,    68,    69,    70,    71,   203,     0,
      73,     0,     0,   431,     0,     0,   435,   435,    76,     0,
       0,   264,   115,     0,    73,     0,     0,     0,     0,     0,
    2083,    75,     0,     0,   524,     0,     0,     0,     0,     0,
     314,    78,    79,     0,  1071,    75,     0,     0,   634,     0,
       0,     0,     0,     0,   115,    78,    79,     0,     0,   115,
       0,     0,   264,   115,     0,   115,     0,     0,     0,     0,
       0,     0,  1775,     0,     0,     0,   115,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,   203,     0,     0,
    1772,     0,   370,     0,   115,   435,     0,   264,     0,  1619,
       0,     0,     0,     0,     0,  1904,     0,     0,  1623,     0,
       0,     0,     0,     0,   262,     0,     0,     0,   115,     0,
       0,   264,     0,     0,     0,   579,     0,     0,   264,     0,
       0,   115,   904,   962,   149,     0,     0,     0,     0,     0,
       0,     0,   115,     0,     0,     0,   464,   464,     0,  2046,
    1437,  1439,  1441,  1775,     0,   435,     0,     0,   115,   115,
       0,   435,   342,     0,     0,   316,     0,     0,     0,     0,
     435,  1772,     0,   115,   115,   115,     0,     0,     0,     0,
    1460,   149,     0,     0,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,  1775,  1178,     0,   147,     0,     0,
     171,   172,    65,    66,    67,    68,    69,    70,    71,   149,
     149,     0,  2084,   322,     0,     0,     0,     0,     0,     0,
       0,  1687,     0,     0,  1623,   147,     0,     0,     0,   435,
      65,    66,    67,    68,    69,    70,    71,  1311,     0,  1505,
       0,  1312,     0,  1313,    58,   478,     0,     0,     0,   149,
       0,     0,     0,   435,    14,    15,    16,    17,    18,  1775,
    1775,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     435,     0,     0,     0,    75,     0,   147,  2084,  2084,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,   115,   115,     0,     0,     0,     0,     0,     0,
       0,  1775,     0,     0,    73,     0,     0,     0,     0,     0,
       0,   115,     0,    58,     0,     0,     0,     0,     0,  2084,
       0,     0,     0,     0,   313,    75,     0,   262,     0,     0,
       0,     0,     0,     0,     0,    78,    79,   316,     0,     0,
       0,     0,     0,  1152,     0,   147,     0,   115,   227,   228,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,   464,     0,     0,     0,     0,     0,     0,
       0,     0,   264,    73,     0,     0,     0,     0,     0,     0,
     435,     0,     0,  1629,     0,   264,  1631,     0,     0,   115,
       0,     0,     0,  1617,    75,   115,   435,     0,  1803,     0,
       0,     0,   601,   316,    78,    79,     0,   115,     0,  1260,
     435,  1814,   115,     0,   147,     0,     0,   227,   228,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,    99,
       0,     0,   153,     0,     0,     0,     0,     0,     0,     0,
       0,   109,    73,     0,   316,     0,     0,   147,  1842,     0,
     227,   228,    65,    66,    67,    68,    69,    70,    71,   435,
       0,     0,   801,    75,     0,     0,   634,     0,     0,     0,
       0,     0,     0,    78,   802,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   636,    99,     0,     0,
       0,     0,     0,     0,     0,  1617,    75,     0,     0,   109,
       0,     0,  1618,     0,     0,     0,    78,    79,   387,     0,
     388,   205,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   115,     0,     0,     0,     0,     0,     0,     0,
       0,   272,     0,     0,     0,     0,     0,     0,     0,   115,
     115,   147,     0,   273,   171,   172,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,   717,  1906,
       0,    76,   398,  1916,     0,     0,     0,     0,   306,     0,
       0,     0,     0,     0,    99,  1637,     0,  1929,  1640,  1654,
       0,     0,     0,     0,  1661,     0,   109,  1938,  1665,   482,
    1667,  1796,   115,   344,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   147,     0,   348,   227,   228,    65,    66,
      67,    68,    69,    70,    71,     0,   147,     0,   441,   227,
     228,    65,    66,    67,    68,    69,    70,    71,     0,   306,
     467,    73,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,   468,     0,    73,   115,     0,     0,     0,     0,
       0,   229,    75,   435,     0,     0,     0,     0,   517,     0,
       0,     0,    78,    79,   313,    75,     0,     0,     0,     0,
    1988,     0,     0,     0,  1993,    78,    79,     0,   542,  1998,
       0,     0,   435,   547,   549,     0,   205,   758,   759,   760,
     761,   762,   763,   764,   765,   766,   767,   768,     0,   264,
     115,   214,     0,     0,  2028,     0,     0,     0,     0,   570,
       0,     0,     0,   572,     0,     0,     0,     0,   573,     0,
     115,   571,     0,     0,     0,     0,     0,     0,   769,   590,
     435,     0,     0,     0,  1260,     0,  1763,     0,     0,     0,
       0,   109,   602,     0,     0,     0,  1504,     0,     0,  2057,
       0,     0,     0,  2060,     0,     0,     0,     0,   115,   435,
       0,     0,     0,   115,     0,     0,  2074,     0,     0,     0,
     625,     0,     0,   649,     0,     0,     0,     0,     0,     0,
       0,     0,   626,     0,     0,   273,  1805,   656,     0,     0,
    2096,   656,  2098,  2099,     0,     0,     0,     0,     0,   626,
       0,  1820,  1822,   626,     0,     0,     0,   115,   115,  1951,
       0,     0,     0,     0,     0,     0,     0,  2109,     0,     0,
       0,   115,   115,   147,     0,  1640,   115,   115,    65,    66,
      67,    68,    69,    70,    71,  1311,     0,     0,     0,  1312,
     147,  1313,     0,   171,   172,    65,    66,    67,    68,    69,
      70,    71,     0,  2132,     0,     0,     0,     0,     0,   115,
     115,   115,     0,     0,  2138,     0,     0,     0,  1596,     0,
       0,     0,    75,     0,     0,  1736,   115,   115,   115,   115,
     115,   115,   115,     0,     0,   306,     0,     0,   264,   625,
       0,     0,     0,     0,     0,     0,     0,     0,  2164,     0,
    2138,   626,     0,     0,     0,     0,   435,   435,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2175,
       0,     0,     0,     0,     0,  2164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2183,   264,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1914,     0,     0,     0,
       0,     0,     0,     0,     0,  1917,     0,  1919,   435,     0,
    1924,  1928,     0,  1654,     0,     0,     0,     0,  1934,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   467,     0,
       0,     0,     0,   115,     0,     0,     0,     0,     0,     0,
     468,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     900,     0,     0,     0,     0,   549,     0,     0,     0,   913,
       0,   590,   348,     0,     0,     0,     0,     0,     0,     0,
       0,   273,   344,   109,    99,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   468,     0,   109,     0,     0,     0,
     656,   937,     0,     0,     0,     0,   115,   115,   115,     0,
       0,     0,   626,   468,     0,   948,     0,     0,     0,     0,
    2004,     0,     0,     0,   625,  2009,  2011,     0,     0,   957,
       0,     0,     0,     0,     0,   435,   626,   656,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   626,
       0,     0,   115,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   264,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2061,
       0,  2064,     0,     0,  2066,  2068,     0,     0,     0,  2071,
    2073,     0,     0,   435,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,     0,     0,   467,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   468,   115,     0,
       0,     0,  1054,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   115,     0,     0,     0,     0,   115,   115,
    2116,  2118,  2120,   115,   115,     0,   937,     0,     0,     0,
       0,  1078,     0,     0,     0,     0,     0,     0,   468,     0,
       0,  2133,     0,     0,     0,     0,     0,     0,   467,   467,
       0,     0,     0,     0,     0,  2144,  2146,  2148,     0,     0,
     348,   348,     0,     0,     0,     0,     0,   467,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   264,   348,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   435,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   900,   120,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   348,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1227,     0,     0,   114,
       0,     0,     0,     0,     0,   467,     0,     0,   109,     0,
       0,   153,     0,     0,     0,     0,     0,   348,     0,     0,
       0,     0,   120,   656,     0,     0,  1262,     0,   900,     0,
       0,     0,     0,  1268,     0,   626,     0,     0,   273,     0,
     348,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,   115,     0,   288,   344,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   468,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,   275,     0,   120,     0,     0,     0,     0,     0,   120,
       0,     0,   120,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   900,     0,
       0,     0,     0,     0,   114,     0,     0,   115,   115,     0,
     348,   264,   120,     0,     0,   900,   900,     0,     0,     0,
       0,     0,     0,   352,   120,     0,     0,   348,   348,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     470,     0,     0,     0,     0,     0,     0,     0,   467,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     348,   120,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   153,     0,     0,     0,     0,     0,     0,     0,
       0,  1416,     0,     0,   120,     0,     0,     0,     0,  1227,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1227,   114,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,  1464,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     628,     0,     0,   275,     0,     0,   625,     0,     0,     0,
       0,     0,     0,     0,     0,   547,     0,   628,   626,     0,
     120,   628,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   900,   344,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   348,   468,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,   900,   900,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   348,   348,   900,   900,     0,
       0,     0,   467,   467,     0,     0,     0,     0,     0,   348,
     348,     0,     0,     0,   348,   348,     0,     0,     0,   628,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   900,   900,   900,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   348,   348,   348,
       0,     0,  1416,  1416,  1416,   153,   549,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   119,     0,
       0,     0,  1646,  1646,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   470,     0,
     120,     0,     0,     0,   120,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,   119,   344,     0,     0,     0,     0,     0,
     352,     0,     0,     0,     0,     0,   468,     0,     0,   275,
       0,   114,     0,     0,     0,     0,     0,   119,     0,   153,
       0,     0,   470,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
     628,   470,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   628,     0,   120,     0,     0,   120,
     120,     0,   120,     0,   119,     0,     0,   628,     0,     0,
     119,   120,     0,   119,   120,   120,   120,     0,     0,     0,
       0,     0,   900,   900,   900,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   348,   348,   348,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,  1789,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   900,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     348,     0,     0,     0,     0,  1806,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,   409,   273,     0,     0,
       0,     0,     0,     0,     0,   470,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1646,
     119,     0,   119,     0,     0,     0,     0,   119,     0,     0,
     344,   109,     0,   153,     0,     0,     0,     0,     0,     0,
       0,     0,   348,     0,   900,     0,   470,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   348,     0,     0,     0,
       0,     0,     0,     0,   124,   119,     0,   124,   352,   352,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   900,
       0,     0,     0,     0,   900,   900,   119,   352,     0,   467,
     467,   348,     0,     0,     0,     0,   348,   348,     0,     0,
       0,   348,   348,     0,     0,     0,     0,     0,     0,     0,
    1896,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,   124,   352,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,  1646,     0,     0,
       0,   119,     0,     0,     0,   352,   124,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   628,     0,     0,   275,     0,   352,     0,
       0,     0,     0,     0,     0,   119,   684,     0,     0,     0,
     409,   690,     0,   124,     0,     0,     0,     0,     0,   124,
     699,   700,   124,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,   409,   409,     0,     0,     0,
       0,     0,     0,     0,     0,   470,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   409,     0,  2019,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     626,     0,     0,     0,     0,     0,     0,     0,   352,     0,
       0,     0,     0,     0,     0,     0,   467,     0,   119,   119,
       0,     0,     0,     0,     0,   352,   352,     0,   348,   124,
       0,   124,     0,     0,     0,  1646,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,  1646,  2019,   119,     0,   119,     0,     0,
       0,     0,     0,     0,   124,   109,   626,     0,   352,     0,
     119,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   124,   120,     0,     0,     0,
       0,     0,     0,  1646,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2125,   120,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     900,   120,     0,     0,     0,     0,     0,   119,     0,     0,
     119,   119,   348,   119,     0,     0,     0,     0,   114,     0,
     124,   120,   119,     0,     0,   119,   119,   119,   213,     0,
       0,     0,     0,     0,   224,   225,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,   628,     0,   295,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   352,   470,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,   409,
     409,   409,   409,   409,   409,   409,   409,   409,   409,   409,
     409,   409,   409,   409,   409,   409,   409,   409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   352,   352,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   352,   352,     0,
       0,     0,   352,   352,     0,     0,     0,   124,   124,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,   120,
     120,   120,   120,   120,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,     0,   352,   352,   352,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,     0,
     124,     0,     0,     0,   124,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,   114,     0,     0,     0,   119,   119,     0,
       0,   598,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,   124,   470,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,   124,
     124,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,   124,   124,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   409,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,   352,   352,   352,     0,     0,     0,     0,     0,
       0,   799,     0,   800,   124,     0,     0,     0,     0,     0,
       0,     0,   816,   817,     0,     1,   409,     0,   146,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   352,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,   275,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,   114,
       0,     0,   202,     0,     0,     0,     0,   119,     0,   120,
     352,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,   352,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   910,
       0,   124,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,   124,     0,   352,
       0,   301,   119,     0,   352,   352,   409,     0,     0,   352,
     352,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   409,     0,   301,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
     409,   409,   550,     0,     0,     0,   409,   409,     0,     0,
       0,     0,   301,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   301,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   582,   586,     0,     0,     0,     0,     0,   593,   594,
       0,     0,     0,     0,     0,     0,     0,     0,   119,   119,
     119,   119,   119,   119,   119,     0,   604,   409,   409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   628,     0,
       0,     0,     0,     0,     0,  1093,   623,     0,   119,   119,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   352,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   120,     0,
       0,     0,     0,     0,     0,   114,   124,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,   716,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,   628,     0,     0,     0,   120,     0,
       0,     0,     0,   124,     0,   119,     0,     0,     0,     0,
       0,     0,  1174,  1175,   757,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,  1233,  1234,  1235,     0,     0,
    1237,     0,     0,   114,     0,     0,     0,     0,     0,     0,
     795,   124,     0,     0,   798,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   820,     0,     0,     0,   821,   822,     0,
       0,   825,     0,     0,     0,     0,     0,     0,     0,     0,
     352,     0,     0,     0,   124,     0,   839,   840,   841,   842,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   871,
       0,     0,  1307,     0,     0,     0,     0,     0,   875,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
     301,     0,     0,     0,     0,     0,     0,     0,  1328,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,   917,     0,     0,     0,     0,     0,     0,   582,   119,
       0,     0,     0,     0,   923,     0,     0,   124,   124,   124,
     124,   124,   124,   124,     0,  1352,     0,     0,     0,     0,
       0,     0,     0,     0,  1356,  1357,  1358,  1359,   940,   945,
     119,     0,  1364,  1365,     0,     0,     0,   124,   124,     0,
       0,   409,  1373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1394,     0,     0,  1397,     0,  1398,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   987,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,  1456,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   168,
       0,     0,     0,     0,     0,     0,     0,  1470,     0,     0,
       0,     0,     0,     0,  1474,     0,  1476,  1478,  1050,     0,
       0,     0,     0,     0,     0,   168,     0,     0,  1487,     0,
    1488,     0,  1489,  1067,  1491,     0,     0,  1068,     0,  1499,
       0,     0,     0,     0,     0,     0,   940,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,  1108,     0,
       0,   168,     0,     0,     0,     0,     0,  1117,     0,     0,
       0,     0,     0,  1120,   168,     0,   168,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,  1544,     0,     0,     0,     0,     0,     0,  1551,  1552,
       0,     0,   409,     0,     0,     0,     0,     0,   372,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,  1575,  1164,   124,     0,     0,     1,     0,  1580,
       0,   119,   372,  1581,     0,     0,     0,     0,   124,     0,
       0,     0,   409,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,   119,
       0,  1600,     0,     0,     0,     0,     0,     0,     0,   124,
     168,     0,     0,     0,   168,   224,     0,   168,   168,     0,
       0,   168,     0,     0,   168,   168,     0,   168,     0,   168,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,  1287,     0,    20,  1682,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,   409,    46,   409,    47,     0,     0,
       0,  1704,     0,     0,   168,     0,     0,   168,     0,  1709,
       0,  1711,   124,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   168,     0,     0,     0,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1335,   168,     0,     0,  1336,
       0,     0,     0,     0,     0,     0,   940,     0,     0,     0,
    1742,  1743,     0,     0,     0,     0,  1349,     0,   409,     0,
       0,     0,     0,  1350,     0,  1748,  1749,     0,  1750,     0,
       0,     0,  1354,     0,  1355,     0,     0,  1754,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1760,  1761,
    1762,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,     0,  1387,
       0,     0,     0,  1388,     0,     0,     0,  1389,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   168,     0,     0,     0,   146,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,   168,     0,     0,     0,     0,     0,  1851,  1852,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1860,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1486,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1511,     0,  1884,  1885,
    1886,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   372,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   166,
       0,     0,   168,   168,   457,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1584,  1955,     0,     0,  1585,     0,   527,     0,  1586,     0,
       0,     0,     0,   527,     0,     0,     0,     0,     0,     0,
       0,     0,  1965,     0,     0,  1966,  1967,     0,     0,     0,
       0,     0,  1969,     0,     0,     0,     0,     0,     0,     0,
       0,   293,     0,     0,     0,     0,  1630,     0,    14,    15,
      16,    17,    18,     0,   299,    20,   300,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -465,  -465,     0,  -465,    46,     0,    47,   527,
    -465,     0,     0,     0,     0,     0,  1692,     0,     0,  1695,
       0,     0,     0,     0,     0,     0,     0,    58,   168,   168,
       0,     0,     0,     0,   168,   359,   638,  1706,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   168,   659,     0,   168,   168,
       0,   168,     0,   168,   168,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   529,   530,     0,
       0,   534,     0,     0,   537,   538,     0,   540,  2082,   541,
    1741,     0,     0,     0,     0,     0,     0,     0,     0,  1746,
       0,     0,     0,  1747,     0,   168,     0,     0,     0,   168,
       0,     0,     0,   168,     0,     0,     0,  1751,  1752,     0,
       0,     0,     0,     0,     0,     0,     0,   527,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   527,   791,  1695,   527,   794,     0,     0,
       0,     0,  2122,     0,   359,     0,     0,     0,   638,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2140,     0,     0,
     620,   621,     0,   168,   168,     0,     0,     0,     0,     0,
       0,     0,  2149,     0,     0,     0,   653,   168,     0,   527,
       0,     0,     0,   527,     0,     0,     0,     0,  2162,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,  1845,  1846,   359,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   251,   252,     0,   253,    46,     0,    47,
       0,   254,     0,     0,    49,    50,    51,    52,    53,    54,
      55,   789,     0,     0,   527,     0,     0,   359,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   935,   359,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   638,     0,   168,   210,
     638,     0,     0,     0,     0,     0,     0,   953,     0,   359,
       0,     0,     0,     0,     0,   268,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   867,     0,     0,     0,     0,     0,     0,   168,
       0,     0,     0,     0,     0,     0,   168,     0,  1940,   168,
    -441,     0,     0,   168,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   210,     0,     0,     0,   323,
       0,     0,     0,  -441,     0,  1695,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1968,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   210,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   477,     0,
     359,   481,     0,     0,  1987,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   527,   527,     0,     0,
       0,     0,     0,     0,     0,     0,   527,  1063,     0,   527,
    1066,  2015,   951,   952,  2016,     0,     0,     0,     0,     0,
       0,   359,     0,     0,   638,   959,   638,   638,     0,     0,
       0,     0,     0,   638,   210,     0,     0,     0,     0,     0,
       0,     0,     0,   359,   359,   168,     0,     0,   268,     0,
       0,     0,     0,   168,   168,     0,     0,     0,     0,     0,
       0,     0,   359,     0,     0,     0,   527,     0,     0,     0,
     527,     0,     0,     0,     0,     0,     0,   527,  1137,   433,
       0,   527,  1141,     0,     0,   527,  1145,     0,   481,     0,
       0,     0,   462,  1149,     0,     0,     0,     0,   210,     0,
       0,     0,     0,     0,     0,   490,     0,   490,     0,     0,
       0,     0,   168,     0,     0,     0,     0,     0,   631,  2102,
     648,   168,     0,     0,   168,     0,   168,   168,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     359,   527,     0,     0,     0,     0,     0,     0,  1056,  1057,
       0,     0,     0,     0,  1061,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   638,   168,   714,     0,  1082,     0,     0,  1085,  1086,
       0,  1089,     0,  1091,  1092,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   599,     0,     0,     0,     0,     0,   210,     0,     0,
     359,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1135,     0,     0,     0,  1139,
       0,     0,     0,  1143,     0,     0,     0,   631,    14,    15,
      16,    17,    18,   815,     0,    20,   168,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -466,  -466,     0,  -466,    46,   527,    47,     0,
    -466,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   638,   638,     0,    58,     0,     0,
       0,   638,     0,  1252,  1253,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1269,     0,     0,
     210,   210,     0,     0,     0,     0,   477,     0,     0,     0,
       0,     0,     0,     0,   168,     0,     0,     0,     0,     0,
       0,     0,     0,   359,     0,     0,     0,     0,   527,  1378,
       0,   527,  1382,     0,     0,   527,  1386,     0,     0,     0,
       0,     0,     0,   168,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   490,     0,     0,
     364,     0,     0,   490,     0,     0,     0,     0,   836,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,   477,
       0,   939,     0,     0,   168,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   631,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   210,     0,  1269,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   714,
       0,     0,   714,   714,     0,   714,     0,     0,     0,     0,
       0,     0,   168,     0,   714,     0,     0,   714,   714,   714,
       0,   359,     0,     0,   916,     0,     0,   638,  1495,  1369,
       0,     0,     0,     0,     0,     0,  1376,     0,     0,  1380,
       0,     0,     0,  1384,     0,     0,     0,     0,     0,     0,
     359,     0,     0,   462,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   947,     0,     0,     0,
       0,     0,     0,   477,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   527,  1548,     0,   210,     0,   168,
     168,     0,     0,   527,  1557,     0,   638,   372,     0,     0,
       0,   168,     0,     0,   477,     0,     0,   359,   359,     0,
       0,   981,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   477,   477,     0,     0,
       0,     0,     0,     0,     0,   836,  1001,     0,     0,  1003,
       0,  1005,     0,     0,     0,   477,     0,  1014,     0,  1019,
    1014,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1493,     0,     0,     0,     0,
       0,     0,     0,  1502,  1503,     0,     0,  1047,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1049,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1058,     0,     0,     0,     0,     0,     0,  1979,   168,
       0,     0,     0,     0,     0,   462,     0,     0,  1047,     0,
       0,     0,     0,   477,     0,     0,     0,     0,     0,     0,
     210,     0,  1546,     0,     0,     0,     0,     0,     0,   359,
       0,  1555,     0,   815,  1559,  1111,  1562,  1563,   490,     0,
       0,     0,     0,     0,     0,     0,   381,     0,     0,   382,
       0,   383,     0,   384,     0,     0,   638,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,     0,   168,     0,     0,     0,     0,     0,
       0,  1150,  1591,   364,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,   387,
       0,   388,     0,   389,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   390,   391,   378,     0,   392,   393,   394,
       0,   395,   396,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,   433,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1259,  1261,   397,
     527,     0,    76,   398,     0,   462,  1689,     0,     0,   399,
      78,    79,   400,   401,   402,   403,   527,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1014,   168,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1047,     0,     0,
       0,     0,     0,     0,     0,  1300,     0,     0,     0,     0,
       0,     0,  1014,     0,     0,     0,   477,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,  1559,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1267,     0,     0,     0,   490,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,   714,
       0,     0,     0,  1756,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   359,   359,     0,     0,   381,     0,
       0,   382,     0,   383,     0,   384,     0,     0,     0,     0,
       0,     0,     0,     0,   527,   527,     0,     0,     0,     0,
       0,    58,   385,   268,     0,   490,     0,  1368,     0,  1371,
     527,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   210,     0,     0,     0,     0,     0,
     386,   387,     0,   388,   631,   389,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   390,   391,   378,     0,   392,
     393,   394,     0,   395,   396,     0,     0,     0,     0,     0,
       0,    73,  1843,   364,     0,     0,     0,   714,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   397,     0,     0,    76,   398,     0,     0,  1448,  1448,
       0,   399,   460,    79,   400,   401,   402,   403,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   527,
       0,     0,     0,     0,     0,     0,     0,   527,     0,     0,
     477,   477,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1900,
    1901,     0,  1490,     0,     0,     0,     0,     0,  1500,     0,
       0,  1905,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   462,     0,     0,
     714,   714,   714,     0,     0,   714,   714,     0,     0,     0,
       0,   359,   481,     0,   490,     0,   527,  2047,     0,     0,
     527,     0,     0,     0,     0,     0,     0,     0,     0,  1014,
       0,     0,   836,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     268,   527,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,  1583,     0,     0,     0,     0,  1977,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1593,  1594,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   527,   527,     0,     0,
       0,     0,     0,     0,  1014,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1497,     0,   490,     0,     0,   836,     0,     0,    14,    15,
      16,    17,    18,     0,  2045,     0,     0,     0,   527,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   381,     0,  1001,   382,     0,   383,
       0,   384,     0,     0,     0,     0,  1707,  1708,     0,   210,
       0,     0,     0,     0,     0,     0,   490,    58,   385,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   490,     0,   836,     0,     0,     0,
       0,     0,   268,     0,     0,     0,   386,   387,     0,   388,
       0,   389,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   390,   391,   378,     0,   392,   393,   394,     0,   395,
     396,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,     0,     0,     0,   397,     0,     0,
      76,   398,     0,     0,     0,     0,     0,   399,  1498,    79,
     400,   401,   402,   403,     0,     0,     0,     0,     0,     0,
       0,     0,   714,   433,     0,     0,     0,     0,  1788,     0,
       0,     0,     0,     0,     0,  2163,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   477,   477,     0,
       0,     0,  1432,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   381,     0,     0,   382,  1834,   383,     0,
     384,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   268,     0,     0,  1180,     0,   385,    -2,     0,
    1182,  -242,  -242,  1183,  1184,  1185,  1186,  1187,  1188,  1189,
    1190,  1191,  1192,  1193,  1194,  -336,     0,  1195,  1196,  1197,
    1198,  1199,     0,  1200,  1862,   386,   387,  1864,   484,     0,
     389,  1201,  1202,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,  1203,   392,   393,   394,     0,   395,   396,
       0,     0,     0,     0,     0,     0,    73,  2163,     0,     0,
       0,     0,     0,     0,  1891,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1432,  -242,   397,     0,     0,    76,
     398,     0,     0,     0,   297,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,     0,
       0,  -184,     0,     0,     0,   381,     0,     0,   382,     0,
     383,     0,   384,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   714,  1180,     0,   385,
      -2,     0,  1182,  -243,  -243,  1183,  1184,  1185,  1186,  1187,
    1188,  1189,  1190,  1191,  1192,  1193,  1194,  -336,     0,  1195,
    1196,  1197,  1198,  1199,   477,  1200,     0,   386,   387,     0,
     484,     0,   389,  1201,  1202,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,  1203,   392,   393,   394,  1841,
     395,   396,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,  1432,     0,     0,     0,
       0,     0,   714,     0,     0,   481,     0,  -243,   397,     0,
       0,    76,   398,     0,     0,     0,   297,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,   381,     0,     0,
     382,     0,   383,  -184,   384,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1014,  1180,
       0,   385,    -2,     0,  1182,     0,     0,  1183,  1184,  1185,
    1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  -336,
       0,  1195,  1196,  1197,  1198,  1199,     0,  1200,     0,   386,
     387,     0,   484,     0,   389,  1201,  1202,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,  1203,   392,   393,
     394,     0,   395,   396,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     397,     0,     0,    76,   398,     0,     0,     0,   297,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,     0,     0,  -184,     4,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1179,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   381,     0,    46,   382,    47,   383,     0,   384,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1180,    58,  1181,    -2,     0,  1182,
       0,     0,  1183,  1184,  1185,  1186,  1187,  1188,  1189,  1190,
    1191,  1192,  1193,  1194,  -336,     0,  1195,  1196,  1197,  1198,
    1199,     0,  1200,     0,   386,   387,    61,   484,     0,   389,
    1201,  1202,    65,    66,    67,    68,    69,    70,    71,   390,
     391,   378,  1203,   392,   393,   394,     0,   395,   396,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,   397,     0,     0,    76,   429,
       0,     0,     0,   297,     0,   399,    78,    79,   400,   401,
     402,   403,     0,     0,     0,     0,     0,     0,     0,     0,
    -184,     4,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1179,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   381,     0,    46,
     382,    47,   383,     0,   384,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,  1180,
      58,  1181,    -2,     0,  1182,     0,     0,  1183,  1184,  1185,
    1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  -336,
       0,  1195,  1196,  1197,  1198,  1199,     0,  1200,     0,   386,
     387,    61,   484,     0,   389,  1201,  1202,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,  1203,   392,   393,
     394,     0,   395,   396,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     397,     0,     0,    76,   429,     0,     0,     0,   297,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,     0,     0,  -184,     4,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   381,     0,    46,   382,    47,   383,     0,   384,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   386,   387,    61,   388,     0,   389,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   390,
     391,   378,     0,   392,   393,   394,     0,   395,   396,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1648,
    1649,  1650,     0,     0,     0,   397,  1651,  1652,    76,   429,
       0,     0,     0,     0,     0,   399,    78,    79,   400,   401,
     402,   403,     0,     0,     0,     0,     0,     0,     0,     0,
    1653,     4,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   381,     0,    46,
     382,    47,   383,     0,   384,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   385,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
     387,    61,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,     0,   395,   396,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1648,  1649,  1650,     0,     0,     0,
     397,  1651,     0,    76,   429,     0,     0,     0,     0,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,     0,     0,  1653,     4,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   381,     0,    46,   382,    47,   383,     0,   384,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   386,   387,    61,   388,     0,   389,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   390,
     391,   378,     0,   392,   393,   394,     0,   395,   396,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   397,     0,  1639,    76,   429,
       0,     0,     0,     0,     0,   399,    78,    79,   400,   401,
     402,   403,     4,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   381,     0,
      46,   382,    47,   383,     0,   384,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   385,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,   387,    61,   388,     0,   389,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   390,   391,   378,     0,   392,
     393,   394,     0,   395,   396,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   397,     0,     0,    76,   429,     0,     0,     0,     0,
       0,   399,    78,    79,   400,   401,   402,   403,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   381,     0,    46,   382,    47,   383,     0,
     384,   339,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   385,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,     0,   395,   396,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   397,     0,     0,    76,
     459,     0,     0,     0,     0,     0,   399,   460,    79,   400,
     401,   402,   403,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   381,     0,
      46,   382,    47,   383,     0,   384,   339,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   385,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,   387,     0,   388,     0,   389,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   390,   391,   378,     0,   392,
     393,   394,     0,   395,   396,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   397,     0,     0,    76,  1256,     0,     0,     0,     0,
       0,   399,  1257,    79,   400,   401,   402,   403,   182,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   381,     0,    46,   382,    47,   383,     0,
     384,   339,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   385,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,     0,   395,   396,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   397,     0,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   381,     0,
      46,   382,    47,   383,     0,   384,   339,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   385,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,   387,     0,   388,     0,   389,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   390,   391,   378,     0,   392,
     393,   394,     0,   395,   396,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   397,     0,     0,    76,   459,     0,     0,     0,     0,
       0,   399,    78,    79,   400,   401,   402,   403,  1986,     0,
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
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,  2014,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,     0,    -2,     0,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
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
       0,     0,    60,     0,     0,     0,    61,    62,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,    75,     0,    76,    77,
       0,     0,     0,     0,     0,     0,    78,    79,   256,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -466,  -466,     0,  -466,    46,     0,    47,     0,
    -466,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,   257,     0,     0,     0,  -794,     0,     0,    78,    79,
       4,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
       0,     0,     0,     0,  -389,  -389,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -389,     0,
       0,     0,    76,    77,     0,     0,     0,     0,     0,     0,
      78,    79,     4,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,     0,     0,     0,     0,  -390,  -390,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -390,     0,     0,     0,    76,    77,     0,     0,     0,     0,
       0,     0,    78,    79,   256,   182,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -466,  -466,
       0,  -466,    46,     0,    47,     0,  -466,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,   257,     0,  1407,
       0,  1408,     0,     0,    78,    79,  1409,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1410,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1411,     0,     0,
       0,    76,   977,     0,  1407,     0,  1408,     0,     0,    78,
      79,  1409,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1603,     0,     0,     0,    76,   977,     0,  1407,
       0,  1408,     0,     0,    78,    79,  1409,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1410,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1604,     0,     0,
       0,    76,   977,     0,  1407,     0,  1408,     0,     0,    78,
      79,  1409,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1605,     0,     0,     0,    76,   977,     0,     0,
       0,     0,     0,     0,    78,    79,   256,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -466,  -466,     0,  -466,    46,     0,    47,   256,  -466,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -466,  -466,     0,  -466,    46,     0,    47,     0,  -466,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   257,
       0,    63,    64,     0,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     321,     0,     0,     0,     0,     0,     0,    78,    79,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -466,  -466,     0,  -466,    46,     0,    47,     0,
    -466,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,   257,     0,     0,     0,  -798,     0,     0,    78,    79,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -466,  -466,     0,  -466,    46,     0,    47,
       0,  -466,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,    75,
       0,    76,   257,     0,     0,     0,     0,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   339,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,    73,
       0,  1102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -658,    76,   341,     0,     0,     0,    63,    64,     0,
      78,    79,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    76,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   339,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   339,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   340,    76,   341,     0,     0,     0,    63,    64,
       0,    78,    79,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    76,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   339,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,  1880,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   341,     0,     0,     0,     0,
       0,     0,    78,    79,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   339,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1882,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   341,     0,     0,     0,
       0,     0,     0,    78,    79,   182,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   339,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   321,     0,     0,
       0,     0,     0,     0,    78,    79,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   339,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   341,     0,
       0,     0,     0,     0,     0,    78,    79,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -466,  -466,     0,  -466,    46,     0,    47,     0,  -466,     0,
     182,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,     0,     0,  1432,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   381,     0,     0,   382,
       0,   383,     0,   384,     0,     0,     0,     0,    76,   257,
     183,     0,     0,   184,   185,     0,    78,    79,  1180,     0,
     385,     0,     0,  1182,  1907,  1908,  1183,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  -336,     0,
    1195,  1196,  1197,  1198,  1199,     0,  1200,     0,   386,   387,
       0,   484,     0,   389,  1201,  1202,    65,    66,    67,    68,
      69,    70,    71,   390,   391,   378,  1203,   392,   393,   394,
    1432,   395,   396,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   397,
       0,   381,    76,   398,   382,     0,   383,   297,   384,   399,
      78,    79,   400,   401,   402,   403,     0,     0,     0,     0,
       0,     0,     0,  1180,  -184,   385,     0,     0,  1182,     0,
       0,  1183,  1184,  1185,  1186,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  -336,     0,  1195,  1196,  1197,  1198,  1199,
       0,  1200,     0,   386,   387,     0,   484,     0,   389,  1201,
    1202,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,  1203,   392,   393,   394,     0,   395,   396,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   397,     0,     0,    76,   398,     0,
       0,     0,   297,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
      14,    15,    16,    17,    18,    19,   701,    20,   702,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   381,     0,    46,   382,
      47,   383,     0,   384,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   703,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,   387,
       0,   388,     0,   389,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   390,   391,   378,     0,   392,   393,   394,
       0,   395,   396,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   397,
       0,     0,    76,   704,     0,     0,     0,   297,     0,   399,
      78,    79,   705,   706,   402,   403,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   381,     0,    46,   382,    47,   383,     0,   384,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   386,   387,     0,   388,     0,   389,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   390,
     391,   378,     0,   392,   393,   394,     0,   395,   396,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   397,     0,   428,    76,   429,
       0,     0,     0,     0,     0,   399,    78,    79,   400,   401,
     402,   403,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   381,     0,
      46,   382,    47,   383,     0,   384,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   385,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,   387,     0,   388,     0,   389,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   390,   391,   378,     0,   392,
     393,   394,     0,   395,   396,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   397,     0,     0,    76,   704,     0,     0,     0,   297,
       0,   399,    78,    79,   400,   401,   402,   403,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   381,     0,    46,   382,    47,   383,
       0,   384,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   385,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   386,   387,     0,   388,
       0,   389,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   390,   391,   378,     0,   392,   393,   394,     0,   395,
     396,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   397,     0,     0,
      76,   429,     0,     0,     0,     0,     0,   399,    78,    79,
     400,   401,   402,   403,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     381,     0,    46,   382,    47,   383,     0,   384,   339,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   385,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   386,   387,     0,   388,     0,   389,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   390,   391,   378,
       0,   392,   393,   394,     0,   395,   396,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   397,     0,     0,    76,   459,     0,     0,
       0,     0,     0,   399,    78,    79,   400,   401,   402,   403,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   381,     0,    46,   382,
      47,   383,     0,   384,   339,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,   387,
       0,   388,     0,   389,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   390,   391,   378,     0,   392,   393,   394,
       0,   395,   396,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   397,
       0,     0,    76,   398,     0,     0,     0,     0,     0,   399,
      78,    79,   400,   401,   402,   403,   182,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   692,     0,     0,   693,
     694,     0,   587,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,   -16,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   256,   182,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    63,    64,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -466,  -466,     0,  -466,    46,
       0,    47,     0,  -466,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
      58,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    63,    64,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    75,     0,    76,    77,     0,     0,     0,  -796,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   147,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    77,     0,
       0,     0,     0,     0,     0,    78,    79,   182,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     339,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   899,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -671,    76,   182,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   339,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1797,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      76,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -466,  -466,     0,  -466,
      46,     0,    47,     0,  -466,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   147,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    75,     0,    76,   321,     0,     0,     0,     0,
       0,     0,    78,    79,   182,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   339,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,    76,     0,    46,     0,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1515,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   983,    76,   977,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   977,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   304,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,   339,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   455,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   341,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   339,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,    63,    64,     0,   339,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   304,     0,     0,     0,    63,    64,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   455,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     339,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
      63,    64,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   321,
       0,     0,     0,    63,    64,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   977,     0,     0,     0,     0,     0,     0,    78,
      79,   182,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -466,  -466,     0,  -466,    46,     0,
      47,     0,  -466,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,    63,    64,     0,   339,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,    76,     0,    46,     0,    47,    63,    64,     0,
     339,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   977,     0,     0,     0,
      63,    64,     0,    78,    79,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,     0,
       0,    14,    15,    16,    17,    18,    78,    79,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -466,  -466,     0,  -466,    46,
       0,    47,     0,  -466,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -466,  -466,     0,  -466,
      46,     0,    47,     0,  -466,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   321,     0,    63,    64,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,     0,     0,     0,     0,     0,
       0,     0,    78,    79,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   381,     0,    46,   382,    47,   383,     0,
     384,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,     0,   395,   396,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   397,     0,     0,    76,
     398,     0,     0,     0,     0,     0,   399,   460,    79,   400,
     401,   402,   403,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   381,     0,    46,   382,    47,   383,     0,   384,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   386,   387,     0,   388,     0,   389,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   390,
     391,   378,     0,   392,   393,   394,     0,   395,   396,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   397,     0,     0,    76,   398,
       0,     0,     0,     0,     0,   399,    78,    79,   400,   401,
     402,   403,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   147,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -466,
    -466,     0,  -466,    46,    76,    47,     0,  -466,     0,     0,
       0,   381,     0,     0,   382,     0,   383,     0,   384,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,     0,   395,   396,   381,     0,
       0,   382,     0,   383,    73,   384,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,  1648,  1649,
    1650,     0,   385,     0,   397,  1821,     0,    76,   398,     0,
       0,     0,     0,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,   387,     0,   388,     0,   389,  1922,    64,    65,    66,
      67,    68,    69,    70,    71,   390,   391,   378,     0,   392,
     393,   394,     0,   395,   396,   381,     0,     0,   382,     0,
     383,    73,   384,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1648,  1649,  1650,     0,   385,
       0,   397,  1923,     0,    76,   398,     0,     0,     0,     0,
       0,   399,    78,    79,   400,   401,   402,   403,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     484,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,    75,
       0,   485,   486,     0,     0,     0,   487,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,  1303,     0,    76,   398,     0,     0,     0,
    1304,     0,   399,    78,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,   381,   395,   396,
     382,     0,   383,     0,   384,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,   397,     0,     0,    76,
     398,     0,     0,     0,   487,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,   386,
     387,     0,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,   381,   395,   396,   382,     0,   383,     0,   384,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     397,   835,     0,    76,   398,     0,     0,     0,     0,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,   381,   395,   396,   382,     0,
     383,     0,   384,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   397,     0,     0,    76,   398,     0,
       0,     0,   297,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     388,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,  1010,
       0,    76,   398,     0,     0,     0,     0,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,     0,     0,    76,   398,     0,     0,  1041,
       0,     0,   399,    78,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,   381,   395,   396,
     382,     0,   383,     0,   384,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,   397,  1370,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,   386,
     387,     0,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,   381,   395,   396,   382,     0,   383,     0,   384,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     397,     0,     0,    76,   398,     0,     0,     0,  1442,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,   381,   395,   396,   382,     0,
     383,     0,   384,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   397,     0,     0,    76,   398,     0,
       0,     0,  1506,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     388,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,     0,
    1913,    76,   398,     0,     0,     0,     0,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,  1918,     0,    76,   398,     0,     0,     0,
       0,     0,   399,    78,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,   381,   395,   396,
     382,     0,   383,     0,   384,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,   397,  1927,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,   386,
     387,     0,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,   381,   395,   396,   382,     0,   383,     0,   384,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     397,  2008,     0,    76,   398,     0,     0,     0,     0,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,   381,   395,   396,   382,     0,
     383,     0,   384,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   397,  2010,     0,    76,   398,     0,
       0,     0,     0,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     388,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,  2063,
       0,    76,   398,     0,     0,     0,     0,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,  2065,     0,    76,   398,     0,     0,     0,
       0,     0,   399,    78,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,   381,   395,   396,
     382,     0,   383,     0,   384,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,   397,  2067,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,   386,
     387,     0,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,   381,   395,   396,   382,     0,   383,     0,   384,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     397,  2070,     0,    76,   398,     0,     0,     0,     0,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,   381,   395,   396,   382,     0,
     383,     0,   384,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   397,  2072,     0,    76,   398,     0,
       0,     0,     0,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     388,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,  2115,
       0,    76,   398,     0,     0,     0,     0,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,  2117,     0,    76,   398,     0,     0,     0,
       0,     0,   399,    78,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,   381,   395,   396,
     382,     0,   383,     0,   384,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,   397,  2119,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,   386,
     387,     0,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,   381,   395,   396,   382,     0,   383,     0,   384,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     397,  2143,     0,    76,   398,     0,     0,     0,     0,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,   381,   395,   396,   382,     0,
     383,     0,   384,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   397,  2145,     0,    76,   398,     0,
       0,     0,     0,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     388,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,  2147,
       0,    76,   398,     0,     0,     0,     0,     0,   399,    78,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,     0,     0,    76,   398,     0,     0,     0,
       0,     0,   399,    78,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,   381,   395,   396,
     382,     0,   383,     0,   384,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,   683,     0,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,     0,     0,     0,     0,     0,     0,   386,
     387,     0,   388,     0,   389,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   390,   391,   378,     0,   392,   393,
     394,   381,   395,   396,   382,     0,   383,     0,   384,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,     0,     0,
     689,     0,     0,    76,   398,     0,     0,     0,     0,     0,
     399,    78,    79,   400,   401,   402,   403,     0,     0,     0,
       0,     0,     0,   386,   387,     0,   388,     0,   389,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   390,   391,
     378,     0,   392,   393,   394,   381,   395,   396,   382,     0,
     383,     0,   384,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,     0,     0,   698,     0,     0,    76,   398,     0,
       0,     0,     0,     0,   399,    78,    79,   400,   401,   402,
     403,     0,     0,     0,     0,     0,     0,   386,   387,     0,
     388,     0,   389,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   390,   391,   378,     0,   392,   393,   394,   381,
     395,   396,   382,     0,   383,     0,   384,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,     0,     0,   397,     0,
       0,    76,   398,     0,     0,     0,     0,     0,   399,   915,
      79,   400,   401,   402,   403,     0,     0,     0,     0,     0,
       0,   386,   387,     0,   388,     0,   389,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   390,   391,   378,     0,
     392,   393,   394,   381,   395,   396,   382,     0,   383,     0,
     384,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,   397,     0,     0,    76,   398,     0,     0,     0,
       0,     0,   399,   460,    79,   400,   401,   402,   403,     0,
       0,     0,     0,     0,     0,   386,   387,     0,   388,     0,
     389,  2003,    64,    65,    66,    67,    68,    69,    70,    71,
     390,   391,   378,     0,   392,   393,   394,     0,   395,   396,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   397,     0,     0,    76,
     398,     0,     0,     0,     0,     0,   399,    78,    79,   400,
     401,   402,   403,   182,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   692,     0,     0,   693,   694
};

static const yytype_int16 yycheck[] =
{
       1,   397,   255,     4,   165,     1,    74,   730,   270,   180,
     165,     1,   176,    74,     4,   229,   250,    74,   301,    74,
     371,   924,   715,   181,   908,   646,    76,   229,  1434,  1435,
      83,   487,  1164,     1,  1189,    99,    59,  1840,  1771,   217,
     931,   821,   633,  1771,   229,   229,   344,   801,   604,   555,
     544,   545,  1771,   399,  1434,    56,    57,  1173,    59,   839,
     566,   774,   229,    59,   633,   708,     1,   639,   633,    59,
      98,   165,   908,    74,   637,    75,   967,     1,   140,  1911,
       4,   152,    83,   229,    75,   181,   804,    83,   152,   229,
      91,    59,   810,  1354,  1355,    96,   149,   892,    99,   313,
      96,   607,   103,    99,     4,   176,   232,   103,   234,    99,
    1907,   313,     1,   103,   229,   241,    72,    72,   229,  1658,
    1772,   803,   166,   146,    59,   803,   921,   801,   313,   313,
     191,   801,   801,  1016,   191,    59,   191,   917,    96,   201,
     141,     1,   229,   144,    74,   146,   313,    72,   336,   177,
     146,   152,    89,   133,   132,     1,   146,     1,   159,     1,
       4,   229,  1045,     0,    99,   166,     1,   313,   229,   467,
      89,    10,   229,   313,   229,    72,   670,   177,   146,   103,
      89,    74,   308,   309,   134,  1161,   177,   375,     0,   190,
     191,     0,  1168,   359,   190,   175,   152,   363,   313,    59,
     156,   156,   313,   103,   205,   258,  1173,     4,   146,   205,
     801,   146,    84,    59,   215,    59,    74,    59,   168,   220,
     202,   158,   146,   160,    59,  1108,   313,   152,   229,   230,
     168,   156,   801,   805,   230,  1173,   801,   809,  1654,    83,
     803,   160,   292,   221,  1896,   313,   818,   819,   342,   250,
     973,   160,   313,   118,  2051,   152,   313,   258,   313,    56,
      57,   191,   258,  1239,   820,   821,   528,   268,   269,   322,
     159,   272,   230,  2105,   536,  1681,   272,  1816,   279,  1002,
     152,    89,   272,   839,   152,     1,   146,   781,  1404,  1405,
    1406,   505,   293,   294,    91,   296,   140,  1181,   191,   229,
     146,  1681,   146,   505,   146,   149,  1312,   152,   542,  2141,
     593,   146,   313,   314,   548,   493,   667,  1071,   314,    20,
     505,   505,   323,   162,   302,   155,  1210,   625,   167,   330,
     331,  2134,   728,   191,   335,   118,   229,   272,   505,   685,
    1120,  1054,   152,    59,   141,  1181,   176,   144,   991,    96,
    2083,   649,   160,    96,  1072,  2083,   314,   201,   656,   505,
     342,   917,   159,   345,  2083,   505,  2018,   461,   397,   166,
    1165,   229,   373,   156,  1210,   376,   377,   359,   431,  1511,
    1271,   363,   342,   889,   152,   948,   600,   103,  1279,   239,
     505,   820,   821,   152,   505,   457,  1078,  1071,   600,   359,
    1078,  1071,  1071,   363,   123,  1821,  1822,   160,     1,   344,
     839,   152,   256,  1819,   258,   600,   600,   267,   505,   397,
     157,   465,   160,   220,   177,  1401,    61,    62,   278,   167,
     146,   987,  2084,   600,   160,   485,   155,  1404,  1405,  1406,
     441,   301,   238,   604,   505,   441,   158,    72,   505,   245,
     505,   177,   107,   108,   600,   301,   622,   301,    75,   301,
     600,   158,   202,   464,   465,   177,  1404,  1405,  1406,  2054,
     266,   268,   269,  2125,    91,   476,   477,   177,   322,   461,
    1071,   277,   279,   230,   485,   600,   487,   230,   917,   600,
    1751,  1752,   109,   659,    75,  2080,   293,   294,  1653,   296,
     155,   461,  1071,  1658,   505,   158,  1071,  1923,  1924,    90,
     604,    72,   156,   600,    72,  1078,   517,   161,    10,   134,
      72,   517,  2107,  1095,   177,   160,   323,   152,  1534,  1535,
    1536,   156,   467,   330,   331,    77,    78,    72,   335,   600,
     134,   542,   177,   600,    72,   600,  1264,   548,   163,   164,
    1434,  1435,   646,   531,    58,   279,   177,    61,    62,   517,
      64,   539,    77,    78,  1120,   158,   159,   314,   550,   149,
     294,   314,   154,   155,   168,   505,   373,   621,   556,   376,
     160,  1987,   152,   606,   755,   301,   587,   431,   589,   567,
     160,   152,  1048,   589,   152,   156,   176,   167,   156,   600,
     152,   602,   900,    72,   156,   606,   602,   841,    72,  2015,
     606,   158,   505,   457,    72,   616,   606,   152,   160,   620,
     621,   156,   604,     3,   152,  1343,   158,   152,   156,  1322,
     177,   589,    72,   867,   158,   160,   168,  1258,   606,   937,
     622,   623,    72,    69,   602,   160,  1529,   505,   152,  2055,
     828,     3,   653,   177,   683,   590,   814,   686,   687,   151,
     689,  1816,   622,   156,   646,   666,   155,   464,   161,   698,
     162,   606,   701,   702,   703,   167,   158,   659,   154,   476,
     477,   158,   606,   152,   158,   158,   646,   156,   152,   165,
     166,  1120,   156,    72,   152,   177,    72,   868,   156,   659,
     177,  1424,   152,   177,    72,   683,   756,  1968,   160,    72,
    1613,   689,   152,   158,   649,   565,   156,   160,   814,   720,
     698,   722,   152,   724,   167,   158,   156,   728,   151,   825,
     731,  1287,   177,   593,   158,   158,   532,   158,    72,   717,
     158,   128,   129,   587,   177,    72,   606,   593,   158,   593,
     168,   593,  1445,   177,  1248,   756,   177,   158,   168,   803,
     606,   557,   606,   623,   606,   160,  1921,   168,   564,   935,
     517,   606,   568,   152,   517,   757,   176,   156,  1933,    13,
      14,    15,    16,    17,   152,   172,   173,   160,   156,   152,
     587,   158,    83,   156,   158,   118,   153,  1681,  1354,   152,
     801,   168,   803,   160,   168,   174,   159,   154,  1699,  1700,
     550,   158,   132,   151,   815,   152,   798,   154,   152,   616,
     158,   822,   156,   620,   621,   152,   987,   828,   152,   156,
     831,    58,   152,  1526,    61,    62,   156,    64,    72,   840,
     841,   842,   589,   163,   164,  1107,   589,  2002,  1304,    47,
      48,  1734,    50,  1736,   152,   602,   653,    55,   149,   602,
     163,    13,    14,    15,    16,    17,   867,   170,   171,   666,
      13,    14,    15,    16,    17,   591,   154,   593,   152,   132,
     154,   159,   156,   623,    13,    14,    15,    16,    17,   154,
     606,   154,   154,   987,   159,   158,   874,   159,   189,   152,
     134,  1252,  1253,   156,   154,   906,   907,   908,   886,   159,
     163,   164,   890,   154,  1606,   959,   894,   158,   908,  1611,
      72,   152,   152,   720,   154,   722,   156,   724,   152,    72,
    1486,   728,   156,   923,   731,  1819,   373,  1115,   798,   376,
     908,   154,   158,    72,   160,  1041,   146,   147,   148,   154,
    1513,   155,   156,   935,    22,   159,   938,   132,   959,   756,
     735,   736,   737,   132,  1262,   900,  1857,   258,   168,   152,
     152,   972,   154,   908,   156,   935,   154,   152,   913,   152,
     132,   156,   154,   152,   908,   152,  1442,   156,   163,   164,
      83,   134,   158,   154,   163,   164,    18,   158,   154,   923,
     152,   176,   937,    96,   156,   987,    99,  1008,  1242,   154,
     103,   163,   164,  1409,  1267,  1016,   152,   757,   815,   158,
    1146,  1117,   152,   923,   154,   822,   156,   158,   154,   385,
     158,   322,   158,  1077,  1078,   742,   743,   744,   745,    61,
      62,    63,    64,   840,  1045,   842,   158,  1048,   908,   154,
    1506,   342,   154,   158,   410,   411,   158,   154,   798,   154,
      99,   158,   908,   158,   908,   152,   908,  1958,  1524,   156,
    1071,   107,   108,   908,   152,   431,  1077,  1078,   156,   923,
     820,   821,   104,   107,   108,  1249,  1250,   109,   110,   111,
     112,   113,   114,   115,   116,   891,   152,   190,   152,   839,
     156,   152,   154,  1987,     3,   461,   158,  1108,    89,   906,
     907,   908,   205,    78,    13,    14,    15,    16,    17,  1277,
    1173,   154,   154,   154,   156,  1286,  1287,   158,  1390,   152,
     160,  2015,  2088,   156,   156,   160,  2092,   230,   151,   104,
     431,   160,   107,   108,   109,   110,   111,   112,   113,   114,
     115,  1434,  1503,     4,     5,     6,     7,     8,     9,    10,
      11,    12,   959,   157,  1258,   258,   146,   147,   148,   167,
     461,  2055,   154,    72,  1164,   176,   158,   917,   158,   272,
     154,  1277,  1183,   154,   158,  1186,  1187,  1188,   168,  1167,
     154,  1181,   908,  1287,   158,  1751,   118,   177,   152,    62,
    1355,   158,  1180,   154,  1362,  1363,   154,   158,   152,  1210,
     158,   152,   177,  1181,    65,  1216,   169,   154,   154,  1197,
    1210,   158,   158,  1224,   132,  1948,  1227,  1228,   152,  1325,
    1231,  1227,  1228,   165,   166,   154,    99,  1227,  1228,   158,
    1164,  1242,  1210,   164,   152,   162,  1181,   110,   156,   112,
     132,   114,   155,   544,   545,   163,   164,  1181,   174,   154,
     154,   154,  1291,   158,  1164,   158,  1362,  1363,    62,     3,
    1603,  1604,  1605,   154,  1275,  1210,  1258,   158,   154,    13,
      14,    15,    16,    17,   154,   154,  1210,  1288,   158,   158,
     153,   156,  1227,   156,   157,   154,   126,   127,  1258,   158,
     146,   147,   148,  1304,  1228,  1287,  1170,  1171,  1172,   154,
     104,  1312,   158,   158,   108,   130,   131,   111,   154,   113,
    1164,  1181,   168,   157,   158,  1486,   154,  1262,  1228,  1173,
     154,   177,   146,   147,   148,  1181,   154,  1181,    72,  1181,
     157,   158,   205,  1344,   163,   164,  1181,   157,   441,   134,
    1210,  1404,  1405,  1406,   168,   646,  1409,  1410,  1624,  1625,
    1626,   134,  1713,   177,  1210,   158,  1210,   159,  1210,    13,
      14,    15,    16,    17,    18,  1210,   157,   158,   734,   670,
    1120,   111,   112,   113,   114,   115,  1183,   157,   158,  1186,
    1187,  1188,  1486,   157,   158,   157,   158,   152,  1681,   158,
     159,   157,   158,  1617,   157,   158,   157,   158,   154,   272,
     159,   274,   275,  1210,  1415,  1617,   154,  1407,  1419,  1216,
     157,  1422,   157,   158,   517,   157,   158,  1224,   157,   158,
     157,   158,  1617,  1617,  1231,    13,    14,    15,    16,    17,
      18,  1442,   154,   306,  1434,  1435,   154,  1425,  1426,   312,
    1617,    13,    14,    15,    16,    17,    18,  1415,   157,   158,
     154,  1462,   154,  1464,   154,  1181,   152,   261,  1464,   157,
     158,  1617,   157,   158,  1464,   157,   158,  1617,  1275,   154,
     665,   344,   154,  1407,   160,  1463,   156,   350,    70,   352,
     781,  1288,   157,   158,  1210,   157,   158,   158,   159,  1434,
    1435,   160,  1617,   176,  1486,  1506,  1617,  1407,   104,  1680,
    1434,  1435,  1228,   109,   110,   111,   112,   113,   114,   115,
     160,  1511,   160,  1524,   160,   388,   157,   158,  1529,  1464,
    1617,  1692,   157,  1534,  1535,  1536,   104,  1692,   152,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    78,  1617,
     157,   158,   346,   347,   157,   349,    18,   351,    77,    78,
    1404,  1405,  1406,  1407,  1408,  1409,  1410,   158,   159,  1313,
    1314,   176,  1786,   158,  1434,  1435,   738,   739,   441,   160,
     740,   741,   746,   747,  1786,  1625,  1626,  1511,  1434,  1435,
    1434,  1435,  1434,  1435,   388,  1249,  1250,  1752,  1692,  1434,
    1435,  1786,  1786,   152,   467,  1606,   469,   470,   177,   177,
    1611,  1511,  1846,   154,  1354,  1355,  1617,   154,  1619,  1786,
     160,   484,  1419,  1504,  1505,  1422,  1627,   983,   160,   177,
     157,   157,   988,   924,    18,   151,   154,   154,  1639,   154,
    1786,   826,   154,   999,   154,   154,  1786,   154,  1606,   154,
     154,  1652,    22,  1611,   517,   154,  1646,  1647,   154,   151,
     151,  1619,   160,   160,  1835,  1462,   160,  1511,  1415,   154,
      70,  1786,  1415,   154,   177,  1786,   176,   154,   154,   542,
     151,    13,   160,   176,   547,   160,   549,   154,  1689,   154,
     484,  1681,   158,   154,  1672,   158,   154,  1726,   158,  1786,
     154,   154,   154,   154,   154,  1695,   154,   570,   157,   572,
     573,  1646,    13,    14,    15,    16,    17,    18,  1434,  1435,
     157,  2019,   154,  1647,   154,   154,   154,   590,   154,   154,
     154,   154,   154,  1734,   154,  1736,   157,   151,   176,   602,
     158,   154,   151,   154,   152,   152,  1681,  1647,   158,  1907,
     152,   152,    14,   152,   152,   159,    88,  1681,    74,   151,
     177,   158,   625,    91,   627,   628,   159,   157,   562,   157,
     151,  1695,   104,  1944,   177,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  2018,  1786,   649,   650,  1789,   160,
     177,   177,   158,   656,   177,  1695,   154,   157,   154,  1800,
     177,   158,   154,  1804,   158,   158,   157,   154,   157,   154,
     151,  1907,   152,  1968,   177,   152,  1806,  1818,   177,    80,
     151,  1681,   177,   152,   154,  1181,   152,  1828,   151,  1819,
    1627,  1789,   151,   177,   177,  1681,   158,  1681,   177,  1681,
     160,   177,  1639,   177,  1845,  1846,  1681,   158,   151,   157,
     160,  1695,   157,   157,   154,  1652,   151,   154,   157,  1606,
     151,   159,   121,  1606,  1611,   159,   154,   154,  1611,  2083,
     154,  1806,  1619,   154,   157,   157,  1619,   154,   151,   151,
     159,  2083,   152,   154,  1819,   177,   152,  1940,   152,   158,
    1630,  2049,  1689,  2051,   110,  1819,   151,   157,  2083,  2083,
     157,   157,  1258,   151,  1905,   154,   160,   151,   154,   154,
    1911,   154,   154,   154,  1915,   154,  2083,  2088,   104,  1920,
     157,  2092,  2093,   109,   110,   111,   112,   113,   114,   115,
     116,  1647,  2090,   154,    75,    75,   151,  2083,  1294,  1295,
    1296,   177,   177,  2083,  1945,  1301,  1302,  1905,   152,   177,
     157,   154,   154,  2049,    90,  2051,  2127,  1248,  1940,  1819,
     157,   160,   151,  2016,   151,  1681,   151,  1258,  2083,    75,
     156,   154,  2083,  1819,   154,  1819,   154,  1819,   154,   154,
    1940,   154,  2153,   154,  1819,   155,  2157,   156,    75,  1990,
     151,   168,   159,  1994,  2090,   177,  2083,  1987,   177,   151,
     168,   154,   158,  1800,   154,   177,  2007,  1804,  2179,   154,
     154,  1751,  1752,   151,  2172,  2083,   153,  2018,   151,  2020,
     168,  1818,  2083,   104,   159,  2015,  2083,  2123,  2083,   168,
    2031,  1828,  2033,  2034,  2016,   152,   158,   900,    75,   152,
     151,   168,  1789,   168,   153,   177,  1789,   177,  1845,   157,
     913,   110,  1987,   110,   151,     1,  2016,  2058,     4,   153,
     159,   154,   154,  1987,   154,  2055,  1251,   151,   151,    75,
     152,  1319,   177,   177,   937,   154,  2172,   154,  1726,   177,
    2015,   707,  2083,  2084,   748,   948,   749,  1210,  2084,  1199,
     750,  2015,   430,  2094,   957,  1280,   751,  1681,   752,  2141,
    2051,  2080,  1827,  1819,  2105,  1819,  1949,    13,    14,    15,
      16,    17,  2134,    59,  1911,  2135,  2122,  1673,  1915,  1673,
    2055,  2016,  2093,  1920,  2125,  2157,  2084,  1987,    74,  2125,
    2015,  2055,  1231,    49,  1227,  1228,   111,    83,  2139,   263,
    2141,  1987,  1905,  1987,  1329,  1987,  1977,   963,  1945,  1410,
      96,  1224,  1987,    99,   828,  2015,   927,   103,  1905,  2160,
     616,   494,  1905,     0,  1695,  2166,    72,  2125,   972,  2015,
      -1,  2015,   773,  2015,  2152,  2176,   104,  1594,   773,  2180,
    2015,   109,   110,   111,   112,   113,   114,   115,    -1,  2190,
    2168,  1054,    -1,  1990,   140,  2055,   773,  1994,    -1,    -1,
     146,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,  2055,
    2007,  2055,    -1,  2055,    -1,  1078,    -1,    -1,    -1,   165,
    2055,    -1,    -1,  2020,   152,   153,   132,    -1,  1968,   121,
      62,   123,   124,   125,  2031,    -1,  2033,  2034,    -1,    -1,
      -1,    -1,    -1,   189,   190,   191,   152,    -1,    -1,    -1,
     156,    -1,    -1,    74,    -1,   201,   202,   163,   164,   205,
     152,  2058,    -1,   155,   156,    -1,    -1,    99,   160,   161,
      -1,  1987,    -1,    -1,    -1,    96,   240,    -1,   110,   111,
      -1,    -1,    -1,   229,   230,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,  2094,    -1,  2015,
      -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,  2105,    -1,
      -1,    -1,   258,    -1,    -1,    -1,  1501,    -1,    -1,    -1,
      -1,   153,  1613,  1508,    -1,    -1,   272,    -1,    -1,    -1,
    1686,   152,    -1,    -1,    -1,    -1,   215,  2084,    83,  2055,
    1525,  2084,  2139,    -1,  2141,   104,    72,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
     306,    -1,    -1,  2160,  1227,    -1,   312,   313,   314,  2166,
      -1,  1464,    -1,   205,    -1,    -1,   322,    -1,  2125,  2176,
      -1,   104,  2125,  2180,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  2190,    -1,    -1,   342,   343,   344,  1262,
      -1,    -1,    -1,    -1,   149,  1268,   132,    -1,   229,   230,
      -1,    -1,    -1,   359,    -1,    -1,    -1,   363,   177,     3,
     165,    -1,    -1,    18,    -1,    -1,   152,    -1,    -1,   250,
     156,    -1,    -1,   156,    -1,    -1,    -1,   163,   164,    -1,
     272,    -1,    -1,    -1,   189,    -1,    -1,    -1,    -1,    -1,
      -1,   397,    -1,    -1,    -1,    -1,    -1,   202,    65,    66,
      67,    68,    57,    58,    59,    60,    61,    62,    63,    64,
     434,    -1,   582,    -1,   306,    -1,    -1,    -1,  1663,    -1,
     312,    -1,    -1,    -1,    -1,   431,   450,    -1,   434,   453,
      -1,    -1,   313,   314,    -1,   441,    -1,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,   457,   344,   258,    -1,   461,    -1,    -1,    -1,   465,
     104,   467,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,  1717,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,  1728,    -1,    -1,   510,    -1,   132,   156,
      -1,    -1,    -1,  1416,    -1,    -1,   388,   875,    -1,   505,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,   152,   153,
      -1,   517,    -1,    -1,    -1,   159,    -1,   322,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,  1947,    -1,    -1,    -1,    -1,   542,   342,   544,   545,
      -1,  1464,   548,    -1,   550,    -1,   485,    -1,   487,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   104,   940,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,   467,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   589,   465,    -1,    -1,    -1,    -1,   132,
      -1,    -1,   484,    -1,   600,    -1,   602,    -1,   604,    -1,
     606,    -1,  1847,  1848,    -1,    -1,    72,    -1,   163,   152,
     153,    -1,    -1,   156,    -1,    -1,   622,   623,    -1,   625,
     163,   164,    -1,    -1,   505,    -1,   431,   633,    -1,    -1,
      -1,   637,    -1,    -1,    -1,    -1,   517,    -1,   104,    -1,
     646,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     656,    -1,    -1,   659,    -1,    -1,   461,   549,    -1,    -1,
      -1,   542,    -1,    -1,   670,    -1,    -1,   548,    -1,    -1,
      -1,    -1,  1050,    -1,    -1,    -1,    -1,   683,   570,    -1,
     686,   687,    -1,   689,    -1,    -1,   152,   153,    -1,  1067,
    1068,    -1,   698,    -1,    -1,   701,   702,   703,   590,    -1,
      -1,    -1,    -1,    -1,    -1,   875,    -1,    -1,   589,    -1,
      -1,    -1,    -1,    -1,  1959,    -1,    -1,    -1,    -1,   600,
     104,   602,    -1,  1646,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   625,    -1,    -1,   120,    -1,   122,   544,
     545,    -1,    -1,    -1,    -1,   550,    -1,    -1,    72,   773,
     774,   757,    -1,    -1,    -1,    -1,    -1,   649,    -1,   783,
      -1,    -1,   786,    -1,   656,    -1,    -1,   773,   774,   153,
     940,    -1,   156,    -1,    -1,   781,    -1,    -1,    -1,    -1,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   798,  2038,    -1,   801,    -1,   803,    -1,   604,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,   820,   821,    -1,    -1,   623,    -1,
      56,    57,    -1,   847,    -1,    -1,    -1,    -1,   152,   153,
     854,    -1,    -1,   839,   858,   841,    -1,    -1,   862,   163,
     164,   646,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    91,    -1,    -1,    -1,    -1,
      -1,   867,    -1,    -1,    -1,   670,  1789,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
    1050,    -1,    -1,  1806,    -1,    -1,    -1,    -1,    -1,   828,
      -1,    -1,   831,    -1,    -1,   132,    -1,  1067,  1068,    -1,
      -1,    -1,   908,    -1,    -1,   141,    -1,    72,   144,    -1,
      -1,   917,    -1,    -1,    -1,   152,   153,   923,   924,   156,
     801,    -1,   803,   159,    -1,    -1,   163,   164,    -1,   935,
      -1,   937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   948,    -1,   109,   110,   111,   112,   113,   114,
     115,    -1,   757,    -1,    -1,    -1,    -1,  1335,  1336,    -1,
     841,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,  1349,  1350,  1896,    -1,    -1,   781,    -1,    -1,   871,
      -1,   987,    -1,    -1,   220,    -1,   867,   152,   153,    -1,
      -1,    -1,    -1,   798,    -1,   104,    -1,    -1,   163,   164,
     109,   110,   111,   112,   113,   114,   115,    -1,   900,  1387,
    1388,  1389,    -1,    -1,    -1,   820,   821,    -1,    -1,    -1,
      -1,   913,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1054,    -1,   268,   269,   839,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,   279,   153,   937,   104,   156,  1054,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   294,    -1,
      -1,    -1,    -1,    -1,    -1,  1071,    -1,    -1,    -1,  1008,
      -1,    -1,  1078,    -1,   104,    -1,    -1,  1016,    -1,   109,
     110,   111,   112,   113,   114,   115,    -1,   323,    -1,    -1,
      -1,    -1,    -1,    -1,   330,   331,  2019,   155,    -1,   335,
      -1,  1125,   132,    -1,  1128,    -1,  1045,    -1,  1132,  1048,
      -1,    -1,   917,    -1,  1120,    -1,    -1,    -1,    -1,   924,
      -1,   104,   152,   153,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   163,   164,    -1,    -1,   373,   104,    -1,
     376,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,    -1,
      -1,    -1,    -1,    -1,    -1,  1335,  1336,  1173,    -1,  1108,
      13,    14,    15,    16,    17,  1181,    -1,   160,    -1,  1349,
    1350,    -1,   987,    -1,    -1,    -1,    -1,    -1,    -1,   155,
    1071,    -1,    -1,    -1,   160,    -1,  1077,  1078,    -1,    -1,
      -1,    -1,  2125,    -1,  1210,    -1,  1584,  1585,  1586,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1387,  1388,  1389,
      -1,  1227,  1228,    -1,    -1,    -1,    -1,    -1,   464,    72,
      -1,    -1,    -1,    -1,    -1,    -1,  1242,    -1,   104,    -1,
     476,   477,  1248,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,  1258,    -1,   120,    -1,   122,    -1,    -1,    -1,
      -1,   104,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,   115,     1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,  1287,    -1,    -1,    -1,  1291,    -1,   153,    -1,   132,
     156,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1331,    -1,   152,
     153,    -1,    -1,   156,    -1,  1120,  1340,    -1,    -1,    -1,
     163,   164,    -1,    -1,    -1,    -1,    -1,    -1,  1706,    -1,
      -1,    59,    -1,    -1,    -1,  1227,    -1,    -1,    -1,    -1,
      -1,   397,    -1,    -1,    -1,    -1,    -1,    -1,  1354,  1355,
      -1,    -1,    -1,    72,    -1,    83,    -1,    -1,    -1,    -1,
      -1,  1242,    -1,  1741,    -1,  1304,    -1,    -1,  1746,  1747,
    1262,    99,    -1,  1312,    -1,   103,    -1,    -1,    -1,    -1,
     616,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,  1404,  1405,
    1406,  1407,    -1,  1409,  1410,    -1,    -1,    -1,    -1,  1415,
    1416,    -1,   140,   132,  1584,  1585,  1586,    -1,   146,    -1,
      -1,   149,    -1,    -1,    -1,   153,    -1,    -1,  1434,  1435,
     666,    -1,    -1,   152,   153,    -1,   164,   165,   166,    -1,
      -1,    -1,    -1,  1248,   163,   164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1258,    -1,    -1,    -1,    -1,  1464,    -1,
    1630,   189,    -1,  1344,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   201,   202,    -1,    -1,   205,    72,    -1,
    1486,    -1,  1287,    -1,    -1,    -1,    -1,   104,   544,   545,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,  1442,  1396,  1511,    -1,  1513,    -1,    -1,
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,  1416,    -1,    -1,    -1,   256,    -1,
     258,    -1,    -1,    -1,  1415,    -1,  1706,    -1,   132,    -1,
      -1,    -1,    -1,   160,   272,    -1,    -1,    -1,    -1,  1354,
    1355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   293,    -1,  1506,    -1,   163,
     164,  1741,  1464,   301,    -1,    -1,  1746,  1747,   306,   815,
      -1,    -1,    -1,  1607,   312,  1524,   822,    -1,    -1,    -1,
    1529,    -1,    -1,    -1,   322,  1534,  1535,  1536,    -1,    -1,
    1606,  1607,    -1,    -1,    -1,  1611,    -1,  1613,    -1,    -1,
      -1,  1617,    -1,  1619,   342,   104,   344,   345,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   683,    -1,    -1,
      -1,   359,    -1,   689,    -1,   363,    -1,    -1,    -1,    -1,
    1646,  1647,   698,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,   717,    -1,    -1,   153,    -1,    -1,   156,    -1,   397,
      -1,   907,    72,    -1,    -1,  1681,    -1,    -1,    -1,    -1,
      -1,  1486,    -1,    -1,    -1,    -1,  1692,    -1,    -1,  1695,
      -1,    13,    14,    15,    16,    17,    -1,   753,    -1,   160,
      -1,    -1,    -1,   431,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
    1726,    -1,    -1,    -1,  2102,  1606,    -1,    -1,    -1,   457,
    1611,    -1,   132,   461,    -1,    -1,  1617,    -1,  1619,   467,
      -1,    -1,  1183,    -1,    -1,  1751,  1752,  1771,  1772,    -1,
      72,    -1,   152,   153,  1646,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,  1771,  1772,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
    1786,    -1,   104,  1789,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1734,   104,  1736,    -1,    -1,
    1806,   109,   110,   111,   112,   113,   114,   115,  1613,    -1,
     132,    -1,    -1,  1819,    -1,    -1,   544,   545,   155,    -1,
      -1,   549,   550,    -1,   132,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,
    1846,   163,   164,    -1,   152,   153,    -1,    -1,   156,    -1,
      -1,    -1,    -1,    -1,   582,   163,   164,    -1,    -1,   587,
      -1,    -1,   590,   591,    -1,   593,    -1,    -1,    -1,    -1,
      -1,    -1,  1896,    -1,    -1,    -1,   604,    -1,   606,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1692,    -1,    -1,
    1896,    -1,   620,    -1,   622,   623,    -1,   625,    -1,  1905,
      -1,    -1,    -1,    -1,    -1,  1786,    -1,    -1,  1789,    -1,
      -1,    -1,    -1,    -1,  1806,    -1,    -1,    -1,   646,    -1,
      -1,   649,    -1,    -1,    -1,   653,    -1,    -1,   656,    -1,
      -1,   659,  2102,   661,  1940,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   670,    -1,    -1,    -1,  1751,  1752,    -1,  1973,
    1186,  1187,  1188,  1977,    -1,   683,    -1,    -1,   686,   687,
      -1,   689,  1968,    -1,    -1,  1846,    -1,    -1,    -1,    -1,
     698,  1977,    -1,   701,   702,   703,    -1,    -1,    -1,    -1,
    1216,  1987,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,  2018,  1231,    -1,   104,    -1,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  2015,
    2016,    -1,  2018,  2019,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1462,    -1,    -1,  1905,   104,    -1,    -1,    -1,   757,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,  1275,
      -1,   120,    -1,   122,    72,   152,    -1,    -1,    -1,  2055,
      -1,    -1,    -1,   781,    13,    14,    15,    16,    17,  2083,
    2084,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     798,    -1,    -1,    -1,   153,    -1,   104,  2083,  2084,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,   820,   821,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2125,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,   839,    -1,    72,    -1,    -1,    -1,    -1,    -1,  2125,
      -1,    -1,    -1,    -1,   152,   153,    -1,  2019,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,  2018,    -1,    -1,
      -1,    -1,    -1,   871,    -1,   104,    -1,   875,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,  1968,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   900,   132,    -1,    -1,    -1,    -1,    -1,    -1,
     908,    -1,    -1,  1419,    -1,   913,  1422,    -1,    -1,   917,
      -1,    -1,    -1,   152,   153,   923,   924,    -1,  1639,    -1,
      -1,    -1,  2083,  2084,   163,   164,    -1,   935,    -1,   937,
     938,  1652,   940,    -1,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,   132,    -1,  2125,    -1,    -1,   104,  1689,    -1,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   987,
      -1,    -1,   152,   153,    -1,    -1,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   176,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    59,
      -1,    -1,   159,    -1,    -1,    -1,   163,   164,   102,    -1,
     104,    83,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,  1050,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1067,
    1068,   104,    -1,   103,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,   152,  1800,
      -1,   155,   156,  1804,    -1,    -1,    -1,    -1,   140,    -1,
      -1,    -1,    -1,    -1,   146,  1431,    -1,  1818,  1434,  1435,
      -1,    -1,    -1,    -1,  1440,    -1,   146,  1828,  1444,   152,
    1446,  1627,  1120,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   165,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   104,    -1,   190,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   201,
     202,   132,    -1,    -1,    -1,    -1,  1164,    -1,    -1,    -1,
      -1,    -1,   202,    -1,   132,  1173,    -1,    -1,    -1,    -1,
      -1,   152,   153,  1181,    -1,    -1,    -1,    -1,   230,    -1,
      -1,    -1,   163,   164,   152,   153,    -1,    -1,    -1,    -1,
    1911,    -1,    -1,    -1,  1915,   163,   164,    -1,   250,  1920,
      -1,    -1,  1210,   255,   256,    -1,   258,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,    -1,  1227,
    1228,   149,    -1,    -1,  1945,    -1,    -1,    -1,    -1,   281,
      -1,    -1,    -1,   285,    -1,    -1,    -1,    -1,   290,    -1,
    1248,   281,    -1,    -1,    -1,    -1,    -1,    -1,   176,   301,
    1258,    -1,    -1,    -1,  1262,    -1,  1592,    -1,    -1,    -1,
      -1,   301,   314,    -1,    -1,    -1,  1274,    -1,    -1,  1990,
      -1,    -1,    -1,  1994,    -1,    -1,    -1,    -1,  1286,  1287,
      -1,    -1,    -1,  1291,    -1,    -1,  2007,    -1,    -1,    -1,
     342,    -1,    -1,   345,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   342,    -1,    -1,   345,  1642,   359,    -1,    -1,
    2031,   363,  2033,  2034,    -1,    -1,    -1,    -1,    -1,   359,
      -1,  1657,  1658,   363,    -1,    -1,    -1,  1335,  1336,  1845,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2058,    -1,    -1,
      -1,  1349,  1350,   104,    -1,  1681,  1354,  1355,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,    -1,    -1,   120,
     104,   122,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,  2094,    -1,    -1,    -1,    -1,    -1,  1387,
    1388,  1389,    -1,    -1,  2105,    -1,    -1,    -1,  1396,    -1,
      -1,    -1,   153,    -1,    -1,   156,  1404,  1405,  1406,  1407,
    1408,  1409,  1410,    -1,    -1,   457,    -1,    -1,  1416,   461,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2139,    -1,
    2141,   461,    -1,    -1,    -1,    -1,  1434,  1435,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2160,
      -1,    -1,    -1,    -1,    -1,  2166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2176,  1464,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1802,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1811,    -1,  1813,  1486,    -1,
    1816,  1817,    -1,  1819,    -1,    -1,    -1,    -1,  1824,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,    -1,
      -1,    -1,    -1,  1511,    -1,    -1,    -1,    -1,    -1,    -1,
     550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     582,    -1,    -1,    -1,    -1,   587,    -1,    -1,    -1,   591,
      -1,   593,   582,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   591,   604,   593,   606,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   604,    -1,   606,    -1,    -1,    -1,
     622,   623,    -1,    -1,    -1,    -1,  1584,  1585,  1586,    -1,
      -1,    -1,   622,   623,    -1,   637,    -1,    -1,    -1,    -1,
    1926,    -1,    -1,    -1,   646,  1931,  1932,    -1,    -1,   651,
      -1,    -1,    -1,    -1,    -1,  1613,   646,   659,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   659,
      -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1646,  1647,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1995,
      -1,  1997,    -1,    -1,  2000,  2001,    -1,    -1,    -1,  2005,
    2006,    -1,    -1,  1681,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1692,    -1,    -1,  1695,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1706,    -1,
      -1,    -1,    -1,    -1,    -1,   757,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,  1726,    -1,
      -1,    -1,   774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1741,    -1,    -1,    -1,    -1,  1746,  1747,
    2076,  2077,  2078,  1751,  1752,    -1,   798,    -1,    -1,    -1,
      -1,   803,    -1,    -1,    -1,    -1,    -1,    -1,   798,    -1,
      -1,  2097,    -1,    -1,    -1,    -1,    -1,    -1,   820,   821,
      -1,    -1,    -1,    -1,    -1,  2111,  2112,  2113,    -1,    -1,
     820,   821,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1806,   839,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   875,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   875,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   908,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,   917,    -1,    -1,   908,    -1,
      -1,   923,    -1,    -1,    -1,    -1,    -1,   917,    -1,    -1,
      -1,    -1,    59,   935,    -1,    -1,   938,    -1,   940,    -1,
      -1,    -1,    -1,   945,    -1,   935,    -1,    -1,   938,    -1,
     940,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
      -1,    -1,  1940,    -1,   111,   987,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   987,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1968,   103,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,
      -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,  1987,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1050,    -1,
      -1,    -1,    -1,    -1,   146,    -1,    -1,  2015,  2016,    -1,
    1050,  2019,   189,    -1,    -1,  1067,  1068,    -1,    -1,    -1,
      -1,    -1,    -1,   165,   201,    -1,    -1,  1067,  1068,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2055,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   256,
    1120,   258,    -1,    -1,    -1,    -1,   263,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1173,    -1,    -1,   301,    -1,    -1,    -1,    -1,  1181,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1181,    -1,    -1,    -1,   322,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1210,   301,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1210,    -1,    -1,    -1,    -1,    -1,  1228,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1228,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     342,    -1,    -1,   345,    -1,    -1,  1258,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1267,    -1,   359,  1258,    -1,
     397,   363,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1286,  1287,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1286,  1287,    -1,    -1,
      -1,    -1,    -1,    -1,   431,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     457,    -1,    -1,  1335,  1336,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1335,  1336,  1349,  1350,    -1,
      -1,    -1,  1354,  1355,    -1,    -1,    -1,    -1,    -1,  1349,
    1350,    -1,    -1,    -1,  1354,  1355,    -1,    -1,    -1,   461,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1387,  1388,  1389,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1387,  1388,  1389,
      -1,    -1,  1404,  1405,  1406,  1407,  1408,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   544,   545,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,  1434,  1435,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1434,  1435,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,    -1,
     587,    -1,    -1,    -1,   591,    -1,   593,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,
      -1,    -1,    -1,    59,  1486,    -1,    -1,    -1,    -1,    -1,
     582,    -1,    -1,    -1,    -1,    -1,  1486,    -1,    -1,   591,
      -1,   593,    -1,    -1,    -1,    -1,    -1,    83,    -1,  1511,
      -1,    -1,   604,    -1,   606,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     622,   623,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   670,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   646,    -1,   683,    -1,    -1,   686,
     687,    -1,   689,    -1,   140,    -1,    -1,   659,    -1,    -1,
     146,   698,    -1,   149,   701,   702,   703,    -1,    -1,    -1,
      -1,    -1,  1584,  1585,  1586,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1584,  1585,  1586,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   189,    -1,    -1,    -1,  1619,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   201,    -1,    -1,  1630,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1630,    -1,    -1,    -1,    -1,  1647,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   781,    -1,   181,  1647,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   757,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1681,
     256,    -1,   258,    -1,    -1,    -1,    -1,   263,    -1,    -1,
    1692,  1681,    -1,  1695,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1692,    -1,  1706,    -1,   798,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1706,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,   301,    -1,     4,   820,   821,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1741,
      -1,    -1,    -1,    -1,  1746,  1747,   322,   839,    -1,  1751,
    1752,  1741,    -1,    -1,    -1,    -1,  1746,  1747,    -1,    -1,
      -1,  1751,  1752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   908,    59,   875,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   923,   924,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   908,  1819,    -1,    -1,
      -1,   397,    -1,    -1,    -1,   917,   103,    -1,    -1,  1819,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   935,    -1,    -1,   938,    -1,   940,    -1,
      -1,    -1,    -1,    -1,    -1,   431,   381,    -1,    -1,    -1,
     385,   386,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,
     395,   396,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   457,    -1,    -1,    -1,   410,   411,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   987,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   431,    -1,    -1,    -1,
      -1,    -1,   189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   461,    -1,  1940,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1940,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1050,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1968,    -1,   544,   545,
      -1,    -1,    -1,    -1,    -1,  1067,  1068,    -1,  1968,   256,
      -1,   258,    -1,    -1,    -1,  1987,   263,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1987,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   587,    -1,  2015,  2016,   591,    -1,   593,    -1,    -1,
      -1,    -1,    -1,    -1,   301,  2015,  2016,    -1,  1120,    -1,
     606,    -1,    -1,    -1,    -1,    -1,    -1,  1164,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   322,  1173,    -1,    -1,    -1,
      -1,    -1,    -1,  2055,  1181,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2055,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2084,  1210,    -1,    -1,    -1,    -1,    -1,  1181,
      -1,    -1,    -1,    -1,   670,    -1,    -1,    -1,    -1,    -1,
    2102,  1228,    -1,    -1,    -1,    -1,    -1,   683,    -1,    -1,
     686,   687,  2102,   689,    -1,    -1,    -1,    -1,  1210,    -1,
     397,  1248,   698,    -1,    -1,   701,   702,   703,    87,    -1,
      -1,    -1,    -1,    -1,    93,    94,  1228,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   431,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1291,    -1,  1258,    -1,   127,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     457,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1286,  1287,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   781,    -1,    -1,    -1,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1335,  1336,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1349,  1350,    -1,
      -1,    -1,  1354,  1355,    -1,    -1,    -1,   544,   545,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1404,  1405,  1406,
    1407,  1408,  1409,  1410,    -1,    -1,    -1,    -1,    -1,   814,
      -1,    -1,    -1,    -1,    -1,  1387,  1388,  1389,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1434,  1435,    -1,
     587,    -1,    -1,    -1,   591,    -1,   593,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,
      -1,    -1,   908,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1434,  1435,    -1,    -1,    -1,   923,   924,    -1,
      -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1511,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   670,  1486,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   683,    -1,    -1,   686,
     687,    -1,   689,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   698,    -1,    -1,   701,   702,   703,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   983,    -1,
      -1,    -1,    -1,   988,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   999,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1613,    -1,    -1,    -1,
      -1,    -1,  1584,  1585,  1586,    -1,    -1,    -1,    -1,    -1,
      -1,   460,    -1,   462,   781,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   471,   472,    -1,     0,  1041,    -1,     3,    -1,
    1647,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1630,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1681,  1647,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1695,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,  1681,
      -1,    -1,    77,    -1,    -1,    -1,    -1,  1173,    -1,  1726,
    1692,    -1,    -1,    -1,    -1,  1181,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1706,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   588,
      -1,   908,    -1,    -1,  1210,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   923,   924,    -1,  1741,
      -1,   136,  1228,    -1,  1746,  1747,  1181,    -1,    -1,  1751,
    1752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1291,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1819,    -1,    -1,
      -1,    -1,    -1,  1258,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1277,    -1,   239,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1294,
    1295,  1296,   257,    -1,    -1,    -1,  1301,  1302,    -1,    -1,
      -1,    -1,   267,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   278,    -1,    -1,    -1,    -1,    -1,    -1,
    1325,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   296,   297,    -1,    -1,    -1,    -1,    -1,   303,   304,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1404,  1405,
    1406,  1407,  1408,  1409,  1410,    -1,   321,  1362,  1363,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1940,    -1,
      -1,    -1,    -1,    -1,    -1,   814,   341,    -1,  1434,  1435,
    1987,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1968,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,  2015,    -1,
      -1,    -1,    -1,    -1,    -1,  1987,  1173,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1181,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   398,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2015,  2016,    -1,    -1,    -1,  2055,    -1,
      -1,    -1,    -1,  1210,    -1,  1511,    -1,    -1,    -1,    -1,
      -1,    -1,   901,   902,   429,    -1,    -1,    -1,    -1,    -1,
      -1,  1228,    -1,    -1,    -1,   914,   915,   916,    -1,    -1,
     919,    -1,    -1,  2055,    -1,    -1,    -1,    -1,    -1,    -1,
     455,  1248,    -1,    -1,   459,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   478,    -1,    -1,    -1,   482,   483,    -1,
      -1,   486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2102,    -1,    -1,    -1,  1291,    -1,   501,   502,   503,   504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1613,    -1,   524,
      -1,    -1,  1001,    -1,    -1,    -1,    -1,    -1,   533,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1647,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     565,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1047,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1681,    -1,    -1,    -1,    -1,
      -1,   596,    -1,    -1,    -1,    -1,    -1,    -1,   603,  1695,
      -1,    -1,    -1,    -1,   609,    -1,    -1,  1404,  1405,  1406,
    1407,  1408,  1409,  1410,    -1,  1094,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1103,  1104,  1105,  1106,   633,   634,
    1726,    -1,  1111,  1112,    -1,    -1,    -1,  1434,  1435,    -1,
      -1,  1686,  1121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1150,    -1,    -1,  1153,    -1,  1155,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   704,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1511,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1819,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1236,    -1,    -1,
      -1,    -1,    -1,    -1,  1243,    -1,  1245,  1246,   773,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,  1257,    -1,
    1259,    -1,  1261,   788,  1263,    -1,    -1,   792,    -1,  1268,
      -1,    -1,    -1,    -1,    -1,    -1,   801,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1613,    -1,   823,    -1,
      -1,   120,    -1,    -1,    -1,    -1,    -1,   832,    -1,    -1,
      -1,    -1,    -1,   838,   133,    -1,   135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1647,  1330,    -1,    -1,    -1,    -1,    -1,    -1,  1337,  1338,
      -1,    -1,  1907,    -1,    -1,    -1,    -1,    -1,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   884,
      -1,    -1,  1361,   888,  1681,    -1,    -1,   892,    -1,  1368,
      -1,  1987,   191,  1372,    -1,    -1,    -1,    -1,  1695,    -1,
      -1,    -1,  1947,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   921,    -1,    -1,  2015,
      -1,  1400,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1726,
     229,    -1,    -1,    -1,   233,  1414,    -1,   236,   237,    -1,
      -1,   240,    -1,    -1,   243,   244,    -1,   246,    -1,   248,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2055,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,   977,    -1,    20,  1454,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,  2049,    51,  2051,    53,    -1,    -1,
      -1,  1490,    -1,    -1,   313,    -1,    -1,   316,    -1,  1498,
      -1,  1500,  1819,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     339,   340,    -1,    -1,    -1,  2090,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1060,   355,    -1,    -1,  1064,
      -1,    -1,    -1,    -1,    -1,    -1,  1071,    -1,    -1,    -1,
    1549,  1550,    -1,    -1,    -1,    -1,  1081,    -1,  2123,    -1,
      -1,    -1,    -1,  1088,    -1,  1564,  1565,    -1,  1567,    -1,
      -1,    -1,  1097,    -1,  1099,    -1,    -1,  1576,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1587,  1588,
    1589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2172,    -1,  1134,
      -1,    -1,    -1,  1138,    -1,    -1,    -1,  1142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   450,    -1,    -1,    -1,  1160,    -1,    -1,    -1,    -1,
    1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1987,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2015,    -1,
      -1,    -1,   521,    -1,    -1,    -1,    -1,    -1,  1707,  1708,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1720,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1256,    -1,    -1,    -1,    -1,    -1,    -1,  2055,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1281,    -1,  1757,  1758,
    1759,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   600,    -1,    -1,    -1,    -1,    -1,    -1,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,   641,   642,   201,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   654,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1375,  1850,    -1,    -1,  1379,    -1,   233,    -1,  1383,    -1,
      -1,    -1,    -1,   240,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1871,    -1,    -1,  1874,  1875,    -1,    -1,    -1,
      -1,    -1,  1881,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,    -1,    -1,    -1,    -1,  1421,    -1,    13,    14,
      15,    16,    17,    -1,   133,    20,   135,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,   316,
      55,    -1,    -1,    -1,    -1,    -1,  1471,    -1,    -1,  1474,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,   777,   778,
      -1,    -1,    -1,    -1,   783,   342,   343,  1492,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   804,   363,    -1,   807,   808,
      -1,   810,    -1,   812,   813,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   236,   237,    -1,
      -1,   240,    -1,    -1,   243,   244,    -1,   246,  2017,   248,
    1545,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1554,
      -1,    -1,    -1,  1558,    -1,   854,    -1,    -1,    -1,   858,
      -1,    -1,    -1,   862,    -1,    -1,    -1,  1572,  1573,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   450,   451,  1600,   453,   454,    -1,    -1,
      -1,    -1,  2081,    -1,   461,    -1,    -1,    -1,   465,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2106,    -1,    -1,
     339,   340,    -1,   932,   933,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2121,    -1,    -1,    -1,   355,   946,    -1,   506,
      -1,    -1,    -1,   510,    -1,    -1,    -1,    -1,  2137,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,  1696,  1697,   550,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    58,    59,    60,    61,    62,    63,
      64,   450,    -1,    -1,   601,    -1,    -1,   604,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   622,   623,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   633,    -1,  1077,    83,
     637,    -1,    -1,    -1,    -1,    -1,    -1,   644,    -1,   646,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   521,    -1,    -1,    -1,    -1,    -1,    -1,  1118,
      -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,  1833,  1128,
     154,    -1,    -1,  1132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,
      -1,    -1,    -1,   177,    -1,  1860,    -1,    -1,    -1,    -1,
      -1,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   189,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   202,    -1,
     757,   205,    -1,    -1,  1909,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   773,   774,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   783,   784,    -1,   786,
     787,  1936,   641,   642,  1939,    -1,    -1,    -1,    -1,    -1,
      -1,   798,    -1,    -1,   801,   654,   803,   804,    -1,    -1,
      -1,    -1,    -1,   810,   258,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   820,   821,  1264,    -1,    -1,   272,    -1,
      -1,    -1,    -1,  1272,  1273,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   839,    -1,    -1,    -1,   843,    -1,    -1,    -1,
     847,    -1,    -1,    -1,    -1,    -1,    -1,   854,   855,   189,
      -1,   858,   859,    -1,    -1,   862,   863,    -1,   312,    -1,
      -1,    -1,   202,   870,    -1,    -1,    -1,    -1,   322,    -1,
      -1,    -1,    -1,    -1,    -1,   215,    -1,   217,    -1,    -1,
      -1,    -1,  1331,    -1,    -1,    -1,    -1,    -1,   342,  2044,
     344,  1340,    -1,    -1,  1343,    -1,  1345,  1346,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     917,   918,    -1,    -1,    -1,    -1,    -1,    -1,   777,   778,
      -1,    -1,    -1,    -1,   783,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   948,  1391,   397,    -1,   804,    -1,    -1,   807,   808,
      -1,   810,    -1,   812,   813,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   311,    -1,    -1,    -1,    -1,    -1,   431,    -1,    -1,
     987,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   854,    -1,    -1,    -1,   858,
      -1,    -1,    -1,   862,    -1,    -1,    -1,   461,    13,    14,
      15,    16,    17,   467,    -1,    20,  1465,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,  1054,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1071,  1072,    -1,    72,    -1,    -1,
      -1,  1078,    -1,   932,   933,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   946,    -1,    -1,
     544,   545,    -1,    -1,    -1,    -1,   550,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1553,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,  1125,  1126,
      -1,  1128,  1129,    -1,    -1,  1132,  1133,    -1,    -1,    -1,
      -1,    -1,    -1,  1582,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   487,    -1,    -1,
     604,    -1,    -1,   493,    -1,    -1,    -1,    -1,   498,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1617,   623,
      -1,   625,    -1,    -1,  1623,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   646,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   670,    -1,  1077,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   683,
      -1,    -1,   686,   687,    -1,   689,    -1,    -1,    -1,    -1,
      -1,    -1,  1691,    -1,   698,    -1,    -1,   701,   702,   703,
      -1,  1258,    -1,    -1,   594,    -1,    -1,  1264,  1265,  1118,
      -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,    -1,  1128,
      -1,    -1,    -1,  1132,    -1,    -1,    -1,    -1,    -1,    -1,
    1287,    -1,    -1,   623,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   636,    -1,    -1,    -1,
      -1,    -1,    -1,   757,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1331,  1332,    -1,   781,    -1,  1778,
    1779,    -1,    -1,  1340,  1341,    -1,  1343,  1786,    -1,    -1,
      -1,  1790,    -1,    -1,   798,    -1,    -1,  1354,  1355,    -1,
      -1,   691,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   820,   821,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   715,   716,    -1,    -1,   719,
      -1,   721,    -1,    -1,    -1,   839,    -1,   727,    -1,   729,
     730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1264,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1272,  1273,    -1,    -1,   757,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,     1,  1898,
      -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,   798,    -1,
      -1,    -1,    -1,   917,    -1,    -1,    -1,    -1,    -1,    -1,
     924,    -1,  1331,    -1,    -1,    -1,    -1,    -1,    -1,  1486,
      -1,  1340,    -1,   937,  1343,   825,  1345,  1346,   828,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,  1513,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,  1973,    -1,    -1,    -1,    -1,    -1,
      -1,   871,  1391,   987,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,   924,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   937,   938,   152,
    1607,    -1,   155,   156,    -1,   945,  1465,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,  1623,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   973,  2083,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   987,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   995,    -1,    -1,    -1,    -1,
      -1,    -1,  1002,    -1,    -1,    -1,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1692,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1553,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,  1048,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,  1173,
      -1,    -1,    -1,  1582,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1751,  1752,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1771,  1772,    -1,    -1,    -1,    -1,
      -1,    72,    73,  1227,    -1,  1115,    -1,  1117,    -1,  1119,
    1787,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,  1258,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,  1691,  1287,    -1,    -1,    -1,  1291,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,  1198,  1199,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1896,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1904,    -1,    -1,
    1354,  1355,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1778,
    1779,    -1,  1262,    -1,    -1,    -1,    -1,    -1,  1268,    -1,
      -1,  1790,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1287,    -1,    -1,
    1404,  1405,  1406,    -1,    -1,  1409,  1410,    -1,    -1,    -1,
      -1,  1968,  1416,    -1,  1304,    -1,  1973,  1974,    -1,    -1,
    1977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,
      -1,    -1,  1322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1464,  2018,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1486,    -1,  1374,    -1,    -1,    -1,    -1,  1898,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1395,  1396,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2083,  2084,    -1,    -1,
      -1,    -1,    -1,    -1,  1424,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,  1442,    -1,    -1,  1445,    -1,    -1,    13,    14,
      15,    16,    17,    -1,  1973,    -1,    -1,    -1,  2125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,  1486,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,  1496,  1497,    -1,  1613,
      -1,    -1,    -1,    -1,    -1,    -1,  1506,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1524,    -1,  1526,    -1,    -1,    -1,
      -1,    -1,  1646,    -1,    -1,    -1,   101,   102,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1692,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1726,  1613,    -1,    -1,    -1,    -1,  1618,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1751,  1752,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,  1677,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1806,    -1,    -1,    71,    -1,    73,    74,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    -1,    93,    94,    95,
      96,    97,    -1,    99,  1724,   101,   102,  1727,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,     1,    -1,    -1,
      -1,    -1,    -1,    -1,  1764,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    18,   151,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1940,    71,    -1,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    -1,    93,
      94,    95,    96,    97,  1968,    99,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,     1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,  2016,    -1,    -1,  2019,    -1,   151,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,   177,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1948,    71,
      -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
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
      87,    88,    89,    90,    91,    -1,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,   103,   104,    -1,   106,
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
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
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
      -1,    -1,    -1,    -1,   101,   102,   103,   104,    -1,   106,
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
     102,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
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
      -1,    -1,    -1,    -1,   101,   102,   103,   104,    -1,   106,
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
     101,   102,   103,   104,    -1,   106,   107,   108,   109,   110,
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
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
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
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
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
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
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
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
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
     103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
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
      -1,    -1,    99,    -1,    -1,    -1,   103,   104,    -1,    -1,
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
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,   164,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   103,    -1,    -1,    -1,   107,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,     3,
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
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
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
      -1,    -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,   107,   108,
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
      47,    48,    -1,    50,    51,    -1,    53,     3,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,   107,   108,    -1,    -1,    -1,   163,   164,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
     155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,   164,
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
     104,    -1,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,    -1,
     163,   164,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   155,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,   155,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
     107,   108,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,   155,   156,
     104,    -1,    -1,   107,   108,    -1,   163,   164,    71,    -1,
      73,    -1,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    -1,
      93,    94,    95,    96,    97,    -1,    99,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
      18,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    49,   155,   156,    52,    -1,    54,   160,    56,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,   177,    73,    -1,    -1,    76,    -1,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,    93,    94,    95,    96,    97,
      -1,    99,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
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
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,   154,   155,   156,
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
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
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
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    -1,   120,   121,   122,    -1,   124,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,
     165,   166,   167,   168,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
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
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,   159,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,   107,   108,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      72,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,   107,   108,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,    -1,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,     4,
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
      -1,    -1,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
     155,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    13,    14,    15,    16,
      17,    18,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,   107,   108,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,   155,    -1,    51,    -1,    53,
     107,   108,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    78,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,   156,
      -1,    -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,
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
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
     107,   108,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,
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
      62,    63,    64,    -1,    13,    14,    15,    16,    17,    18,
      72,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,   107,   108,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,   107,   108,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    13,
      14,    15,    16,    17,    18,    72,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
     107,   108,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,   107,   108,    -1,   163,   164,    -1,    -1,
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
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    72,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,   107,   108,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    13,    14,    15,    16,
      17,    18,    72,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,   155,    -1,    51,    -1,    53,   107,   108,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
     107,   108,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
      -1,    13,    14,    15,    16,    17,   163,   164,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,   107,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,   107,   108,    -1,    -1,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,   155,    53,    -1,    55,    -1,    -1,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
     108,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    49,    -1,
      -1,    52,    -1,    54,   132,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   146,   147,
     148,    -1,    73,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    49,    -1,    -1,    52,    -1,
      54,   132,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,   147,   148,    -1,    73,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,   159,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
     154,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,    -1,
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
      -1,    -1,    -1,   104,    -1,    -1,   107,   108
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   179,   394,   395,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
      99,   103,   104,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   119,   132,   152,   153,   155,   156,   163,   164,
     182,   183,   184,   199,   282,   283,   284,   285,   286,   287,
     288,   289,   290,   291,   292,   293,   296,   299,   301,   302,
     303,   304,   305,   306,   307,   308,   309,   311,   313,   314,
     315,   317,   318,   322,   323,   324,   325,   326,   328,   334,
     335,   336,   337,   348,   352,   386,   389,   399,   405,   407,
     413,   417,   422,   423,   424,   425,   426,   427,   428,   429,
     455,   473,   474,   475,   476,     0,   179,   104,   183,   199,
     286,   288,   299,   302,   305,   314,   318,   323,   118,   152,
      58,    61,    62,    64,   152,   152,   411,   412,   413,   310,
     311,   107,   108,   183,   366,   387,   388,   366,   152,   399,
     152,   152,     4,   104,   107,   108,   303,   308,   309,   152,
     199,   412,   417,   423,   424,   425,   427,   428,   429,   107,
     325,   157,   179,   289,   299,   302,   422,   426,   472,   473,
     476,   477,   177,   180,   149,   160,   176,   220,   369,    89,
     158,   406,   366,   180,   180,   180,   177,   107,   108,   152,
     199,   294,   295,   417,   418,   419,   420,   421,   422,   426,
     430,   431,   432,   433,   434,   435,   436,   437,   438,   444,
       3,    47,    48,    50,    55,   316,     3,   156,   199,   288,
     303,   307,   309,   319,   324,   402,   422,   426,   476,    69,
     286,   288,   302,   314,   318,   323,   403,   422,   426,    65,
     308,   308,   303,   309,   297,   308,   309,   316,   335,   303,
     308,   303,   155,   411,   158,   180,   152,   160,   228,   411,
     411,   179,   277,   278,   156,   299,   302,   474,   366,   366,
     399,   176,   302,   152,   199,   408,   417,   418,   422,   431,
     435,   156,   199,   476,   400,   401,    65,    66,    67,    68,
     156,   174,   366,   375,   377,   381,   383,   384,   324,    57,
     154,   156,   199,   298,   302,   306,   307,   313,   314,   320,
     321,   322,   323,   327,   334,   335,   352,   362,   364,   455,
     468,   469,   470,   471,   476,   477,   107,   108,   160,   183,
     324,   444,   413,   152,   382,   383,   152,   152,   118,   185,
     186,    49,    52,    54,    56,    73,   101,   102,   104,   106,
     116,   117,   120,   121,   122,   124,   125,   152,   156,   162,
     165,   166,   167,   168,   181,   182,   185,   187,   190,   198,
     199,   200,   201,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   213,   214,   215,   216,   222,   324,   154,   156,
     198,   199,   215,   217,   299,   324,   367,   368,   385,   472,
     477,   302,   423,   424,   425,   427,   428,   429,   154,   154,
     154,   154,   154,   154,   154,   156,   299,   455,   474,   156,
     163,   199,   217,   288,   289,   298,   300,   302,   314,   321,
     323,   357,   358,   361,   362,   363,   468,   476,   152,   422,
     426,   476,   152,   158,   104,   155,   156,   160,   182,   184,
     217,   370,   371,   372,   373,   374,    22,   370,   152,   366,
     228,   152,   158,   158,   158,   412,   417,   419,   420,   421,
     430,   432,   433,   434,   436,   437,   438,   302,   418,   431,
     435,   158,    99,   410,   156,   411,   452,   455,   410,   411,
     411,   406,   277,   152,   411,   452,   410,   411,   411,   406,
     411,   411,   302,   408,   152,   152,   301,   302,   299,   302,
     179,   299,   472,   477,   326,   160,   406,   277,   366,   366,
     369,   288,   307,   404,   422,   426,   160,   406,   277,   387,
     302,   314,   302,   302,   107,   325,   107,   108,   183,   324,
     329,   387,   179,   183,   365,   151,   179,     3,   293,   296,
     302,   306,   228,   179,   179,   410,   152,   410,   180,   217,
     412,   417,   302,   152,   179,   366,   397,   160,   366,   160,
     366,   134,   163,   164,   380,   154,   158,   366,   384,   154,
     411,   411,   157,   179,   300,   302,   314,   321,   323,   467,
     468,   476,   477,   152,   156,   164,   176,   199,   455,   457,
     458,   459,   460,   461,   462,   479,   199,   327,   476,   302,
     321,   308,   303,   411,   154,   300,   302,   469,   300,   455,
     469,    10,   162,   167,   351,   353,   354,   349,   351,   375,
     176,   375,    13,    88,   104,   107,   108,   182,   414,   415,
     416,   154,   118,   152,   198,   152,   152,   152,   201,   152,
     198,   152,   104,   107,   108,   303,   308,   309,   152,   198,
     198,    19,    21,    85,   156,   165,   166,   202,   203,   217,
     224,   228,   337,   367,   476,   158,   179,   152,   187,   156,
     161,   156,   161,   121,   123,   124,   125,   152,   155,   156,
     160,   161,   201,   201,   169,   163,   170,   171,   165,   166,
     126,   127,   128,   129,   172,   173,   130,   131,   164,   162,
     174,   132,   133,   175,   154,   158,   155,   179,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   176,
     219,   220,   221,   152,   199,   448,   449,   450,   451,   452,
     154,   158,   154,   154,   154,   154,   154,   154,   152,   411,
     452,   455,   152,   452,   455,   179,   299,   474,   179,   180,
     180,   152,   164,   199,   417,   439,   440,   441,   442,   443,
     444,   445,   446,   447,   134,   476,   180,   180,   366,   366,
     179,   179,   179,   156,   184,   179,   371,   159,   158,   478,
     370,   155,   156,   159,   374,   153,   217,   223,   152,   179,
     179,   179,   179,   417,   419,   420,   421,   430,   432,   433,
     434,   436,   437,   438,   154,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   418,   431,   435,   411,   152,   176,
     157,   179,   369,   228,   406,   179,   369,   228,   408,   224,
     368,   224,   368,   408,   397,   228,   406,   410,   160,   160,
     406,   277,   397,   228,   406,   331,   332,   330,   160,   134,
     302,   359,   360,   363,   364,   154,   158,    70,   279,   280,
     180,   422,   435,   302,   293,   163,   217,   179,   417,   358,
     399,   397,   157,   179,   152,   379,   377,   378,    78,   312,
     183,   160,   183,   444,   300,   455,   469,   302,   306,   476,
     179,   458,   459,   460,   157,   179,    18,   217,   302,   457,
     479,   411,   411,   455,   300,   467,   477,   302,   183,   411,
     300,   469,   324,   158,   478,   366,   351,   160,   154,   368,
     154,   154,   158,   152,   177,   367,   187,   156,   367,   367,
     367,   217,   367,   154,   367,   367,   367,   179,   154,   165,
     166,   203,    18,   304,   154,   158,   154,   163,   164,   154,
     223,   217,   160,   217,   183,   217,   183,   116,   156,   183,
     153,   191,   192,   193,   217,   116,   156,   183,   337,   217,
     191,   183,   201,   204,   204,   204,   205,   205,   206,   206,
     207,   207,   207,   207,   208,   208,   209,   210,   211,   212,
     213,   159,   224,   177,   185,   156,   183,   217,   160,   217,
     179,   449,   450,   451,   302,   448,   411,   411,   217,   368,
     152,   411,   452,   455,   152,   452,   455,   179,   179,   157,
     157,   152,   417,   440,   441,   442,   445,    18,   302,   439,
     443,   152,   411,   461,   479,   411,   411,   479,   152,   411,
     461,   411,   411,   180,   216,   366,   157,   158,   157,   158,
     479,   479,   134,   356,   357,   358,   356,   366,   179,   215,
     216,   217,   409,   478,   370,   372,   151,   179,   154,   158,
     179,   356,   183,   408,   183,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   152,   411,   452,   455,   152,   411,
     452,   455,   152,   411,   452,   455,   408,   185,    22,   455,
     217,   309,   324,   453,   228,   359,   154,   154,   154,   154,
     395,   396,   228,   151,   179,   397,   228,   406,   396,   228,
     160,   160,   160,   338,   180,   180,   183,   281,   366,    18,
      71,    73,    76,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    93,    94,    95,    96,    97,
      99,   107,   108,   119,   179,   224,   225,   226,   227,   228,
     229,   230,   232,   233,   243,   249,   250,   251,   252,   253,
     254,   259,   260,   266,   267,   268,   282,   302,   306,   366,
     407,    70,   177,   180,   180,   180,   356,   180,   398,   396,
     286,   288,   299,   390,   391,   392,   393,   385,   176,   376,
     376,   353,   411,   411,   300,   469,   156,   163,   199,   217,
     324,   217,   302,   359,   154,   154,   154,     5,   302,   411,
     457,   160,   183,   444,    10,   354,   151,   176,   355,   160,
     353,   160,   154,   415,   191,   154,   158,   179,   158,   154,
     154,   158,   154,   201,   154,   154,   154,   201,    18,   304,
     217,   154,   154,   153,   160,   201,   157,   180,   191,   157,
     157,   116,   120,   122,   184,   194,   195,   196,   154,   158,
     194,   157,   158,   151,   215,   159,   154,   194,   180,   371,
     359,   154,   154,   154,   448,   179,   179,   359,   359,   445,
     154,   154,   154,   154,   152,   417,   444,   439,   443,   179,
     179,   157,   180,   479,   179,   179,   180,   180,   180,   180,
     369,   194,   134,   168,   180,   180,   151,   370,   217,   411,
     153,   217,   356,   180,   176,   152,   411,   452,   455,   152,
     411,   452,   455,   152,   411,   452,   455,   179,   179,   179,
     410,   154,   146,   168,   180,   454,   158,   180,   180,   398,
     390,   396,   228,   398,   338,   338,   338,     3,     5,    10,
      73,   151,   283,   290,   291,   299,   302,   339,   344,   472,
     154,   158,   158,   177,   152,    61,    62,   177,   228,   282,
     407,   152,    18,   226,   152,   152,   177,   366,   177,   366,
     163,   366,   160,   225,   152,   152,   152,   228,   217,   218,
     218,    14,   269,    74,   234,   177,   180,   230,    78,   177,
     366,    91,   255,   365,   302,   159,   281,   177,   157,   157,
     180,   158,   398,   408,   180,   177,   180,   177,   180,   154,
     368,   382,   382,   478,   351,   351,   179,   180,   180,   180,
     217,   180,   152,   411,   461,   455,   301,     5,   163,   180,
     217,   353,   411,   411,   324,   366,   160,   216,   353,   478,
     151,   179,   154,   298,   183,    78,   188,   189,   367,   201,
     201,   201,   201,   201,   160,   371,   158,   151,   197,   156,
     195,   197,   197,   157,   158,   123,   155,   193,   157,   223,
     215,   177,   157,   478,   180,   152,   411,   452,   455,   359,
     359,   180,   180,   154,   152,   411,   452,   455,   152,   411,
     461,   417,   411,   411,   359,   359,   157,   358,   361,   361,
     362,   154,   158,   158,   154,   180,   216,   216,   157,   157,
     180,   180,   154,   217,   179,   179,   179,   359,   359,   359,
     369,   411,   158,   217,   217,   309,   324,   157,   154,   151,
     180,   398,   151,   151,   151,   151,   299,   299,   337,   345,
     472,   299,   344,   152,   333,   177,   177,   152,   159,   199,
     340,   341,   347,   417,   418,   431,   435,   158,   177,   366,
     179,   366,   191,   177,   228,   177,   228,   224,    80,   154,
     224,   235,   282,   284,   287,   293,   302,   306,   146,   147,
     148,   153,   154,   177,   224,   244,   245,   246,   282,   177,
     177,   224,   177,   371,   177,   224,   223,   224,   111,   112,
     113,   114,   115,   261,   263,   264,   177,    98,   177,    84,
     152,   152,   180,   151,   177,   177,   152,   226,   228,   411,
     177,   154,   179,   151,   151,   179,   158,   158,   151,   160,
     160,   157,   157,   157,   180,   154,   179,   217,   217,   180,
     157,   180,   478,   350,   351,   355,   355,   371,   478,   151,
     390,   456,   457,   154,   159,   154,   158,   159,   371,   478,
     223,   121,   194,   195,   156,   195,   156,   195,   157,   151,
     154,   179,   180,   180,   154,   154,   179,   179,   180,   180,
     180,   179,   179,   157,   180,   154,   411,   359,   359,   359,
     180,   180,   180,   224,   454,   151,   151,   333,   333,   333,
     340,   152,   199,   342,   343,   452,   463,   464,   465,   466,
     177,   158,   177,   340,   177,   385,   412,   417,   217,   302,
     158,   177,   346,   347,   346,   346,   366,   134,   363,   364,
     154,   154,   152,   226,   154,   224,   302,   146,   147,   148,
     168,   177,   247,   248,   226,   225,   177,   248,   154,   159,
     224,   153,   224,   225,   246,   177,   478,   154,   154,   154,
     228,   263,   264,   152,   217,   152,   185,   235,   201,   256,
     110,     1,   226,   411,   391,   179,   179,   353,   353,   157,
     359,   180,   180,   157,   157,   151,   351,   160,   478,   151,
     180,   154,   217,   189,   217,   478,   151,   157,   157,   194,
     194,   359,   154,   154,   359,   359,   154,   154,   157,   158,
     134,   358,   134,   157,   180,   180,   180,   154,   154,   154,
     157,   217,   177,   464,   465,   466,   302,   463,   158,   177,
     411,   411,   177,   154,   417,   411,   226,    77,    78,   160,
     238,   239,   240,   154,   224,    75,   226,   224,   153,   224,
      75,   177,   107,   153,   224,   225,   246,   153,   224,   226,
     245,   248,   248,   177,   224,   151,   160,   240,   226,   152,
     179,   177,   185,   154,   159,   154,   154,   158,   159,   254,
     258,   366,   408,   478,   478,   180,   157,   157,   160,   353,
     151,   151,   151,   157,   157,   180,   180,   180,   179,   180,
     154,   154,   154,   154,   154,   154,   463,   411,   341,     1,
     216,   236,   237,   409,     1,   159,     1,   179,   226,   238,
      75,   177,   154,   226,    75,   177,   168,   168,   226,   225,
     248,   248,   177,   107,   224,   168,   168,    75,   153,   224,
     153,   224,   225,   177,     1,   179,   179,   265,   300,   302,
     472,   159,   177,   156,   185,   270,   271,   272,   226,   201,
     191,    75,   109,   255,   257,   151,   151,   154,   353,   478,
     154,   154,   154,   361,   152,   411,   452,   455,   343,   134,
       1,   158,   159,   151,   275,   276,   282,   226,    75,   177,
     226,   224,   153,   153,   224,   153,   224,   153,   224,   225,
     153,   224,   153,   224,   226,   168,   168,   168,   168,   151,
     275,   265,   180,   152,   199,   408,   463,   183,   159,   104,
     152,   154,   159,   158,    75,   154,   226,   152,   226,   226,
     478,   151,   179,   216,   236,   239,   241,   242,   282,   226,
     168,   168,   168,   168,   153,   153,   224,   153,   224,   153,
     224,   241,   180,   177,   262,   302,   270,   157,   216,   177,
     270,   272,   226,   224,   110,   110,   151,   359,   226,   231,
     180,   239,   153,   153,   224,   153,   224,   153,   224,   180,
     262,   215,   154,   159,   185,   154,   154,   159,   154,   258,
      75,   253,   180,     1,   226,   151,   231,   151,   154,   228,
     185,   273,   152,   177,   273,   226,    75,   154,   228,   158,
     159,   216,   154,   226,   185,   183,   274,   154,   177,   154,
     158,   177,   183
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   178,   179,   180,   181,   181,   181,   181,   181,   182,
     182,   182,   182,   182,   182,   182,   183,   183,   184,   184,
     185,   186,   186,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   188,   188,
     189,   189,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   191,   191,   192,   192,
     193,   193,   194,   194,   195,   195,   195,   195,   195,   195,
     195,   196,   196,   196,   197,   197,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     199,   199,   199,   200,   200,   200,   200,   201,   201,   201,
     201,   201,   201,   201,   201,   201,   202,   202,   202,   202,
     203,   203,   204,   204,   205,   205,   205,   205,   206,   206,
     206,   207,   207,   207,   208,   208,   208,   208,   208,   209,
     209,   209,   210,   210,   211,   211,   212,   212,   213,   213,
     214,   214,   215,   215,   215,   216,   217,   217,   217,   218,
     218,   219,   219,   220,   220,   221,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   222,   222,   223,   223,
     223,   223,   224,   224,   225,   225,   226,   226,   226,   226,
     226,   226,   226,   226,   226,   226,   226,   226,   226,   227,
     227,   228,   228,   229,   229,   230,   230,   230,   230,   230,
     231,   231,   231,   232,   233,   233,   233,   233,   233,   233,
     233,   234,   234,   235,   235,   235,   235,   236,   236,   236,
     237,   237,   238,   238,   238,   238,   238,   239,   239,   240,
     241,   241,   242,   242,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   244,   244,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   246,   246,   246,   247,   247,   248,   248,
     248,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   250,   250,   251,   252,   253,   254,   254,   255,   255,
     256,   256,   257,   258,   258,   258,   258,   258,   258,   259,
     259,   260,   260,   260,   261,   261,   262,   262,   263,   263,
     263,   263,   264,   265,   265,   265,   265,   265,   266,   267,
     267,   268,   268,   268,   268,   268,   269,   269,   270,   270,
     271,   271,   272,   272,   273,   273,   273,   274,   274,   275,
     275,   276,   276,   277,   277,   278,   278,   279,   279,   280,
     280,   281,   281,   282,   282,   282,   283,   283,   284,   284,
     284,   284,   284,   285,   285,   285,   286,   286,   286,   287,
     287,   287,   287,   287,   288,   288,   289,   289,   290,   290,
     290,   291,   291,   291,   291,   291,   292,   292,   293,   293,
     293,   293,   294,   294,   294,   294,   294,   295,   295,   296,
     296,   296,   296,   297,   297,   297,   298,   298,   298,   299,
     299,   299,   300,   300,   300,   301,   301,   302,   302,   303,
     303,   304,   304,   304,   304,   304,   305,   306,   306,   306,
     307,   307,   308,   308,   308,   308,   308,   308,   308,   308,
     308,   309,   309,   309,   309,   309,   309,   309,   309,   309,
     309,   309,   309,   309,   309,   309,   309,   309,   309,   309,
     309,   309,   309,   309,   309,   309,   309,   309,   309,   310,
     310,   311,   312,   312,   313,   313,   313,   313,   313,   314,
     314,   315,   315,   315,   315,   316,   316,   316,   316,   316,
     316,   317,   317,   317,   317,   318,   319,   318,   318,   320,
     320,   320,   320,   321,   321,   321,   322,   322,   322,   322,
     323,   323,   323,   324,   324,   324,   324,   324,   324,   325,
     325,   325,   326,   326,   327,   327,   329,   328,   330,   328,
     331,   328,   332,   328,   328,   333,   333,   334,   334,   335,
     335,   336,   336,   336,   337,   337,   337,   337,   337,   337,
     337,   337,   338,   338,   339,   339,   339,   339,   339,   339,
     339,   339,   339,   339,   339,   340,   340,   340,   341,   341,
     341,   341,   342,   342,   342,   343,   344,   344,   345,   345,
     346,   346,   347,   348,   349,   348,   348,   348,   348,   350,
     348,   348,   348,   348,   348,   351,   351,   352,   352,   353,
     353,   353,   353,   354,   354,   355,   355,   355,   356,   356,
     356,   356,   356,   356,   356,   357,   357,   357,   357,   358,
     358,   359,   359,   359,   359,   360,   360,   360,   360,   361,
     361,   361,   361,   361,   362,   362,   362,   362,   362,   363,
     363,   364,   364,   365,   365,   366,   366,   366,   367,   367,
     367,   368,   368,   369,   369,   369,   369,   370,   370,   371,
     371,   371,   371,   371,   372,   372,   373,   373,   374,   374,
     374,   374,   374,   375,   375,   376,   376,   378,   377,   379,
     377,   377,   377,   380,   380,   380,   380,   381,   381,   381,
     381,   382,   382,   383,   383,   384,   384,   385,   385,   385,
     385,   386,   386,   386,   387,   387,   388,   388,   389,   389,
     389,   389,   390,   390,   391,   391,   392,   392,   392,   393,
     393,   394,   394,   395,   395,   396,   396,   397,   398,   399,
     399,   399,   399,   399,   399,   399,   399,   399,   399,   399,
     400,   399,   401,   399,   402,   399,   403,   399,   404,   399,
     405,   405,   405,   406,   406,   407,   407,   407,   407,   407,
     407,   407,   407,   407,   407,   408,   408,   408,   408,   409,
     410,   410,   411,   411,   412,   412,   413,   414,   414,   415,
     415,   415,   416,   416,   416,   416,   416,   416,   417,   417,
     418,   418,   418,   418,   419,   419,   419,   419,   420,   420,
     420,   420,   420,   420,   420,   421,   421,   421,   421,   422,
     422,   422,   423,   423,   423,   423,   423,   424,   424,   424,
     424,   425,   425,   425,   425,   425,   425,   426,   426,   426,
     427,   427,   427,   427,   427,   428,   428,   428,   428,   429,
     429,   429,   429,   429,   429,   430,   430,   431,   431,   431,
     431,   432,   432,   432,   432,   433,   433,   433,   433,   433,
     433,   433,   434,   434,   434,   434,   435,   435,   435,   436,
     436,   436,   436,   436,   437,   437,   437,   437,   438,   438,
     438,   438,   438,   438,   439,   439,   439,   439,   439,   440,
     440,   440,   441,   441,   441,   441,   442,   442,   442,   443,
     443,   443,   443,   443,   444,   444,   445,   445,   445,   446,
     446,   447,   447,   448,   448,   448,   449,   449,   449,   449,
     449,   450,   450,   450,   450,   451,   451,   451,   452,   452,
     452,   452,   452,   453,   453,   453,   453,   453,   453,   454,
     454,   455,   455,   455,   455,   456,   456,   457,   457,   457,
     457,   458,   458,   458,   458,   458,   459,   459,   459,   459,
     460,   460,   460,   461,   461,   461,   462,   462,   462,   462,
     462,   462,   463,   463,   463,   464,   464,   464,   464,   464,
     465,   465,   465,   465,   466,   466,   467,   467,   467,   468,
     468,   469,   469,   469,   469,   469,   469,   470,   470,   470,
     470,   470,   470,   470,   470,   470,   470,   471,   471,   471,
     471,   472,   472,   472,   473,   473,   474,   474,   474,   474,
     474,   474,   475,   475,   475,   475,   475,   475,   476,   476,
     476,   477,   477,   478,   478,   479,   479
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     3,     3,
       5,     6,     2,     2,     2,     2,     2,     2,     1,     3,
       3,     3,     1,     6,     4,     4,     4,     4,     4,     7,
       3,     3,     3,     3,     3,     2,     5,     3,     3,     3,
       5,     2,     2,     7,     8,     5,     0,     1,     1,     3,
       1,     1,     1,     3,     1,     2,     4,     3,     5,     3,
       5,     2,     2,     2,     0,     2,     1,     1,     1,     2,
       2,     2,     2,     2,     2,     4,     2,     4,     6,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     5,
       5,     4,     5,     5,     5,     4,     2,     2,     3,     3,
       1,     1,     1,     3,     1,     3,     3,     3,     1,     3,
       3,     1,     3,     3,     1,     3,     3,     3,     3,     1,
       3,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     5,     4,     1,     1,     3,     6,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     7,     1,     1,
       3,     3,     1,     3,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       4,     2,     6,     1,     2,     1,     2,     1,     2,     1,
       1,     2,     2,     2,     3,     5,    10,     7,     5,    10,
       7,     5,     7,     1,     1,     1,     2,     1,     3,     1,
       1,     3,     2,     3,     3,     2,     2,     1,     2,     2,
       0,     1,     2,     3,     4,     6,     5,     7,     6,     7,
       7,     8,     4,     6,     5,     7,     1,     3,     4,     5,
       4,     3,     5,     1,     2,     3,     3,     3,     5,     5,
       5,     5,     3,     5,     5,     5,     3,     4,     5,     5,
       5,     5,     7,     7,     7,     7,     7,     7,     7,     2,
       3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
       6,     3,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     4,     2,     3,     3,     2,     3,     2,     3,
       3,     6,     2,     2,     3,     3,     3,     3,     3,     3,
       5,     1,     1,     5,     5,     4,     0,     1,     4,     6,
       1,     3,     4,     3,     5,     3,     3,     6,     7,     3,
       5,     3,     3,     4,     8,     9,     0,     2,     1,     1,
       1,     1,     2,     1,     2,     2,     2,     1,     3,     1,
       1,     6,     8,    10,    12,    14,     0,     1,     0,     1,
       1,     3,     4,     7,     0,     1,     3,     1,     3,     0,
       1,     1,     2,     0,     1,     4,     5,     0,     1,     3,
       4,     1,     3,     2,     2,     1,     7,     5,     1,     1,
       1,     1,     1,     2,     3,     6,     3,     3,     4,     1,
       2,     2,     3,     8,     8,     8,     5,     9,     2,     2,
       5,     3,     5,     4,     3,     4,     4,     7,     2,     1,
       1,     1,     3,     3,     2,     4,     6,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     2,     3,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     5,     0,     1,     1,     2,     2,     3,     3,     1,
       3,     1,     2,     2,     2,     4,     4,     4,     4,     1,
       1,     1,     2,     2,     3,     1,     0,     3,     2,     1,
       2,     2,     3,     1,     2,     2,     1,     2,     2,     3,
       1,     2,     2,     1,     2,     3,     1,     2,     3,     1,
       3,     4,     1,     1,     1,     1,     0,     7,     0,     8,
       0,     8,     0,     8,     1,     0,     3,     3,     3,     1,
       1,     2,     1,     1,     1,     2,     1,     2,     1,     2,
       1,     2,     0,     2,     3,     4,     4,     4,     3,     2,
       2,     3,     3,     2,     1,     0,     1,     4,     1,     2,
       2,     2,     0,     1,     4,     1,     2,     3,     1,     2,
       0,     1,     2,     6,     0,     9,     8,     9,     8,     0,
      13,    11,    12,    11,     1,     0,     1,     3,     3,     3,
       2,     5,     5,     1,     1,     0,     2,     5,     0,     1,
       1,     1,     5,     5,     5,     1,     5,     5,     9,     1,
       5,     0,     1,     1,     5,     1,     1,     5,     5,     1,
       3,     3,     4,     1,     1,     1,     1,     2,     1,     3,
       3,     2,     3,     1,     3,     1,     1,     1,     1,     1,
       2,     1,     1,     0,     2,     2,     4,     1,     4,     0,
       1,     2,     3,     4,     2,     2,     1,     2,     2,     5,
       5,     7,     6,     1,     3,     0,     2,     0,     5,     0,
       5,     3,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     6,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,     5,
      10,     8,     1,     4,     2,     2,     1,     1,     5,     2,
       5,     0,     1,     3,     4,     0,     1,     0,     0,     1,
       1,     2,     2,     2,     2,     2,     2,     1,     2,     5,
       0,     6,     0,     8,     0,     7,     0,     7,     0,     8,
       1,     2,     3,     0,     5,     3,     4,     4,     4,     4,
       5,     5,     5,     5,     6,     1,     1,     1,     1,     3,
       0,     5,     0,     1,     1,     2,     6,     1,     3,     0,
       1,     4,     1,     1,     1,     1,     1,     1,     1,     3,
       2,     1,     2,     2,     2,     3,     4,     5,     2,     4,
       5,     4,     5,     3,     4,     8,     9,     3,     4,     2,
       1,     2,     6,     8,     9,     3,     4,     2,     3,     4,
       5,     4,     5,     4,     5,     3,     4,     1,     1,     1,
       4,     8,     9,     3,     4,     2,     3,     3,     4,     4,
       5,     4,     5,     3,     4,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     8,     9,     3,     4,     2,     1,     2,     6,
       8,     9,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     2,     4,     1,     2,     2,     2,
       3,     4,     2,     4,     4,     3,     6,     8,     3,     2,
       4,     1,     2,     2,     1,     1,     2,     3,     4,     2,
       4,     6,     8,     1,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     5,     8,     3,     2,     3,
       7,     5,     1,     1,     1,     3,     3,     3,     5,     1,
       1,     5,     5,     6,     6,     0,     1,     1,     3,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       5,     8,     3,     1,     2,     1,     2,     6,     5,     6,
       7,     7,     1,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     8,     3,     1,     1,     2,     1,
       1,     2,     3,     2,     3,     2,     3,     3,     2,     4,
       3,     2,     3,     2,     4,     3,     2,     6,     6,     6,
       7,     1,     2,     1,     1,     1,     2,     3,     2,     3,
       2,     3,     3,     4,     2,     3,     4,     2,     5,     6,
       7,     6,     6,     0,     1,     0,     2
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
#line 7749 "Parser/parser.cc"
    break;

  case 3:
#line 610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7755 "Parser/parser.cc"
    break;

  case 4:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7761 "Parser/parser.cc"
    break;

  case 5:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7767 "Parser/parser.cc"
    break;

  case 6:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7773 "Parser/parser.cc"
    break;

  case 7:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7779 "Parser/parser.cc"
    break;

  case 8:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7785 "Parser/parser.cc"
    break;

  case 19:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7791 "Parser/parser.cc"
    break;

  case 20:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7797 "Parser/parser.cc"
    break;

  case 21:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7803 "Parser/parser.cc"
    break;

  case 22:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7813 "Parser/parser.cc"
    break;

  case 23:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7819 "Parser/parser.cc"
    break;

  case 24:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7825 "Parser/parser.cc"
    break;

  case 25:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7831 "Parser/parser.cc"
    break;

  case 27:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7837 "Parser/parser.cc"
    break;

  case 28:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild( (yyvsp[-1].sn) ) ) ) ); }
#line 7843 "Parser/parser.cc"
    break;

  case 29:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7849 "Parser/parser.cc"
    break;

  case 30:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7855 "Parser/parser.cc"
    break;

  case 31:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7865 "Parser/parser.cc"
    break;

  case 32:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7871 "Parser/parser.cc"
    break;

  case 33:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7877 "Parser/parser.cc"
    break;

  case 34:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7883 "Parser/parser.cc"
    break;

  case 35:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7889 "Parser/parser.cc"
    break;

  case 36:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7895 "Parser/parser.cc"
    break;

  case 37:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7901 "Parser/parser.cc"
    break;

  case 39:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7912 "Parser/parser.cc"
    break;

  case 40:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild( (yyvsp[0].en) ) } } );
		}
#line 7921 "Parser/parser.cc"
    break;

  case 41:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild( (yyvsp[0].en) ) } } ); }
#line 7927 "Parser/parser.cc"
    break;

  case 43:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7933 "Parser/parser.cc"
    break;

  case 44:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7939 "Parser/parser.cc"
    break;

  case 45:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 46:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 47:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7961 "Parser/parser.cc"
    break;

  case 48:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7967 "Parser/parser.cc"
    break;

  case 49:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 50:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 51:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7986 "Parser/parser.cc"
    break;

  case 52:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7992 "Parser/parser.cc"
    break;

  case 53:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7998 "Parser/parser.cc"
    break;

  case 54:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 8004 "Parser/parser.cc"
    break;

  case 55:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 8010 "Parser/parser.cc"
    break;

  case 56:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8016 "Parser/parser.cc"
    break;

  case 57:
#line 768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 58:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 8028 "Parser/parser.cc"
    break;

  case 59:
#line 772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 8034 "Parser/parser.cc"
    break;

  case 60:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 61:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 8046 "Parser/parser.cc"
    break;

  case 62:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 8052 "Parser/parser.cc"
    break;

  case 63:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 8058 "Parser/parser.cc"
    break;

  case 64:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 8064 "Parser/parser.cc"
    break;

  case 65:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 8074 "Parser/parser.cc"
    break;

  case 66:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8080 "Parser/parser.cc"
    break;

  case 69:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8086 "Parser/parser.cc"
    break;

  case 70:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8092 "Parser/parser.cc"
    break;

  case 73:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8098 "Parser/parser.cc"
    break;

  case 75:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8104 "Parser/parser.cc"
    break;

  case 76:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8110 "Parser/parser.cc"
    break;

  case 77:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8116 "Parser/parser.cc"
    break;

  case 78:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8122 "Parser/parser.cc"
    break;

  case 79:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8128 "Parser/parser.cc"
    break;

  case 80:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8134 "Parser/parser.cc"
    break;

  case 81:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8140 "Parser/parser.cc"
    break;

  case 82:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8146 "Parser/parser.cc"
    break;

  case 83:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8154 "Parser/parser.cc"
    break;

  case 84:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8160 "Parser/parser.cc"
    break;

  case 85:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8169 "Parser/parser.cc"
    break;

  case 88:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8175 "Parser/parser.cc"
    break;

  case 89:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8181 "Parser/parser.cc"
    break;

  case 90:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			switch ( (yyvsp[-1].op) ) {
			  case OperKinds::AddressOf:
				(yyval.en) = new ExpressionNode( new AddressExpr( maybeMoveBuild( (yyvsp[0].en) ) ) );
				break;
			  case OperKinds::PointTo:
				(yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) );
				break;
			  case OperKinds::And:
				(yyval.en) = new ExpressionNode( new AddressExpr( new AddressExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ) );
				break;
			  default:
				assert( false );
			}
		}
#line 8201 "Parser/parser.cc"
    break;

  case 91:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 92:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8213 "Parser/parser.cc"
    break;

  case 93:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8219 "Parser/parser.cc"
    break;

  case 94:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8225 "Parser/parser.cc"
    break;

  case 95:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8231 "Parser/parser.cc"
    break;

  case 96:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8237 "Parser/parser.cc"
    break;

  case 97:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 98:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 99:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8258 "Parser/parser.cc"
    break;

  case 100:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8264 "Parser/parser.cc"
    break;

  case 101:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8270 "Parser/parser.cc"
    break;

  case 102:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8276 "Parser/parser.cc"
    break;

  case 103:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8282 "Parser/parser.cc"
    break;

  case 104:
#line 912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8288 "Parser/parser.cc"
    break;

  case 105:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8294 "Parser/parser.cc"
    break;

  case 106:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8300 "Parser/parser.cc"
    break;

  case 108:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 109:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 110:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 111:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 112:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8330 "Parser/parser.cc"
    break;

  case 113:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8336 "Parser/parser.cc"
    break;

  case 114:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8342 "Parser/parser.cc"
    break;

  case 115:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8348 "Parser/parser.cc"
    break;

  case 123:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 125:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 126:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 127:
#line 964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 129:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 130:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8384 "Parser/parser.cc"
    break;

  case 132:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8390 "Parser/parser.cc"
    break;

  case 133:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8396 "Parser/parser.cc"
    break;

  case 135:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8402 "Parser/parser.cc"
    break;

  case 136:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8408 "Parser/parser.cc"
    break;

  case 137:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8414 "Parser/parser.cc"
    break;

  case 138:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8420 "Parser/parser.cc"
    break;

  case 140:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8426 "Parser/parser.cc"
    break;

  case 141:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 143:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8438 "Parser/parser.cc"
    break;

  case 145:
#line 1012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8444 "Parser/parser.cc"
    break;

  case 147:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8450 "Parser/parser.cc"
    break;

  case 149:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8456 "Parser/parser.cc"
    break;

  case 151:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8462 "Parser/parser.cc"
    break;

  case 153:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8468 "Parser/parser.cc"
    break;

  case 154:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 157:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8486 "Parser/parser.cc"
    break;

  case 158:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8492 "Parser/parser.cc"
    break;

  case 159:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8498 "Parser/parser.cc"
    break;

  case 163:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8504 "Parser/parser.cc"
    break;

  case 164:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8510 "Parser/parser.cc"
    break;

  case 165:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8516 "Parser/parser.cc"
    break;

  case 166:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8522 "Parser/parser.cc"
    break;

  case 167:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8528 "Parser/parser.cc"
    break;

  case 168:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8534 "Parser/parser.cc"
    break;

  case 169:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8540 "Parser/parser.cc"
    break;

  case 170:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8546 "Parser/parser.cc"
    break;

  case 171:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8552 "Parser/parser.cc"
    break;

  case 172:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8558 "Parser/parser.cc"
    break;

  case 173:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8564 "Parser/parser.cc"
    break;

  case 174:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8570 "Parser/parser.cc"
    break;

  case 175:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8576 "Parser/parser.cc"
    break;

  case 176:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 177:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8588 "Parser/parser.cc"
    break;

  case 179:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8594 "Parser/parser.cc"
    break;

  case 180:
#line 1109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8600 "Parser/parser.cc"
    break;

  case 181:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8606 "Parser/parser.cc"
    break;

  case 183:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8612 "Parser/parser.cc"
    break;

  case 184:
#line 1122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8618 "Parser/parser.cc"
    break;

  case 196:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8624 "Parser/parser.cc"
    break;

  case 198:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 199:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 200:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8647 "Parser/parser.cc"
    break;

  case 201:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8653 "Parser/parser.cc"
    break;

  case 202:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8659 "Parser/parser.cc"
    break;

  case 204:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8665 "Parser/parser.cc"
    break;

  case 205:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8671 "Parser/parser.cc"
    break;

  case 206:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8677 "Parser/parser.cc"
    break;

  case 207:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8683 "Parser/parser.cc"
    break;

  case 208:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8689 "Parser/parser.cc"
    break;

  case 211:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8695 "Parser/parser.cc"
    break;

  case 212:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8701 "Parser/parser.cc"
    break;

  case 213:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8707 "Parser/parser.cc"
    break;

  case 214:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8713 "Parser/parser.cc"
    break;

  case 215:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8719 "Parser/parser.cc"
    break;

  case 216:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8733 "Parser/parser.cc"
    break;

  case 217:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8739 "Parser/parser.cc"
    break;

  case 218:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 219:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8754 "Parser/parser.cc"
    break;

  case 220:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8760 "Parser/parser.cc"
    break;

  case 221:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8766 "Parser/parser.cc"
    break;

  case 222:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8772 "Parser/parser.cc"
    break;

  case 223:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 224:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8784 "Parser/parser.cc"
    break;

  case 225:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8790 "Parser/parser.cc"
    break;

  case 226:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8796 "Parser/parser.cc"
    break;

  case 227:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8802 "Parser/parser.cc"
    break;

  case 228:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8808 "Parser/parser.cc"
    break;

  case 230:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8814 "Parser/parser.cc"
    break;

  case 231:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 232:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8826 "Parser/parser.cc"
    break;

  case 233:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8832 "Parser/parser.cc"
    break;

  case 234:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8838 "Parser/parser.cc"
    break;

  case 235:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8844 "Parser/parser.cc"
    break;

  case 236:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8850 "Parser/parser.cc"
    break;

  case 238:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8856 "Parser/parser.cc"
    break;

  case 239:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8862 "Parser/parser.cc"
    break;

  case 240:
#line 1288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8868 "Parser/parser.cc"
    break;

  case 242:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8874 "Parser/parser.cc"
    break;

  case 243:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8880 "Parser/parser.cc"
    break;

  case 244:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8886 "Parser/parser.cc"
    break;

  case 245:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8895 "Parser/parser.cc"
    break;

  case 246:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8901 "Parser/parser.cc"
    break;

  case 247:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8907 "Parser/parser.cc"
    break;

  case 248:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8913 "Parser/parser.cc"
    break;

  case 249:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8922 "Parser/parser.cc"
    break;

  case 250:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8928 "Parser/parser.cc"
    break;

  case 251:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8934 "Parser/parser.cc"
    break;

  case 252:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8940 "Parser/parser.cc"
    break;

  case 253:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8949 "Parser/parser.cc"
    break;

  case 254:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8955 "Parser/parser.cc"
    break;

  case 255:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8961 "Parser/parser.cc"
    break;

  case 257:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyvsp[-2].fctl)->init->set_last( (yyvsp[0].fctl)->init );
			if ( (yyvsp[-2].fctl)->condition ) {
				if ( (yyvsp[0].fctl)->condition ) {
					(yyvsp[-2].fctl)->condition->expr.reset( new LogicalExpr( (yyvsp[-2].fctl)->condition->expr.release(), (yyvsp[0].fctl)->condition->expr.release(), true ) );
				} // if
			} else (yyvsp[-2].fctl)->condition = (yyvsp[0].fctl)->condition;
			if ( (yyvsp[-2].fctl)->change ) {
				if ( (yyvsp[0].fctl)->change ) {
					(yyvsp[-2].fctl)->change->expr.reset( new CommaExpr( (yyvsp[-2].fctl)->change->expr.release(), (yyvsp[0].fctl)->change->expr.release() ) );
				} // if
			} else (yyvsp[-2].fctl)->change = (yyvsp[0].fctl)->change;
			(yyval.fctl) = (yyvsp[-2].fctl);
		}
#line 8980 "Parser/parser.cc"
    break;

  case 258:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8986 "Parser/parser.cc"
    break;

  case 259:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].en) ? new StatementNode( new ExprStmt( maybeMoveBuild( (yyvsp[-4].en) ) ) ) : nullptr;
			(yyval.fctl) = new ForCtrl( init, (yyvsp[-2].en), (yyvsp[0].en) );
		}
#line 8995 "Parser/parser.cc"
    break;

  case 260:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9001 "Parser/parser.cc"
    break;

  case 261:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[0].en), nullptr ); }
#line 9007 "Parser/parser.cc"
    break;

  case 262:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9013 "Parser/parser.cc"
    break;

  case 263:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9019 "Parser/parser.cc"
    break;

  case 264:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9025 "Parser/parser.cc"
    break;

  case 265:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9031 "Parser/parser.cc"
    break;

  case 266:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9040 "Parser/parser.cc"
    break;

  case 267:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9049 "Parser/parser.cc"
    break;

  case 268:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9055 "Parser/parser.cc"
    break;

  case 269:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9064 "Parser/parser.cc"
    break;

  case 270:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9073 "Parser/parser.cc"
    break;

  case 271:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9079 "Parser/parser.cc"
    break;

  case 272:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9085 "Parser/parser.cc"
    break;

  case 273:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9091 "Parser/parser.cc"
    break;

  case 274:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9097 "Parser/parser.cc"
    break;

  case 275:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9103 "Parser/parser.cc"
    break;

  case 276:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9109 "Parser/parser.cc"
    break;

  case 277:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9115 "Parser/parser.cc"
    break;

  case 278:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9121 "Parser/parser.cc"
    break;

  case 279:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9130 "Parser/parser.cc"
    break;

  case 280:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9140 "Parser/parser.cc"
    break;

  case 281:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9146 "Parser/parser.cc"
    break;

  case 282:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 283:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9161 "Parser/parser.cc"
    break;

  case 284:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9171 "Parser/parser.cc"
    break;

  case 285:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9177 "Parser/parser.cc"
    break;

  case 286:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9186 "Parser/parser.cc"
    break;

  case 287:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9196 "Parser/parser.cc"
    break;

  case 288:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9202 "Parser/parser.cc"
    break;

  case 289:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9208 "Parser/parser.cc"
    break;

  case 290:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9214 "Parser/parser.cc"
    break;

  case 291:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9220 "Parser/parser.cc"
    break;

  case 292:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9229 "Parser/parser.cc"
    break;

  case 293:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9239 "Parser/parser.cc"
    break;

  case 294:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9245 "Parser/parser.cc"
    break;

  case 295:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9254 "Parser/parser.cc"
    break;

  case 296:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9264 "Parser/parser.cc"
    break;

  case 297:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9270 "Parser/parser.cc"
    break;

  case 298:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9279 "Parser/parser.cc"
    break;

  case 299:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9289 "Parser/parser.cc"
    break;

  case 300:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9295 "Parser/parser.cc"
    break;

  case 301:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9304 "Parser/parser.cc"
    break;

  case 302:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9313 "Parser/parser.cc"
    break;

  case 303:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9319 "Parser/parser.cc"
    break;

  case 304:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9325 "Parser/parser.cc"
    break;

  case 305:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9331 "Parser/parser.cc"
    break;

  case 306:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9337 "Parser/parser.cc"
    break;

  case 307:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9343 "Parser/parser.cc"
    break;

  case 309:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9349 "Parser/parser.cc"
    break;

  case 310:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9355 "Parser/parser.cc"
    break;

  case 311:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9361 "Parser/parser.cc"
    break;

  case 312:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9367 "Parser/parser.cc"
    break;

  case 313:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9373 "Parser/parser.cc"
    break;

  case 314:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9379 "Parser/parser.cc"
    break;

  case 315:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 316:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 317:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9397 "Parser/parser.cc"
    break;

  case 318:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 319:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 320:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 321:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9421 "Parser/parser.cc"
    break;

  case 322:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 323:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 324:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 325:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 326:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 327:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 328:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 329:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9469 "Parser/parser.cc"
    break;

  case 330:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9475 "Parser/parser.cc"
    break;

  case 333:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9481 "Parser/parser.cc"
    break;

  case 334:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].en) ) { SemanticError( yylloc, "mutex argument list cannot be empty." ); (yyval.sn) = nullptr; }
			(yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 9490 "Parser/parser.cc"
    break;

  case 335:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9496 "Parser/parser.cc"
    break;

  case 336:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9502 "Parser/parser.cc"
    break;

  case 338:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9508 "Parser/parser.cc"
    break;

  case 339:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9514 "Parser/parser.cc"
    break;

  case 341:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9520 "Parser/parser.cc"
    break;

  case 342:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9526 "Parser/parser.cc"
    break;

  case 343:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9532 "Parser/parser.cc"
    break;

  case 344:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9538 "Parser/parser.cc"
    break;

  case 345:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9544 "Parser/parser.cc"
    break;

  case 346:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9550 "Parser/parser.cc"
    break;

  case 347:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9556 "Parser/parser.cc"
    break;

  case 348:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9562 "Parser/parser.cc"
    break;

  case 349:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9568 "Parser/parser.cc"
    break;

  case 350:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9574 "Parser/parser.cc"
    break;

  case 351:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), nullptr ) ); }
#line 9580 "Parser/parser.cc"
    break;

  case 352:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), nullptr, (yyvsp[0].sn) ) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 353:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 354:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 355:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 356:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9610 "Parser/parser.cc"
    break;

  case 357:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9616 "Parser/parser.cc"
    break;

  case 358:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9622 "Parser/parser.cc"
    break;

  case 359:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9628 "Parser/parser.cc"
    break;

  case 360:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9634 "Parser/parser.cc"
    break;

  case 361:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9640 "Parser/parser.cc"
    break;

  case 362:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9646 "Parser/parser.cc"
    break;

  case 364:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9652 "Parser/parser.cc"
    break;

  case 365:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9658 "Parser/parser.cc"
    break;

  case 366:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9664 "Parser/parser.cc"
    break;

  case 371:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), nullptr ) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 372:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9676 "Parser/parser.cc"
    break;

  case 373:
#line 1732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9682 "Parser/parser.cc"
    break;

  case 374:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9688 "Parser/parser.cc"
    break;

  case 375:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), nullptr, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9694 "Parser/parser.cc"
    break;

  case 376:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9700 "Parser/parser.cc"
    break;

  case 377:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9706 "Parser/parser.cc"
    break;

  case 378:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9712 "Parser/parser.cc"
    break;

  case 381:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9718 "Parser/parser.cc"
    break;

  case 382:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild( (yyvsp[-1].en) ) ) ); }
#line 9724 "Parser/parser.cc"
    break;

  case 383:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild( (yyvsp[-1].en) ) ) ); }
#line 9730 "Parser/parser.cc"
    break;

  case 384:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9736 "Parser/parser.cc"
    break;

  case 385:
#line 1769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9742 "Parser/parser.cc"
    break;

  case 386:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9748 "Parser/parser.cc"
    break;

  case 387:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9757 "Parser/parser.cc"
    break;

  case 388:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9766 "Parser/parser.cc"
    break;

  case 389:
#line 1791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9772 "Parser/parser.cc"
    break;

  case 392:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 393:
#line 1803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9784 "Parser/parser.cc"
    break;

  case 395:
#line 1809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9790 "Parser/parser.cc"
    break;

  case 396:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9796 "Parser/parser.cc"
    break;

  case 403:
#line 1831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9807 "Parser/parser.cc"
    break;

  case 406:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9813 "Parser/parser.cc"
    break;

  case 407:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9819 "Parser/parser.cc"
    break;

  case 411:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9825 "Parser/parser.cc"
    break;

  case 413:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9831 "Parser/parser.cc"
    break;

  case 414:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9837 "Parser/parser.cc"
    break;

  case 415:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9843 "Parser/parser.cc"
    break;

  case 416:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9849 "Parser/parser.cc"
    break;

  case 417:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9855 "Parser/parser.cc"
    break;

  case 418:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9861 "Parser/parser.cc"
    break;

  case 420:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9867 "Parser/parser.cc"
    break;

  case 421:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9873 "Parser/parser.cc"
    break;

  case 422:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9879 "Parser/parser.cc"
    break;

  case 423:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9890 "Parser/parser.cc"
    break;

  case 424:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9896 "Parser/parser.cc"
    break;

  case 425:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 9902 "Parser/parser.cc"
    break;

  case 426:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9908 "Parser/parser.cc"
    break;

  case 427:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9914 "Parser/parser.cc"
    break;

  case 428:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9923 "Parser/parser.cc"
    break;

  case 429:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9932 "Parser/parser.cc"
    break;

  case 430:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9941 "Parser/parser.cc"
    break;

  case 431:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			if ( TypedefForall( (yyvsp[-1].decl) ) ) (yyval.decl) = nullptr;
			else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();		// watchout frees $2 and $3
		}
#line 9951 "Parser/parser.cc"
    break;

  case 432:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9960 "Parser/parser.cc"
    break;

  case 433:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			if ( TypedefForall( (yyvsp[-3].decl) ) ) (yyval.decl) = nullptr;
			else (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9970 "Parser/parser.cc"
    break;

  case 434:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			if ( TypedefForall( (yyvsp[-2].decl) ) ) (yyval.decl) = nullptr;
			else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9980 "Parser/parser.cc"
    break;

  case 435:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			if ( TypedefForall( (yyvsp[-1].decl) ) ) (yyval.decl) = nullptr;
			else (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addType( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9990 "Parser/parser.cc"
    break;

  case 436:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9998 "Parser/parser.cc"
    break;

  case 437:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10006 "Parser/parser.cc"
    break;

  case 438:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10012 "Parser/parser.cc"
    break;

  case 441:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10027 "Parser/parser.cc"
    break;

  case 442:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10033 "Parser/parser.cc"
    break;

  case 443:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10039 "Parser/parser.cc"
    break;

  case 444:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10045 "Parser/parser.cc"
    break;

  case 445:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10051 "Parser/parser.cc"
    break;

  case 446:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 10057 "Parser/parser.cc"
    break;

  case 452:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Missing ';' after end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration" ) );
			(yyval.decl) = nullptr;
		}
#line 10068 "Parser/parser.cc"
    break;

  case 465:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10074 "Parser/parser.cc"
    break;

  case 468:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10080 "Parser/parser.cc"
    break;

  case 471:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10086 "Parser/parser.cc"
    break;

  case 472:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10092 "Parser/parser.cc"
    break;

  case 473:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10098 "Parser/parser.cc"
    break;

  case 474:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10104 "Parser/parser.cc"
    break;

  case 475:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10110 "Parser/parser.cc"
    break;

  case 476:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10116 "Parser/parser.cc"
    break;

  case 478:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10122 "Parser/parser.cc"
    break;

  case 479:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10128 "Parser/parser.cc"
    break;

  case 481:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10134 "Parser/parser.cc"
    break;

  case 482:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10140 "Parser/parser.cc"
    break;

  case 483:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10146 "Parser/parser.cc"
    break;

  case 484:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10152 "Parser/parser.cc"
    break;

  case 485:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10158 "Parser/parser.cc"
    break;

  case 486:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10164 "Parser/parser.cc"
    break;

  case 487:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10170 "Parser/parser.cc"
    break;

  case 488:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10176 "Parser/parser.cc"
    break;

  case 489:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10182 "Parser/parser.cc"
    break;

  case 490:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10188 "Parser/parser.cc"
    break;

  case 491:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10194 "Parser/parser.cc"
    break;

  case 492:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10200 "Parser/parser.cc"
    break;

  case 493:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10206 "Parser/parser.cc"
    break;

  case 494:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10212 "Parser/parser.cc"
    break;

  case 495:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10218 "Parser/parser.cc"
    break;

  case 496:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10224 "Parser/parser.cc"
    break;

  case 497:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10230 "Parser/parser.cc"
    break;

  case 498:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10236 "Parser/parser.cc"
    break;

  case 499:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10242 "Parser/parser.cc"
    break;

  case 500:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10248 "Parser/parser.cc"
    break;

  case 501:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10254 "Parser/parser.cc"
    break;

  case 502:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10260 "Parser/parser.cc"
    break;

  case 503:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10266 "Parser/parser.cc"
    break;

  case 504:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10272 "Parser/parser.cc"
    break;

  case 505:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10278 "Parser/parser.cc"
    break;

  case 506:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10284 "Parser/parser.cc"
    break;

  case 507:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10290 "Parser/parser.cc"
    break;

  case 508:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10296 "Parser/parser.cc"
    break;

  case 509:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10302 "Parser/parser.cc"
    break;

  case 510:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10308 "Parser/parser.cc"
    break;

  case 511:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10314 "Parser/parser.cc"
    break;

  case 512:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10320 "Parser/parser.cc"
    break;

  case 513:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10326 "Parser/parser.cc"
    break;

  case 514:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10332 "Parser/parser.cc"
    break;

  case 515:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10338 "Parser/parser.cc"
    break;

  case 516:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10344 "Parser/parser.cc"
    break;

  case 517:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10350 "Parser/parser.cc"
    break;

  case 519:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10356 "Parser/parser.cc"
    break;

  case 521:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10362 "Parser/parser.cc"
    break;

  case 522:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10368 "Parser/parser.cc"
    break;

  case 523:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10374 "Parser/parser.cc"
    break;

  case 525:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 526:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10386 "Parser/parser.cc"
    break;

  case 527:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10392 "Parser/parser.cc"
    break;

  case 528:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 530:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10404 "Parser/parser.cc"
    break;

  case 532:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10410 "Parser/parser.cc"
    break;

  case 533:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10416 "Parser/parser.cc"
    break;

  case 534:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10422 "Parser/parser.cc"
    break;

  case 535:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10428 "Parser/parser.cc"
    break;

  case 536:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10434 "Parser/parser.cc"
    break;

  case 537:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10440 "Parser/parser.cc"
    break;

  case 538:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10446 "Parser/parser.cc"
    break;

  case 539:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10452 "Parser/parser.cc"
    break;

  case 540:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10458 "Parser/parser.cc"
    break;

  case 541:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10469 "Parser/parser.cc"
    break;

  case 542:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 543:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 544:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10487 "Parser/parser.cc"
    break;

  case 545:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10498 "Parser/parser.cc"
    break;

  case 546:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10504 "Parser/parser.cc"
    break;

  case 547:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 548:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10519 "Parser/parser.cc"
    break;

  case 550:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10525 "Parser/parser.cc"
    break;

  case 551:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10531 "Parser/parser.cc"
    break;

  case 552:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10537 "Parser/parser.cc"
    break;

  case 554:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10543 "Parser/parser.cc"
    break;

  case 555:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10549 "Parser/parser.cc"
    break;

  case 557:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10555 "Parser/parser.cc"
    break;

  case 558:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10561 "Parser/parser.cc"
    break;

  case 559:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10567 "Parser/parser.cc"
    break;

  case 561:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10573 "Parser/parser.cc"
    break;

  case 562:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10579 "Parser/parser.cc"
    break;

  case 563:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10585 "Parser/parser.cc"
    break;

  case 564:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10591 "Parser/parser.cc"
    break;

  case 565:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10597 "Parser/parser.cc"
    break;

  case 567:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10603 "Parser/parser.cc"
    break;

  case 568:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10609 "Parser/parser.cc"
    break;

  case 569:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10615 "Parser/parser.cc"
    break;

  case 570:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10621 "Parser/parser.cc"
    break;

  case 571:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 572:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10638 "Parser/parser.cc"
    break;

  case 576:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10644 "Parser/parser.cc"
    break;

  case 577:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10650 "Parser/parser.cc"
    break;

  case 578:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10659 "Parser/parser.cc"
    break;

  case 579:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10667 "Parser/parser.cc"
    break;

  case 580:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10676 "Parser/parser.cc"
    break;

  case 581:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10685 "Parser/parser.cc"
    break;

  case 582:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10694 "Parser/parser.cc"
    break;

  case 583:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10703 "Parser/parser.cc"
    break;

  case 585:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10709 "Parser/parser.cc"
    break;

  case 586:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10715 "Parser/parser.cc"
    break;

  case 587:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10725 "Parser/parser.cc"
    break;

  case 588:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10744 "Parser/parser.cc"
    break;

  case 591:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10750 "Parser/parser.cc"
    break;

  case 592:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10756 "Parser/parser.cc"
    break;

  case 593:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10762 "Parser/parser.cc"
    break;

  case 594:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10768 "Parser/parser.cc"
    break;

  case 595:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10774 "Parser/parser.cc"
    break;

  case 596:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10780 "Parser/parser.cc"
    break;

  case 597:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10786 "Parser/parser.cc"
    break;

  case 598:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10792 "Parser/parser.cc"
    break;

  case 599:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10798 "Parser/parser.cc"
    break;

  case 600:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10804 "Parser/parser.cc"
    break;

  case 601:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10810 "Parser/parser.cc"
    break;

  case 602:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10816 "Parser/parser.cc"
    break;

  case 603:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10822 "Parser/parser.cc"
    break;

  case 604:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10835 "Parser/parser.cc"
    break;

  case 605:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 606:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10847 "Parser/parser.cc"
    break;

  case 607:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10860 "Parser/parser.cc"
    break;

  case 608:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10866 "Parser/parser.cc"
    break;

  case 611:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10872 "Parser/parser.cc"
    break;

  case 612:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10878 "Parser/parser.cc"
    break;

  case 615:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10884 "Parser/parser.cc"
    break;

  case 617:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 618:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 619:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 620:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 621:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 622:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10920 "Parser/parser.cc"
    break;

  case 624:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 626:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 627:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 629:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 630:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10950 "Parser/parser.cc"
    break;

  case 632:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10956 "Parser/parser.cc"
    break;

  case 633:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 634:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 635:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 636:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 637:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10991 "Parser/parser.cc"
    break;

  case 638:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10999 "Parser/parser.cc"
    break;

  case 639:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 11008 "Parser/parser.cc"
    break;

  case 640:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11016 "Parser/parser.cc"
    break;

  case 641:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11024 "Parser/parser.cc"
    break;

  case 642:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11032 "Parser/parser.cc"
    break;

  case 643:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11040 "Parser/parser.cc"
    break;

  case 645:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11046 "Parser/parser.cc"
    break;

  case 646:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 11052 "Parser/parser.cc"
    break;

  case 647:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 648:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 649:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 650:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11076 "Parser/parser.cc"
    break;

  case 651:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 652:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 654:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11094 "Parser/parser.cc"
    break;

  case 655:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11100 "Parser/parser.cc"
    break;

  case 656:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 657:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11112 "Parser/parser.cc"
    break;

  case 658:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11118 "Parser/parser.cc"
    break;

  case 659:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11124 "Parser/parser.cc"
    break;

  case 662:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 663:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11136 "Parser/parser.cc"
    break;

  case 664:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11142 "Parser/parser.cc"
    break;

  case 666:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 667:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11154 "Parser/parser.cc"
    break;

  case 668:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 670:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 671:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11172 "Parser/parser.cc"
    break;

  case 672:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11178 "Parser/parser.cc"
    break;

  case 674:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11184 "Parser/parser.cc"
    break;

  case 677:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 678:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 680:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 681:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 682:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11214 "Parser/parser.cc"
    break;

  case 687:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11220 "Parser/parser.cc"
    break;

  case 689:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11226 "Parser/parser.cc"
    break;

  case 690:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11232 "Parser/parser.cc"
    break;

  case 691:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11238 "Parser/parser.cc"
    break;

  case 692:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11244 "Parser/parser.cc"
    break;

  case 693:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11250 "Parser/parser.cc"
    break;

  case 694:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 700:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 703:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11268 "Parser/parser.cc"
    break;

  case 704:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11274 "Parser/parser.cc"
    break;

  case 705:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11280 "Parser/parser.cc"
    break;

  case 706:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11286 "Parser/parser.cc"
    break;

  case 707:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 708:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11298 "Parser/parser.cc"
    break;

  case 709:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11304 "Parser/parser.cc"
    break;

  case 711:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 712:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 713:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11322 "Parser/parser.cc"
    break;

  case 715:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 717:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11334 "Parser/parser.cc"
    break;

  case 718:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 719:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11346 "Parser/parser.cc"
    break;

  case 720:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11352 "Parser/parser.cc"
    break;

  case 721:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild( (yyvsp[-4].en) ), maybeMoveBuild( (yyvsp[-2].en) ) ) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 722:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11364 "Parser/parser.cc"
    break;

  case 724:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 725:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11376 "Parser/parser.cc"
    break;

  case 726:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11382 "Parser/parser.cc"
    break;

  case 727:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11393 "Parser/parser.cc"
    break;

  case 728:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11399 "Parser/parser.cc"
    break;

  case 729:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11405 "Parser/parser.cc"
    break;

  case 730:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11411 "Parser/parser.cc"
    break;

  case 731:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11420 "Parser/parser.cc"
    break;

  case 732:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 733:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11432 "Parser/parser.cc"
    break;

  case 734:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11438 "Parser/parser.cc"
    break;

  case 735:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11444 "Parser/parser.cc"
    break;

  case 736:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11450 "Parser/parser.cc"
    break;

  case 737:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11456 "Parser/parser.cc"
    break;

  case 738:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11462 "Parser/parser.cc"
    break;

  case 739:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11468 "Parser/parser.cc"
    break;

  case 740:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11474 "Parser/parser.cc"
    break;

  case 741:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11480 "Parser/parser.cc"
    break;

  case 744:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 745:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 746:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11498 "Parser/parser.cc"
    break;

  case 747:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11504 "Parser/parser.cc"
    break;

  case 749:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11510 "Parser/parser.cc"
    break;

  case 750:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11516 "Parser/parser.cc"
    break;

  case 751:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11522 "Parser/parser.cc"
    break;

  case 752:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11528 "Parser/parser.cc"
    break;

  case 753:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 754:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 755:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 756:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11555 "Parser/parser.cc"
    break;

  case 757:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11564 "Parser/parser.cc"
    break;

  case 758:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11573 "Parser/parser.cc"
    break;

  case 759:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11579 "Parser/parser.cc"
    break;

  case 760:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11588 "Parser/parser.cc"
    break;

  case 761:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 763:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 768:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 769:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11612 "Parser/parser.cc"
    break;

  case 770:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 772:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11624 "Parser/parser.cc"
    break;

  case 773:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11630 "Parser/parser.cc"
    break;

  case 774:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11636 "Parser/parser.cc"
    break;

  case 775:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11642 "Parser/parser.cc"
    break;

  case 777:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11648 "Parser/parser.cc"
    break;

  case 778:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11654 "Parser/parser.cc"
    break;

  case 779:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11660 "Parser/parser.cc"
    break;

  case 780:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( (yyvsp[0].decl)->linkage == LinkageSpec::Cforall && ! (yyvsp[0].decl)->storageClasses.is_static && (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->kind == TypeData::AggregateInst ) {
				if ( (yyvsp[0].decl)->type->aggInst.aggregate->kind == TypeData::Enum && (yyvsp[0].decl)->type->aggInst.aggregate->enumeration.anon ) {
					SemanticError( yylloc, "extern anonymous enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
				} else if ( (yyvsp[0].decl)->type->aggInst.aggregate->aggregate.anon ) { // handles struct or union
					SemanticError( yylloc, "extern anonymous struct/union is currently unimplemented." ); (yyval.decl) = nullptr;
				}
			}
		}
#line 11677 "Parser/parser.cc"
    break;

  case 781:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11683 "Parser/parser.cc"
    break;

  case 782:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11689 "Parser/parser.cc"
    break;

  case 783:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11695 "Parser/parser.cc"
    break;

  case 784:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11701 "Parser/parser.cc"
    break;

  case 785:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11707 "Parser/parser.cc"
    break;

  case 786:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11713 "Parser/parser.cc"
    break;

  case 788:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11722 "Parser/parser.cc"
    break;

  case 789:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), nullptr ) ) ); }
#line 11728 "Parser/parser.cc"
    break;

  case 790:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11737 "Parser/parser.cc"
    break;

  case 791:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11747 "Parser/parser.cc"
    break;

  case 792:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11756 "Parser/parser.cc"
    break;

  case 793:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11766 "Parser/parser.cc"
    break;

  case 794:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11775 "Parser/parser.cc"
    break;

  case 795:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11785 "Parser/parser.cc"
    break;

  case 796:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11794 "Parser/parser.cc"
    break;

  case 797:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11804 "Parser/parser.cc"
    break;

  case 798:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11813 "Parser/parser.cc"
    break;

  case 799:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11823 "Parser/parser.cc"
    break;

  case 801:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11829 "Parser/parser.cc"
    break;

  case 802:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 803:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11841 "Parser/parser.cc"
    break;

  case 804:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11853 "Parser/parser.cc"
    break;

  case 805:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11864 "Parser/parser.cc"
    break;

  case 806:
#line 3221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11873 "Parser/parser.cc"
    break;

  case 807:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11882 "Parser/parser.cc"
    break;

  case 808:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11888 "Parser/parser.cc"
    break;

  case 809:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11894 "Parser/parser.cc"
    break;

  case 810:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11900 "Parser/parser.cc"
    break;

  case 811:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11909 "Parser/parser.cc"
    break;

  case 812:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 813:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 814:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 819:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 820:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11939 "Parser/parser.cc"
    break;

  case 821:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11949 "Parser/parser.cc"
    break;

  case 822:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11955 "Parser/parser.cc"
    break;

  case 825:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11961 "Parser/parser.cc"
    break;

  case 826:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11967 "Parser/parser.cc"
    break;

  case 828:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11973 "Parser/parser.cc"
    break;

  case 829:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11979 "Parser/parser.cc"
    break;

  case 830:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11985 "Parser/parser.cc"
    break;

  case 831:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11991 "Parser/parser.cc"
    break;

  case 836:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11997 "Parser/parser.cc"
    break;

  case 837:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12003 "Parser/parser.cc"
    break;

  case 838:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12009 "Parser/parser.cc"
    break;

  case 839:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12015 "Parser/parser.cc"
    break;

  case 840:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12021 "Parser/parser.cc"
    break;

  case 842:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12027 "Parser/parser.cc"
    break;

  case 843:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 844:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12039 "Parser/parser.cc"
    break;

  case 845:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12045 "Parser/parser.cc"
    break;

  case 846:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12051 "Parser/parser.cc"
    break;

  case 847:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12057 "Parser/parser.cc"
    break;

  case 848:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12063 "Parser/parser.cc"
    break;

  case 849:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12069 "Parser/parser.cc"
    break;

  case 850:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12075 "Parser/parser.cc"
    break;

  case 851:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12081 "Parser/parser.cc"
    break;

  case 852:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12087 "Parser/parser.cc"
    break;

  case 853:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12093 "Parser/parser.cc"
    break;

  case 854:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12099 "Parser/parser.cc"
    break;

  case 855:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12105 "Parser/parser.cc"
    break;

  case 856:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12111 "Parser/parser.cc"
    break;

  case 857:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12117 "Parser/parser.cc"
    break;

  case 858:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12123 "Parser/parser.cc"
    break;

  case 859:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12129 "Parser/parser.cc"
    break;

  case 861:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12135 "Parser/parser.cc"
    break;

  case 862:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12141 "Parser/parser.cc"
    break;

  case 863:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12147 "Parser/parser.cc"
    break;

  case 864:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12153 "Parser/parser.cc"
    break;

  case 865:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12159 "Parser/parser.cc"
    break;

  case 866:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12165 "Parser/parser.cc"
    break;

  case 867:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12171 "Parser/parser.cc"
    break;

  case 868:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12177 "Parser/parser.cc"
    break;

  case 869:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12183 "Parser/parser.cc"
    break;

  case 870:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12189 "Parser/parser.cc"
    break;

  case 871:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12195 "Parser/parser.cc"
    break;

  case 872:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12201 "Parser/parser.cc"
    break;

  case 873:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12207 "Parser/parser.cc"
    break;

  case 874:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12213 "Parser/parser.cc"
    break;

  case 875:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12219 "Parser/parser.cc"
    break;

  case 876:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12225 "Parser/parser.cc"
    break;

  case 880:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12231 "Parser/parser.cc"
    break;

  case 881:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12237 "Parser/parser.cc"
    break;

  case 882:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12243 "Parser/parser.cc"
    break;

  case 883:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12249 "Parser/parser.cc"
    break;

  case 884:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12255 "Parser/parser.cc"
    break;

  case 885:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12261 "Parser/parser.cc"
    break;

  case 886:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12267 "Parser/parser.cc"
    break;

  case 887:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12273 "Parser/parser.cc"
    break;

  case 888:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12279 "Parser/parser.cc"
    break;

  case 889:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12285 "Parser/parser.cc"
    break;

  case 890:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12291 "Parser/parser.cc"
    break;

  case 891:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12297 "Parser/parser.cc"
    break;

  case 892:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12303 "Parser/parser.cc"
    break;

  case 893:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12309 "Parser/parser.cc"
    break;

  case 894:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12315 "Parser/parser.cc"
    break;

  case 895:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12324 "Parser/parser.cc"
    break;

  case 896:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12330 "Parser/parser.cc"
    break;

  case 897:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12336 "Parser/parser.cc"
    break;

  case 899:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12342 "Parser/parser.cc"
    break;

  case 900:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12348 "Parser/parser.cc"
    break;

  case 901:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12354 "Parser/parser.cc"
    break;

  case 902:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12360 "Parser/parser.cc"
    break;

  case 903:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12366 "Parser/parser.cc"
    break;

  case 904:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12372 "Parser/parser.cc"
    break;

  case 905:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12378 "Parser/parser.cc"
    break;

  case 906:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12384 "Parser/parser.cc"
    break;

  case 907:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12390 "Parser/parser.cc"
    break;

  case 908:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12396 "Parser/parser.cc"
    break;

  case 909:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12402 "Parser/parser.cc"
    break;

  case 910:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12408 "Parser/parser.cc"
    break;

  case 911:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12414 "Parser/parser.cc"
    break;

  case 912:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12420 "Parser/parser.cc"
    break;

  case 913:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12426 "Parser/parser.cc"
    break;

  case 914:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12432 "Parser/parser.cc"
    break;

  case 915:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12438 "Parser/parser.cc"
    break;

  case 916:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12444 "Parser/parser.cc"
    break;

  case 918:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12450 "Parser/parser.cc"
    break;

  case 919:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12456 "Parser/parser.cc"
    break;

  case 920:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12462 "Parser/parser.cc"
    break;

  case 921:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12468 "Parser/parser.cc"
    break;

  case 922:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12474 "Parser/parser.cc"
    break;

  case 923:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12480 "Parser/parser.cc"
    break;

  case 924:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12486 "Parser/parser.cc"
    break;

  case 925:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12492 "Parser/parser.cc"
    break;

  case 926:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12498 "Parser/parser.cc"
    break;

  case 927:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12504 "Parser/parser.cc"
    break;

  case 928:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12510 "Parser/parser.cc"
    break;

  case 929:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12516 "Parser/parser.cc"
    break;

  case 930:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12522 "Parser/parser.cc"
    break;

  case 931:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12528 "Parser/parser.cc"
    break;

  case 932:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12534 "Parser/parser.cc"
    break;

  case 933:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12540 "Parser/parser.cc"
    break;

  case 934:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12546 "Parser/parser.cc"
    break;

  case 935:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12552 "Parser/parser.cc"
    break;

  case 937:
#line 3641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12558 "Parser/parser.cc"
    break;

  case 938:
#line 3643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12564 "Parser/parser.cc"
    break;

  case 939:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12570 "Parser/parser.cc"
    break;

  case 940:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12576 "Parser/parser.cc"
    break;

  case 941:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12582 "Parser/parser.cc"
    break;

  case 942:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12588 "Parser/parser.cc"
    break;

  case 943:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12594 "Parser/parser.cc"
    break;

  case 944:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12600 "Parser/parser.cc"
    break;

  case 945:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12606 "Parser/parser.cc"
    break;

  case 946:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12612 "Parser/parser.cc"
    break;

  case 947:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12618 "Parser/parser.cc"
    break;

  case 948:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12624 "Parser/parser.cc"
    break;

  case 949:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12630 "Parser/parser.cc"
    break;

  case 950:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12636 "Parser/parser.cc"
    break;

  case 952:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12642 "Parser/parser.cc"
    break;

  case 953:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12648 "Parser/parser.cc"
    break;

  case 954:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12654 "Parser/parser.cc"
    break;

  case 955:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12660 "Parser/parser.cc"
    break;

  case 956:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12666 "Parser/parser.cc"
    break;

  case 957:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12672 "Parser/parser.cc"
    break;

  case 958:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12678 "Parser/parser.cc"
    break;

  case 959:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12684 "Parser/parser.cc"
    break;

  case 960:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12690 "Parser/parser.cc"
    break;

  case 961:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12696 "Parser/parser.cc"
    break;

  case 962:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12702 "Parser/parser.cc"
    break;

  case 964:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12708 "Parser/parser.cc"
    break;

  case 965:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12714 "Parser/parser.cc"
    break;

  case 966:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12720 "Parser/parser.cc"
    break;

  case 967:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12726 "Parser/parser.cc"
    break;

  case 968:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12732 "Parser/parser.cc"
    break;

  case 969:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12738 "Parser/parser.cc"
    break;

  case 970:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12744 "Parser/parser.cc"
    break;

  case 972:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12750 "Parser/parser.cc"
    break;

  case 973:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12756 "Parser/parser.cc"
    break;

  case 974:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12762 "Parser/parser.cc"
    break;

  case 975:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12768 "Parser/parser.cc"
    break;

  case 976:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12774 "Parser/parser.cc"
    break;

  case 977:
#line 3775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12780 "Parser/parser.cc"
    break;

  case 978:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12786 "Parser/parser.cc"
    break;

  case 979:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 12792 "Parser/parser.cc"
    break;

  case 980:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), nullptr, false ) ); }
#line 12798 "Parser/parser.cc"
    break;

  case 981:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12804 "Parser/parser.cc"
    break;

  case 983:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12810 "Parser/parser.cc"
    break;

  case 984:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12816 "Parser/parser.cc"
    break;

  case 986:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12822 "Parser/parser.cc"
    break;

  case 987:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12828 "Parser/parser.cc"
    break;

  case 989:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 12834 "Parser/parser.cc"
    break;

  case 990:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 12840 "Parser/parser.cc"
    break;

  case 991:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ); }
#line 12846 "Parser/parser.cc"
    break;

  case 992:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12852 "Parser/parser.cc"
    break;

  case 993:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ) ); }
#line 12858 "Parser/parser.cc"
    break;

  case 994:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12864 "Parser/parser.cc"
    break;

  case 995:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12870 "Parser/parser.cc"
    break;

  case 998:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12876 "Parser/parser.cc"
    break;

  case 999:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12882 "Parser/parser.cc"
    break;

  case 1000:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12888 "Parser/parser.cc"
    break;

  case 1001:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12894 "Parser/parser.cc"
    break;

  case 1002:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12900 "Parser/parser.cc"
    break;

  case 1003:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12906 "Parser/parser.cc"
    break;

  case 1004:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12912 "Parser/parser.cc"
    break;

  case 1005:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12918 "Parser/parser.cc"
    break;

  case 1007:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12924 "Parser/parser.cc"
    break;

  case 1008:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12930 "Parser/parser.cc"
    break;

  case 1009:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12936 "Parser/parser.cc"
    break;

  case 1010:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12942 "Parser/parser.cc"
    break;

  case 1011:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12948 "Parser/parser.cc"
    break;

  case 1012:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12954 "Parser/parser.cc"
    break;

  case 1014:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12960 "Parser/parser.cc"
    break;

  case 1016:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 12966 "Parser/parser.cc"
    break;

  case 1017:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12972 "Parser/parser.cc"
    break;

  case 1018:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 12978 "Parser/parser.cc"
    break;

  case 1019:
#line 3924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12984 "Parser/parser.cc"
    break;

  case 1020:
#line 3926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12990 "Parser/parser.cc"
    break;

  case 1021:
#line 3928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12996 "Parser/parser.cc"
    break;

  case 1023:
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13002 "Parser/parser.cc"
    break;

  case 1024:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13008 "Parser/parser.cc"
    break;

  case 1025:
#line 3950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 13014 "Parser/parser.cc"
    break;

  case 1026:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13020 "Parser/parser.cc"
    break;

  case 1027:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13026 "Parser/parser.cc"
    break;

  case 1028:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13032 "Parser/parser.cc"
    break;

  case 1029:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13038 "Parser/parser.cc"
    break;

  case 1031:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13044 "Parser/parser.cc"
    break;

  case 1032:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13050 "Parser/parser.cc"
    break;

  case 1033:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13056 "Parser/parser.cc"
    break;

  case 1034:
#line 3973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13062 "Parser/parser.cc"
    break;

  case 1035:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13068 "Parser/parser.cc"
    break;

  case 1038:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13074 "Parser/parser.cc"
    break;

  case 1041:
#line 3996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13080 "Parser/parser.cc"
    break;

  case 1042:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13086 "Parser/parser.cc"
    break;

  case 1043:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13092 "Parser/parser.cc"
    break;

  case 1044:
#line 4002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13098 "Parser/parser.cc"
    break;

  case 1045:
#line 4004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13104 "Parser/parser.cc"
    break;

  case 1046:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13110 "Parser/parser.cc"
    break;

  case 1047:
#line 4013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13116 "Parser/parser.cc"
    break;

  case 1048:
#line 4015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13122 "Parser/parser.cc"
    break;

  case 1049:
#line 4017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13128 "Parser/parser.cc"
    break;

  case 1050:
#line 4019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13134 "Parser/parser.cc"
    break;

  case 1051:
#line 4021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13140 "Parser/parser.cc"
    break;

  case 1052:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13146 "Parser/parser.cc"
    break;

  case 1053:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13152 "Parser/parser.cc"
    break;

  case 1054:
#line 4028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13158 "Parser/parser.cc"
    break;

  case 1055:
#line 4030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13164 "Parser/parser.cc"
    break;

  case 1056:
#line 4032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13170 "Parser/parser.cc"
    break;

  case 1057:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13176 "Parser/parser.cc"
    break;

  case 1058:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 13182 "Parser/parser.cc"
    break;

  case 1059:
#line 4044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 13188 "Parser/parser.cc"
    break;

  case 1060:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13194 "Parser/parser.cc"
    break;

  case 1062:
#line 4073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13200 "Parser/parser.cc"
    break;

  case 1066:
#line 4084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13206 "Parser/parser.cc"
    break;

  case 1067:
#line 4086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13212 "Parser/parser.cc"
    break;

  case 1068:
#line 4088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13218 "Parser/parser.cc"
    break;

  case 1069:
#line 4090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13224 "Parser/parser.cc"
    break;

  case 1070:
#line 4092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13230 "Parser/parser.cc"
    break;

  case 1071:
#line 4094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13236 "Parser/parser.cc"
    break;

  case 1072:
#line 4101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13242 "Parser/parser.cc"
    break;

  case 1073:
#line 4103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13248 "Parser/parser.cc"
    break;

  case 1074:
#line 4105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13254 "Parser/parser.cc"
    break;

  case 1075:
#line 4107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13260 "Parser/parser.cc"
    break;

  case 1076:
#line 4109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13266 "Parser/parser.cc"
    break;

  case 1077:
#line 4111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13272 "Parser/parser.cc"
    break;

  case 1078:
#line 4116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13278 "Parser/parser.cc"
    break;

  case 1079:
#line 4118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13284 "Parser/parser.cc"
    break;

  case 1080:
#line 4120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13290 "Parser/parser.cc"
    break;

  case 1081:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13296 "Parser/parser.cc"
    break;

  case 1082:
#line 4129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13302 "Parser/parser.cc"
    break;

  case 1085:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13308 "Parser/parser.cc"
    break;

  case 1086:
#line 4155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13314 "Parser/parser.cc"
    break;


#line 13318 "Parser/parser.cc"

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
#line 4158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
