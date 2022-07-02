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

#include "SynTree/Attribute.h"     // for Attribute

// lex uses __null in a boolean context, it's fine.
#pragma GCC diagnostic ignored "-Wparentheses-equality"

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
//	printf( "distAttr1 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout );
	DeclarationNode * cur = declList, * cl = (new DeclarationNode)->addType( typeSpec );
//	printf( "distAttr2 cl %p\n", cl ); cl->type->print( std::cout );
//	cl->type->aggregate.name = cl->type->aggInst.aggregate->aggregate.name;

	for ( cur = dynamic_cast<DeclarationNode *>( cur->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur );
	} // for
	declList->addType( cl );
//	printf( "distAttr3 declList %p\n", declList ); declList->print( std::cout, 0 );
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

ForCtrl * forCtrl( ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ConstantExpr * constant = dynamic_cast<ConstantExpr *>(type->expr.get());
	if ( constant && (constant->get_constant()->get_value() == "0" || constant->get_constant()->get_value() == "1") ) {
		type = new ExpressionNode( new CastExpr( maybeMoveBuild<Expression>(type), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) ) );
	} // if
//	type = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__for_control_index_constraints__" ) ) ), type ) );
	return new ForCtrl(
		distAttr( DeclarationNode::newTypeof( type, true ), DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) ) ),
		// NULL comp/inc => leave blank
		comp ? new ExpressionNode( build_binary_val( compop, new ExpressionNode( build_varref( new string( *index ) ) ), comp ) ) : 0,
		inc ? new ExpressionNode( build_binary_val( compop == OperKinds::LThan || compop == OperKinds::LEThan ? // choose += or -= for upto/downto
							OperKinds::PlusAssn : OperKinds::MinusAssn, new ExpressionNode( build_varref( new string( *index ) ) ), inc ) ) : 0 );
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

#line 276 "Parser/parser.cc"

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
    THREADLOCAL = 263,
    INLINE = 264,
    FORTRAN = 265,
    NORETURN = 266,
    CONST = 267,
    VOLATILE = 268,
    RESTRICT = 269,
    ATOMIC = 270,
    FORALL = 271,
    MUTEX = 272,
    VIRTUAL = 273,
    VTABLE = 274,
    COERCE = 275,
    VOID = 276,
    CHAR = 277,
    SHORT = 278,
    INT = 279,
    LONG = 280,
    FLOAT = 281,
    DOUBLE = 282,
    SIGNED = 283,
    UNSIGNED = 284,
    BOOL = 285,
    COMPLEX = 286,
    IMAGINARY = 287,
    INT128 = 288,
    UINT128 = 289,
    uuFLOAT80 = 290,
    uuFLOAT128 = 291,
    uFLOAT16 = 292,
    uFLOAT32 = 293,
    uFLOAT32X = 294,
    uFLOAT64 = 295,
    uFLOAT64X = 296,
    uFLOAT128 = 297,
    DECIMAL32 = 298,
    DECIMAL64 = 299,
    DECIMAL128 = 300,
    ZERO_T = 301,
    ONE_T = 302,
    SIZEOF = 303,
    TYPEOF = 304,
    VALIST = 305,
    AUTO_TYPE = 306,
    OFFSETOF = 307,
    BASETYPEOF = 308,
    TYPEID = 309,
    ENUM = 310,
    STRUCT = 311,
    UNION = 312,
    EXCEPTION = 313,
    GENERATOR = 314,
    COROUTINE = 315,
    MONITOR = 316,
    THREAD = 317,
    OTYPE = 318,
    FTYPE = 319,
    DTYPE = 320,
    TTYPE = 321,
    TRAIT = 322,
    LABEL = 323,
    SUSPEND = 324,
    ATTRIBUTE = 325,
    EXTENSION = 326,
    IF = 327,
    ELSE = 328,
    SWITCH = 329,
    CASE = 330,
    DEFAULT = 331,
    DO = 332,
    WHILE = 333,
    FOR = 334,
    BREAK = 335,
    CONTINUE = 336,
    GOTO = 337,
    RETURN = 338,
    CHOOSE = 339,
    FALLTHRU = 340,
    FALLTHROUGH = 341,
    WITH = 342,
    WHEN = 343,
    WAITFOR = 344,
    DISABLE = 345,
    ENABLE = 346,
    TRY = 347,
    THROW = 348,
    THROWRESUME = 349,
    AT = 350,
    ASM = 351,
    ALIGNAS = 352,
    ALIGNOF = 353,
    GENERIC = 354,
    STATICASSERT = 355,
    IDENTIFIER = 356,
    QUOTED_IDENTIFIER = 357,
    TYPEDIMname = 358,
    TYPEDEFname = 359,
    TYPEGENname = 360,
    TIMEOUT = 361,
    WOR = 362,
    CATCH = 363,
    RECOVER = 364,
    CATCHRESUME = 365,
    FIXUP = 366,
    FINALLY = 367,
    INTEGERconstant = 368,
    CHARACTERconstant = 369,
    STRINGliteral = 370,
    DIRECTIVE = 371,
    FLOATING_DECIMALconstant = 372,
    FLOATING_FRACTIONconstant = 373,
    FLOATINGconstant = 374,
    ARROW = 375,
    ICR = 376,
    DECR = 377,
    LS = 378,
    RS = 379,
    LE = 380,
    GE = 381,
    EQ = 382,
    NE = 383,
    ANDAND = 384,
    OROR = 385,
    ELLIPSIS = 386,
    EXPassign = 387,
    MULTassign = 388,
    DIVassign = 389,
    MODassign = 390,
    PLUSassign = 391,
    MINUSassign = 392,
    LSassign = 393,
    RSassign = 394,
    ANDassign = 395,
    ERassign = 396,
    ORassign = 397,
    ErangeUpEq = 398,
    ErangeDown = 399,
    ErangeDownEq = 400,
    ATassign = 401,
    THEN = 402
  };
#endif
/* Tokens.  */
#define TYPEDEF 258
#define EXTERN 259
#define STATIC 260
#define AUTO 261
#define REGISTER 262
#define THREADLOCAL 263
#define INLINE 264
#define FORTRAN 265
#define NORETURN 266
#define CONST 267
#define VOLATILE 268
#define RESTRICT 269
#define ATOMIC 270
#define FORALL 271
#define MUTEX 272
#define VIRTUAL 273
#define VTABLE 274
#define COERCE 275
#define VOID 276
#define CHAR 277
#define SHORT 278
#define INT 279
#define LONG 280
#define FLOAT 281
#define DOUBLE 282
#define SIGNED 283
#define UNSIGNED 284
#define BOOL 285
#define COMPLEX 286
#define IMAGINARY 287
#define INT128 288
#define UINT128 289
#define uuFLOAT80 290
#define uuFLOAT128 291
#define uFLOAT16 292
#define uFLOAT32 293
#define uFLOAT32X 294
#define uFLOAT64 295
#define uFLOAT64X 296
#define uFLOAT128 297
#define DECIMAL32 298
#define DECIMAL64 299
#define DECIMAL128 300
#define ZERO_T 301
#define ONE_T 302
#define SIZEOF 303
#define TYPEOF 304
#define VALIST 305
#define AUTO_TYPE 306
#define OFFSETOF 307
#define BASETYPEOF 308
#define TYPEID 309
#define ENUM 310
#define STRUCT 311
#define UNION 312
#define EXCEPTION 313
#define GENERATOR 314
#define COROUTINE 315
#define MONITOR 316
#define THREAD 317
#define OTYPE 318
#define FTYPE 319
#define DTYPE 320
#define TTYPE 321
#define TRAIT 322
#define LABEL 323
#define SUSPEND 324
#define ATTRIBUTE 325
#define EXTENSION 326
#define IF 327
#define ELSE 328
#define SWITCH 329
#define CASE 330
#define DEFAULT 331
#define DO 332
#define WHILE 333
#define FOR 334
#define BREAK 335
#define CONTINUE 336
#define GOTO 337
#define RETURN 338
#define CHOOSE 339
#define FALLTHRU 340
#define FALLTHROUGH 341
#define WITH 342
#define WHEN 343
#define WAITFOR 344
#define DISABLE 345
#define ENABLE 346
#define TRY 347
#define THROW 348
#define THROWRESUME 349
#define AT 350
#define ASM 351
#define ALIGNAS 352
#define ALIGNOF 353
#define GENERIC 354
#define STATICASSERT 355
#define IDENTIFIER 356
#define QUOTED_IDENTIFIER 357
#define TYPEDIMname 358
#define TYPEDEFname 359
#define TYPEGENname 360
#define TIMEOUT 361
#define WOR 362
#define CATCH 363
#define RECOVER 364
#define CATCHRESUME 365
#define FIXUP 366
#define FINALLY 367
#define INTEGERconstant 368
#define CHARACTERconstant 369
#define STRINGliteral 370
#define DIRECTIVE 371
#define FLOATING_DECIMALconstant 372
#define FLOATING_FRACTIONconstant 373
#define FLOATINGconstant 374
#define ARROW 375
#define ICR 376
#define DECR 377
#define LS 378
#define RS 379
#define LE 380
#define GE 381
#define EQ 382
#define NE 383
#define ANDAND 384
#define OROR 385
#define ELLIPSIS 386
#define EXPassign 387
#define MULTassign 388
#define DIVassign 389
#define MODassign 390
#define PLUSassign 391
#define MINUSassign 392
#define LSassign 393
#define RSassign 394
#define ANDassign 395
#define ERassign 396
#define ORassign 397
#define ErangeUpEq 398
#define ErangeDown 399
#define ErangeDownEq 400
#define ATassign 401
#define THEN 402

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
	enum OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	CatchStmt::Kind catch_kind;
	GenericExpr * genexpr;

#line 644 "Parser/parser.cc"

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
#define YYFINAL  144
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   20210

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  995
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2021

#define YYUNDEFTOK  2
#define YYMAXUTOK   402


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
       2,     2,     2,   164,     2,     2,     2,   168,   161,     2,
     149,   151,   160,   162,   155,   163,   152,   167,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   156,   174,
     169,   173,   170,   172,   150,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   153,   166,   154,   159,     2,   158,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   157,   171,   148,   165,     2,     2,     2,
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
     145,   146,   147
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   546,   546,   550,   557,   558,   559,   560,   561,   565,
     566,   567,   568,   569,   570,   571,   575,   576,   580,   581,
     586,   590,   591,   602,   604,   606,   610,   611,   613,   615,
     617,   619,   629,   637,   646,   647,   657,   662,   667,   668,
     673,   679,   681,   683,   689,   691,   693,   695,   697,   699,
     701,   703,   705,   707,   709,   711,   713,   715,   717,   719,
     721,   731,   732,   736,   737,   742,   745,   749,   750,   754,
     755,   757,   759,   761,   763,   765,   770,   772,   774,   782,
     783,   791,   794,   795,   797,   802,   818,   820,   822,   824,
     826,   828,   830,   832,   834,   842,   843,   845,   849,   850,
     851,   852,   856,   857,   859,   861,   863,   865,   867,   869,
     871,   878,   879,   880,   881,   885,   886,   890,   891,   896,
     897,   899,   901,   906,   907,   909,   914,   915,   917,   922,
     923,   925,   927,   929,   934,   935,   937,   942,   943,   948,
     949,   954,   955,   960,   961,   966,   967,   972,   973,   976,
     981,   986,   987,   995,  1001,  1002,  1006,  1007,  1011,  1012,
    1016,  1017,  1018,  1019,  1020,  1021,  1022,  1023,  1024,  1025,
    1026,  1036,  1038,  1043,  1044,  1046,  1048,  1053,  1054,  1060,
    1061,  1067,  1068,  1069,  1070,  1071,  1072,  1073,  1074,  1075,
    1076,  1077,  1079,  1080,  1086,  1088,  1098,  1100,  1108,  1109,
    1114,  1116,  1118,  1120,  1122,  1126,  1127,  1129,  1134,  1136,
    1143,  1145,  1147,  1157,  1159,  1161,  1166,  1171,  1174,  1179,
    1181,  1183,  1185,  1193,  1194,  1196,  1200,  1202,  1206,  1208,
    1209,  1211,  1213,  1218,  1219,  1223,  1228,  1229,  1233,  1235,
    1240,  1242,  1247,  1249,  1251,  1253,  1258,  1260,  1262,  1264,
    1269,  1271,  1276,  1277,  1299,  1301,  1303,  1306,  1309,  1312,
    1314,  1316,  1318,  1321,  1324,  1326,  1329,  1336,  1338,  1340,
    1342,  1344,  1349,  1351,  1353,  1355,  1360,  1362,  1367,  1369,
    1371,  1373,  1376,  1380,  1383,  1387,  1389,  1391,  1393,  1395,
    1397,  1399,  1401,  1403,  1405,  1407,  1412,  1413,  1417,  1423,
    1428,  1433,  1434,  1438,  1442,  1447,  1448,  1454,  1458,  1460,
    1462,  1464,  1467,  1469,  1474,  1476,  1481,  1483,  1485,  1490,
    1492,  1498,  1499,  1503,  1504,  1505,  1506,  1510,  1515,  1516,
    1518,  1520,  1522,  1526,  1530,  1531,  1535,  1537,  1539,  1541,
    1543,  1549,  1550,  1556,  1557,  1561,  1562,  1567,  1569,  1575,
    1576,  1578,  1583,  1588,  1599,  1600,  1604,  1605,  1611,  1612,
    1616,  1618,  1622,  1624,  1628,  1629,  1633,  1634,  1638,  1645,
    1646,  1650,  1652,  1667,  1668,  1669,  1670,  1672,  1676,  1678,
    1682,  1689,  1691,  1693,  1698,  1699,  1701,  1703,  1705,  1737,
    1740,  1745,  1747,  1753,  1758,  1763,  1774,  1779,  1784,  1789,
    1794,  1803,  1807,  1814,  1816,  1817,  1818,  1824,  1826,  1831,
    1832,  1833,  1842,  1843,  1844,  1848,  1849,  1856,  1865,  1866,
    1867,  1872,  1873,  1882,  1883,  1888,  1889,  1893,  1895,  1897,
    1899,  1901,  1905,  1910,  1911,  1913,  1923,  1924,  1929,  1931,
    1933,  1935,  1937,  1940,  1942,  1944,  1949,  1951,  1953,  1955,
    1957,  1959,  1961,  1963,  1965,  1967,  1969,  1971,  1973,  1975,
    1977,  1979,  1981,  1983,  1985,  1987,  1989,  1991,  1993,  1995,
    1997,  1999,  2001,  2003,  2008,  2009,  2013,  2020,  2021,  2027,
    2028,  2030,  2032,  2034,  2039,  2041,  2046,  2047,  2049,  2051,
    2056,  2058,  2060,  2062,  2064,  2066,  2071,  2078,  2080,  2082,
    2087,  2095,  2094,  2098,  2106,  2107,  2109,  2111,  2116,  2117,
    2119,  2124,  2125,  2127,  2129,  2134,  2135,  2137,  2142,  2144,
    2146,  2148,  2149,  2151,  2156,  2158,  2160,  2165,  2172,  2176,
    2177,  2182,  2181,  2186,  2185,  2204,  2203,  2215,  2214,  2225,
    2230,  2231,  2236,  2242,  2256,  2257,  2261,  2263,  2265,  2271,
    2273,  2275,  2277,  2279,  2281,  2283,  2285,  2291,  2292,  2297,
    2306,  2308,  2317,  2319,  2320,  2321,  2323,  2325,  2326,  2331,
    2332,  2333,  2338,  2340,  2343,  2350,  2351,  2352,  2358,  2363,
    2365,  2371,  2372,  2378,  2379,  2383,  2388,  2391,  2390,  2394,
    2397,  2405,  2404,  2413,  2419,  2423,  2425,  2430,  2432,  2434,
    2436,  2442,  2443,  2444,  2451,  2452,  2454,  2455,  2456,  2458,
    2460,  2467,  2468,  2470,  2472,  2477,  2478,  2484,  2485,  2487,
    2488,  2493,  2494,  2495,  2497,  2505,  2506,  2508,  2511,  2513,
    2517,  2518,  2519,  2521,  2523,  2528,  2530,  2535,  2537,  2546,
    2548,  2553,  2554,  2555,  2559,  2560,  2561,  2566,  2567,  2572,
    2573,  2574,  2575,  2579,  2580,  2585,  2586,  2587,  2588,  2589,
    2603,  2604,  2609,  2610,  2616,  2618,  2621,  2623,  2625,  2648,
    2649,  2655,  2656,  2662,  2661,  2671,  2670,  2674,  2680,  2686,
    2687,  2689,  2693,  2698,  2700,  2702,  2704,  2710,  2711,  2715,
    2716,  2721,  2723,  2730,  2732,  2733,  2735,  2740,  2742,  2744,
    2749,  2751,  2756,  2761,  2769,  2771,  2776,  2777,  2782,  2783,
    2787,  2788,  2789,  2794,  2796,  2802,  2804,  2809,  2811,  2817,
    2818,  2822,  2826,  2830,  2832,  2833,  2834,  2839,  2842,  2841,
    2853,  2852,  2864,  2863,  2875,  2874,  2886,  2885,  2899,  2905,
    2907,  2913,  2914,  2925,  2932,  2937,  2943,  2946,  2949,  2953,
    2959,  2962,  2965,  2970,  2971,  2972,  2976,  2982,  2983,  2993,
    2994,  2998,  2999,  3004,  3009,  3010,  3016,  3017,  3019,  3024,
    3025,  3026,  3027,  3028,  3030,  3065,  3067,  3072,  3074,  3075,
    3077,  3082,  3084,  3086,  3088,  3093,  3095,  3097,  3099,  3101,
    3103,  3105,  3110,  3112,  3114,  3116,  3125,  3127,  3128,  3133,
    3135,  3137,  3139,  3141,  3146,  3148,  3150,  3152,  3157,  3159,
    3161,  3163,  3165,  3167,  3179,  3180,  3181,  3185,  3187,  3189,
    3191,  3193,  3198,  3200,  3202,  3204,  3209,  3211,  3213,  3215,
    3217,  3219,  3234,  3239,  3244,  3246,  3247,  3249,  3254,  3256,
    3258,  3260,  3265,  3267,  3269,  3271,  3273,  3275,  3277,  3282,
    3284,  3286,  3288,  3290,  3300,  3302,  3304,  3305,  3307,  3312,
    3314,  3316,  3321,  3323,  3325,  3327,  3332,  3334,  3336,  3350,
    3352,  3354,  3355,  3357,  3362,  3364,  3369,  3371,  3373,  3378,
    3380,  3385,  3387,  3404,  3405,  3407,  3412,  3414,  3416,  3418,
    3420,  3425,  3426,  3428,  3430,  3435,  3437,  3439,  3445,  3447,
    3449,  3452,  3456,  3458,  3460,  3462,  3496,  3497,  3499,  3501,
    3506,  3508,  3510,  3512,  3514,  3519,  3520,  3522,  3524,  3529,
    3531,  3533,  3539,  3540,  3542,  3551,  3554,  3556,  3559,  3561,
    3563,  3577,  3578,  3580,  3585,  3587,  3589,  3591,  3593,  3598,
    3599,  3601,  3603,  3608,  3610,  3618,  3619,  3620,  3625,  3626,
    3631,  3633,  3635,  3637,  3639,  3641,  3648,  3650,  3652,  3654,
    3656,  3659,  3661,  3663,  3665,  3667,  3672,  3674,  3676,  3681,
    3707,  3708,  3710,  3714,  3715,  3719,  3721,  3723,  3725,  3727,
    3729,  3736,  3738,  3740,  3742,  3744,  3746,  3751,  3753,  3755,
    3762,  3764,  3782,  3784,  3789,  3790
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TYPEDEF", "EXTERN", "STATIC", "AUTO",
  "REGISTER", "THREADLOCAL", "INLINE", "FORTRAN", "NORETURN", "CONST",
  "VOLATILE", "RESTRICT", "ATOMIC", "FORALL", "MUTEX", "VIRTUAL", "VTABLE",
  "COERCE", "VOID", "CHAR", "SHORT", "INT", "LONG", "FLOAT", "DOUBLE",
  "SIGNED", "UNSIGNED", "BOOL", "COMPLEX", "IMAGINARY", "INT128",
  "UINT128", "uuFLOAT80", "uuFLOAT128", "uFLOAT16", "uFLOAT32",
  "uFLOAT32X", "uFLOAT64", "uFLOAT64X", "uFLOAT128", "DECIMAL32",
  "DECIMAL64", "DECIMAL128", "ZERO_T", "ONE_T", "SIZEOF", "TYPEOF",
  "VALIST", "AUTO_TYPE", "OFFSETOF", "BASETYPEOF", "TYPEID", "ENUM",
  "STRUCT", "UNION", "EXCEPTION", "GENERATOR", "COROUTINE", "MONITOR",
  "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE", "TRAIT", "LABEL",
  "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE", "SWITCH", "CASE",
  "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE", "GOTO", "RETURN",
  "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN", "WAITFOR",
  "DISABLE", "ENABLE", "TRY", "THROW", "THROWRESUME", "AT", "ASM",
  "ALIGNAS", "ALIGNOF", "GENERIC", "STATICASSERT", "IDENTIFIER",
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
  "for_control_expression", "inclexcl", "jump_statement",
  "fall_through_name", "with_statement", "mutex_statement", "when_clause",
  "when_clause_opt", "waitfor", "cast_expression_list", "timeout",
  "waitfor_clause", "waitfor_statement", "exception_statement",
  "handler_clause", "handler_predicate_opt", "handler_key",
  "finally_clause", "exception_declaration", "enable_disable_statement",
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
  "declaration_specifier", "declaration_specifier_nobody",
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
  "enum_type", "$@6", "$@7", "enum_type_nobody", "enumerator_list",
  "enumerator_value_opt", "cfa_parameter_ellipsis_list_opt",
  "cfa_parameter_list", "cfa_abstract_parameter_list",
  "parameter_type_list_opt", "parameter_list", "cfa_parameter_declaration",
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
  "type_ptr", "type_array", "type_function",
  "identifier_parameter_declarator", "identifier_parameter_ptr",
  "identifier_parameter_array", "identifier_parameter_function",
  "type_parameter_redeclarator", "typedef_name", "type_parameter_ptr",
  "type_parameter_array", "type_parameter_function", "abstract_declarator",
  "abstract_ptr", "abstract_array", "abstract_function", "array_dimension",
  "multi_array_dimension", "abstract_parameter_declarator",
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
     395,   396,   397,   398,   399,   400,   401,   402,   125,    40,
      64,    41,    46,    91,    93,    44,    58,   123,    96,    94,
      42,    38,    43,    45,    33,   126,    92,    47,    37,    60,
      62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1714)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-876)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      53, 12065,    82,   146, 16487,   -46, -1714, -1714, -1714, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714,    65,   924,   121,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714,   111,   220, -1714,
   -1714, -1714, -1714, -1714, -1714,  3628,  3628,   150, 12065,   179,
     212, -1714, -1714,   289, -1714, -1714, -1714, -1714, -1714, -1714,
   -1714, -1714, -1714,  2698, -1714,   672,   305, -1714, -1714, -1714,
   -1714, -1714, 16337, -1714, -1714,   307,   383,   373,    90, -1714,
    3628,   383,   383,   383,   341,  3947,   553,   891, 12224, -1714,
   -1714, -1714, 16187,  1117, -1714, -1714, -1714,  1460,   568, 12276,
     518,   962,  1460,  1022,   472, -1714, -1714, -1714, -1714,   512,
   -1714, -1714, -1714, -1714,   443, -1714, -1714, -1714, -1714, -1714,
     484,   516,   512, -1714,   512,   528, -1714, -1714, -1714, 17043,
    3628, -1714, -1714,  3628, -1714, 12065,   474, 17095, -1714, -1714,
    4050, 18107, -1714,   982,   982,   548,  2471, -1714, -1714, -1714,
   -1714,   429, 14797,  2673,   512, -1714, -1714, -1714, -1714, -1714,
   -1714,   538, -1714,   532,   578,   608, -1714,   651, 19685, 15417,
    3079,  2698,   230,   617,   628,   645,   647,   671,   676, -1714,
   -1714, 17245, 11024,   691, -1714, 16630, -1714, -1714, -1714, -1714,
     714, -1714, -1714,   713, -1714,  8012,   873, 18965, -1714,   752,
    3628,   516,   760,   762,   767,   778, -1714, -1714, -1714,  2183,
    2600,   787,   854,   102, -1714, -1714,   512,   512,    69,   101,
     420,    69, -1714,   512,   512, -1714,  3876, -1714, -1714,   803,
     822,   982, 14375, -1714, -1714, 16337, -1714, -1714,  1460, -1714,
    1355,   472,   816,   934,   101,  3628,   373, -1714, 13773, -1714,
     982,   982,   844,   934,   101,  3628, -1714, 13665, -1714, -1714,
     982, -1714,   982, -1714,   728,  3509,  3628, -1714,   993,   859,
   -1714, -1714, -1714, 16789,   516,   191, -1714, -1714, 18157, -1714,
     854,   207, -1714, 19685, 18107,  3003,  3876, -1714,   440, -1714,
   -1714, -1714, 17095,  3628, -1714,   870, -1714, -1714, -1714, -1714,
    3628,  3113,   335,   590, -1714,  3628,   532, -1714,   657,   512,
     902, 17297,   796, 14955, 14533,  1460,  1460, -1714,  1460,   982,
    1460,   982, -1714, -1714,   512, -1714,   918, -1714, 17447, -1714,
   -1714, -1714, 17499,   714, -1714,   922,   446,   976,   933,   472,
     936, -1714,  2471,   938,   532,  2471,  1470, -1714,   923,  1023,
   19757,   996,  1009, 19685, 19829,  1038, 13522, -1714, -1714, -1714,
   -1714, -1714, -1714, 19901, 19901, 15263,  1042,  4064, -1714, -1714,
   -1714, -1714,   279, -1714,   360, -1714,  2196, -1714, 19685, 19685,
   -1714,  1036,   626,   877,   948,   530,   920,  1058,  1046,  1065,
    1100,   118, -1714,   704, -1714,  1086, -1714,   954,  4126, 15725,
   -1714, -1714,   738,  1086, -1714, -1714,   718, -1714, -1714,  3079,
    1092,  1097,  1115,  1120,  1138,  1174, -1714, -1714,   462,  1098,
   -1714,   775,  1098, -1714, -1714, 17043, -1714,  1013,  1150, 15879,
   -1714, -1714,  3349,  3758,  1196, 14955,  1201,   490,   613, -1714,
   -1714, -1714, -1714, -1714,  3628,  3816, -1714, -1714, -1714, -1714,
   -1714, -1714,  9281,   740,  1042,  8012,  1183,  1185, -1714, -1714,
    1188, 18965,   763, -1714, -1714, -1714, 19037,  1207, -1714, -1714,
   -1714, -1714, -1714,  2183,   809,  1199,  1208,  1228,   817,  1234,
    1269,  1292,  2600, -1714, -1714,   512,  1251,   373,  1276, -1714,
   -1714,  1298, -1714, -1714,   516,   934, -1714, -1714, -1714,   516,
   -1714, -1714,  3876, -1714, 15725, 15725, -1714,   982,  4050, 18885,
   14797, -1714, -1714, -1714, -1714, -1714,   516,   934,   207, -1714,
   -1714,  1460,  1304,   934,   101, -1714,   516,   934, -1714, 15163,
   -1714,   982,   982, -1714, -1714,  1320,   450,  1324,   472,  1335,
   -1714, 18315, -1714,   800, -1714,  1441, 18781, -1714,  4050, 17658,
   14375, -1714, 16789, 19973, -1714, -1714, -1714, -1714, -1714,  3003,
     866,  3876, -1714, 14797,   854, 12065, -1714,  1358, -1714,  1365,
   -1714, -1714, -1714, -1714, -1714,  2471, -1714, -1714,  1440,  4125,
   17499, 11024, -1714, 17710, -1714,   982,   982, -1714, -1714,   714,
   -1714,   850,  1366,  1505, 19685,  2020,  1298,  1354, -1714,   512,
     512, -1714,  1098, -1714, 17297, -1714, -1714, 18596,   982,   982,
   -1714,  4125,   512, -1714, 17964, -1714, -1714, 17447, -1714,   429,
    1377,  1361,  1379,   976,   808, 17095,   836, -1714, -1714, -1714,
   -1714, -1714, -1714,   851, -1714,  1386,  1363, -1714, 15571, -1714,
   17762, 17762, -1714, 15571, -1714, 19685, -1714, 12276, 12276, 15571,
   -1714, -1714, 16841, 17762, 17762,   954,  1262,  1284,   784,  1299,
   -1714,   857,  1388,   970,  1393, -1714, 19037, 19685, 19109,  1390,
   19685,   993, 19685,   993, -1714,  2973, -1714, -1714, 19181,  2393,
   19685, 19181,   993, -1714, -1714, 19685, 19685, 19685, 19685, 19685,
   19685, 19685, 19685, 19685, 19685, 19685, 19685, 19685, 19685, 19685,
   19685, 19685, 19685, 19685, 19253,  1371,   651,  2406, 11024, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,
    1391, 19685, -1714, -1714,   738,  2170, -1714, -1714,   512,   512,
   -1714, -1714, 15725, -1714,   492,  1098, -1714,   871,  1098, -1714,
   -1714, -1714,  1298, -1714, -1714,  1298, 20045, -1714, -1714, 11024,
    1395,  1396,  1043,  1535,  2890,   566,  1354, -1714,   512,   512,
    1354,   573, -1714,   512,   512, 19685,  3628,  1030,  1114,  1354,
     217, 14323, 14323,  3628, -1714, -1714, 19685,  1188, -1714,  8012,
    1409, -1714,  2380, -1714, -1714, -1714, -1714, -1714,   890, -1714,
   14323,   993,  4050,   993,   914,  1407,  1408,  1410,   917,  1413,
    1416,  1417,   576,  1098, -1714, -1714,   594,  1098, -1714, -1714,
   -1714,  4050,   651, -1714,  1098, 20045, -1714,   516, 18315, -1714,
   -1714,   940,  1418,   986,  1421, -1714,  1437, -1714,   516, -1714,
   -1714,   516,   934,  1437, -1714,   516,  1429,  1432,  1433, -1714,
   -1714, 18596, -1714,  1442, -1714, -1714, -1714,   993,  3628, 10183,
    1523,  1422, 18683, -1714,  1150, -1714, 14323,   994, -1714, -1714,
    1437, -1714, 17095, 15725,  1419, -1714,  1419, -1714, -1714, -1714,
   -1714, 17447, -1714, 11186, 16033, -1714, 18315,  1450,  1451,  1454,
   -1714,  9088,   512, -1714,  2020, -1714, -1714, -1714, -1714,  1298,
   -1714, -1714, -1714,   982, -1714,  3310, -1714, -1714,   472,  1153,
    1458, 19325, -1714,   976,  1377, -1714, -1714,  1452,  1457,  1470,
   19181, -1714,  1463,   305,  1456,  1465,  1468,  1472,  1478, 19685,
    1479,  1480,  1481, 11024, 19685, -1714, -1714,  1381, -1714, -1714,
   -1714, 19685, -1714,  1484,  1485,  9417,  1137, -1714, 19181,  1469,
   -1714,  1488, -1714, -1714,  3165, -1714, -1714,  1006, -1714, -1714,
   -1714, -1714,  3165, -1714, -1714,  1167,   607, -1714, -1714,  1036,
    1036,  1036,   626,   626,   877,   877,   948,   948,   948,   948,
     530,   530,   920,  1058,  1046,  1065,  1100, 19685,  1211, -1714,
    1492,  3165, -1714, -1714,  8012, -1714, 18315,  1493,  1496,  1498,
    2170, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,  1298,
   -1714, -1714,  1298, 18315, 18315, -1714, -1714,  1043,   906,  1499,
    1500,  1501,  1502,  2359,  2890, -1714, -1714, -1714, -1714, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,  1503,
   -1714,  1354, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,
    1504,  1508, -1714,   373,  3165,  1215,    56, -1714, -1714,  1506,
   -1714, 18965, -1714, 19685,   512, 19397, 14323, -1714, -1714, -1714,
    1487,   597,  1098, -1714,   616,  1098, -1714, -1714, -1714, -1714,
    1298, -1714, -1714, -1714,  1298,   854,  1510,  1298, -1714, -1714,
   -1714, -1714, -1714, -1714, -1714,  1515, -1714, -1714,  1437, -1714,
     516, -1714, -1714, -1714, -1714, -1714, 12854,  1513,  1512, -1714,
     313, -1714,   489,    59, 10862,  1516, 14152,  1525,  1526,  1964,
    2260,  2277, 19469,  1530, -1714, -1714,  1532,  1533, -1714, -1714,
     516, 19685, 19685,  1659,  1527,   565, -1714,  1613,  1537,  1514,
   -1714, -1714, -1714, 10011, -1714, -1714, -1714, -1714, -1714,  1739,
   -1714, -1714, -1714,  1605, -1714, -1714, -1714,   993, -1714, -1714,
   12701, 16337,  1540, -1714,  3628, -1714,  1521,  1543,  1544, -1714,
    1233, -1714, -1714, -1714, -1714,  4050, -1714, -1714,  1528,  1531,
    1024, 17095,   532,   532, -1714, -1714,  1042,  1150, 15879, -1714,
    1086, -1714, 11348, -1714,   658,  1098, -1714,   982, 11902, -1714,
   -1714,   976,   512,   512,   429,  1361, -1714,  8012, -1714,  1377,
    1552,  1556, -1714, -1714,  1031,   574, 11024,   993, -1714,   574,
   16893,   574, -1714, 19685, 19685, 19685, -1714, -1714, -1714, -1714,
   19685, 19685,  1554,  8012, -1714, -1714,  1557,   664, -1714, -1714,
   -1714,  2568, -1714, -1714,  1235, -1714,   218, -1714, 19181,  1249,
   -1714, 19037, -1714, -1714, 19685,  1541,  1268,  1270,  1188, -1714,
     665,  1098, -1714, -1714, 18315, 18315, -1714, -1714,  1562,   668,
    1098, -1714,   686,  2860,   512,   512, -1714, -1714, 18315, 18315,
   -1714,  1567, -1714, 14797, 14797,  1565,  1568,  1570,  1578, -1714,
    1581, 19685, 19685,  1279,  1583, -1714, -1714, -1714, -1714, -1714,
   -1714, -1714,  1588, 19685, -1714, -1714, -1714,  1298, -1714, -1714,
   -1714,  1298, 18315, 18315,   373,   512,  1287,  1590,  1594, -1714,
   -1714,  1595, 13007, 13160, 13313, 17095, 17762, 17762,  1596, -1714,
    1573,  1574,   771, 13615, -1714,   430,  3628, -1714, -1714,  3628,
   -1714, 18893,   -29,   105, -1714, -1714, -1714, -1714, 19685,  1601,
    1674, 10699, 10355, -1714,  1579, -1714,  1586, 19685,  1592,  8012,
    1597, 19685, 19037, 19685,  1173, -1714,  1600,   157, -1714,   149,
    1608, -1714, -1714,  1614, -1714,  1602, -1714,  1607,  1619, 14152,
     453, 13931,   512,   442, -1714, -1714, -1714,  1628, -1714,  1622,
   -1714,  1635, -1714,  1632, -1714,  1639, -1714, -1714, -1714, -1714,
   11510,  1642,  1644,  1645, -1714,  1649, -1714, -1714, -1714,  1298,
   19685, 19685,  1150,  1647, -1714,  1377, -1714,  1648,   262, -1714,
    1188,  1654, -1714, -1714, 17095, -1714,  1653,  1650,  1039, -1714,
    1651, -1714, -1714, -1714, -1714, -1714,  8012,  1188, 19037, -1714,
    1690,  3165, -1714,  1690,  1690, -1714,  3165,  3425,  3531, -1714,
   -1714,  1295, -1714, -1714, -1714,  1661,  1662, -1714, -1714, -1714,
    1298, -1714, -1714,  1663,  1665,   512, -1714, -1714, -1714,  1298,
   -1714, -1714, -1714,  1666, -1714, -1714, -1714, -1714, -1714, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714,  1656, -1714, -1714,
   -1714, -1714,  1667,  1668,   512, -1714, 18315, 18315, -1714, -1714,
   -1714, -1714, 19685, -1714, -1714,  1675, -1714,  1596,  1596,  1596,
     901,  1652,   498, -1714,  2115,   515, 15725, -1714, -1714, -1714,
    3187, 19685,  3889,   520, -1714, -1714,    40,  1669,  1669,  3628,
   -1714, -1714, 18464, -1714, 19685,  1673,  1671, -1714, -1714, -1714,
   -1714,  1044,  1681, 14152,  1537,  1680, 19685,   307,  1677,   341,
   13472, 17095, 14152, 19685, 19685,  1033,   384, -1714, 19685, -1714,
   -1714,   522, -1714,  1188, -1714,  1057,  1060,  1062, -1714, -1714,
   -1714, -1714,   516,  1173,  1687, -1714, -1714, 19685, -1714,  1688,
     651, 10862, -1714, -1714, -1714, -1714, 19685,  1717, -1714,  9839,
   -1714,   512, 14797, -1714, -1714, 17095, -1714, -1714, -1714, -1714,
   -1714,  1684, -1714, 18315, -1714, -1714,  1685, -1714,  1701,  1709,
    1702,   976, -1714,  1710, -1714, -1714, -1714, 19685, -1714, 16893,
   19685,  1188,  1719,  1302, -1714,  1319, -1714,  3165, -1714,  3165,
   -1714, -1714, -1714, -1714, 18315,  1720,  1725, -1714, -1714, 18315,
   18315,  1727,  1729,  1321, 14481, 14639, -1714,  1714, -1714, -1714,
   -1714, -1714,  1731,  1732,  1325, -1714, -1714, -1714, -1714,   901,
    2197,   546, -1714, -1714, -1714, -1714,   512,   512, -1714, -1714,
   -1714,   563, -1714,  1070,  3187,   510, -1714,  3889,   512, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714,   593, 14152,   126,
   19541,  1797, 14152,  1537, 15113,  1811,  1537,  1718, -1714, -1714,
   -1714, -1714,  7537, 19685, 14152, 10527,  1724, -1714,  1743,   258,
   14152, -1714, -1714,  1745, -1714, -1714,  1726,   651,   493,  1750,
    1751,  1283,  1815, -1714, -1714, -1714, -1714,  3628,  4050, -1714,
   -1714,  1752,  1758, -1714, -1714, -1714,   976,  1377, -1714,  1756,
   -1714, -1714, -1714,  1768, -1714, -1714, -1714,  1332,  1334, -1714,
   -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,  1754,
   -1714, -1714,  1767,  1769, -1714, -1714, -1714,  1770,  1771,  1777,
    2197, -1714,   512, -1714, -1714, -1714, -1714, -1714,  1778,  2115,
   -1714, -1714,  5867,    57, 11675, -1714, 14034, -1714,    15,  1075,
   14152,  1859, 14152, 19685,  1781, 19685,  1079,  1761,   328,  1866,
   -1714, 19685,  1772, 11836, -1714, -1714, -1714, 17912, -1714,  1786,
    1775,   234, 14152, -1714, 19685, 19181,   486, -1714, -1714, -1714,
    1792, -1714, -1714,  1377,  1804, -1714, -1714, -1714, -1714,  1803,
    1808,  1809, 14797,  1800, -1714, -1714,   689,  1098, -1714, -1714,
     901, -1714, -1714,   282, -1714,    74, -1714, -1714, -1714,  1813,
   12383, -1714, -1714, 14152, -1714,    50, -1714, 14152, -1714, -1714,
    1537,  1817,  1820, 19685, 19685, 19685, 14152, -1714, -1714,  1824,
   12383, 17912, -1714,  3276, 17710,   993,  1807, -1714,  1864,  1819,
     523,  1821, -1714,  1900, -1714,  1090, 14152,  1828, 14152, 14152,
   -1714,  1831, -1714, -1714, -1714, -1714, -1714, -1714, -1714, -1714,
    1298, -1714, 19685, -1714, 19685, -1714, -1714,  1378, 12542, -1714,
   -1714, 14152, -1714, -1714,  1816,  1825,   412, -1714,  1537, -1714,
   -1714,  1378, -1714,  1812,  3398,  3612, -1714, -1714, -1714,   234,
    1835, 19685,  1822,   234,   234, 14152, -1714, -1714, 19685,  1887,
    1890, -1714, 18315, -1714, -1714, 14034, -1714,  1378, -1714, -1714,
   19685, 19613, 19685, -1714,  1812, 19685,  1847,  3612,  1843,   651,
    1849, -1714,   639, -1714, -1714,  1095,  1815,   358, -1714, -1714,
    9601,  1854, 14034,  1537, -1714,  1537,  1537,  1856,  1855, -1714,
     516,   651,  1861, -1714,  1834,   651, -1714, -1714, 14152,  1932,
    1860, -1714, -1714, -1714,  9720, -1714,   516, -1714, -1714,  1339,
   19685, -1714,  1139, -1714, 14152, -1714, -1714,   651,   993,  1862,
    1838, -1714, -1714, -1714,  1168, -1714, -1714,  1842,   993, -1714,
   -1714
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   421,     0,     2,   421,   438,   439,   440,   441,   442,
     443,   444,   445,   427,   429,   428,   430,     0,     0,     0,
     446,   448,   469,   449,   470,   452,   453,   467,   468,   447,
     465,   466,   450,   451,   454,   455,   456,   457,   458,   459,
     460,   461,   462,   463,   464,   471,   472,   759,   474,   547,
     548,   551,   553,   549,   555,     0,     0,     0,   421,     0,
       0,    16,   518,   524,     9,    10,    11,    12,    13,    14,
      15,   723,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   775,   421,   724,   370,     0,   373,   649,   375,   384,
       0,   374,   404,   405,     0,     0,     0,     0,   501,   423,
     425,   431,   421,   433,   436,   486,   473,   409,   479,   484,
     410,   496,   411,   511,   515,   521,   500,   527,   539,   759,
     544,   545,   528,   594,   376,   377,     3,   725,   738,   426,
       0,     0,   759,   797,   759,     2,   814,   815,   816,   421,
       0,   973,   974,     0,     1,   421,     0,   421,   393,   394,
       0,   501,   415,   416,   417,   728,     0,   550,   552,   554,
     556,     0,   421,     0,   760,   761,   546,   475,   642,   643,
     641,   702,   697,   687,     0,     0,   726,     0,     0,   421,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   519,
     522,   421,   421,     0,   975,   501,   804,   822,   979,   972,
     970,   977,   369,     0,   159,   655,   158,     0,   378,     0,
       0,     0,     0,     0,     0,     0,   368,   874,   875,     0,
       0,   403,   757,   759,   753,   778,   759,   759,   755,     2,
     759,   754,   835,   759,   759,   832,     0,   494,   495,     0,
       0,   421,   421,   438,     2,   421,   385,   424,   434,   487,
       0,   516,     0,   741,     2,     0,   649,   386,   501,   480,
     497,   512,     0,   741,     2,     0,   437,   481,   488,   489,
     498,   503,   513,   517,     0,   531,     0,   717,     2,     2,
     739,   796,   798,   421,     0,     2,     2,   983,   501,   986,
     757,   757,     3,     0,   501,     0,     0,   396,   759,   755,
     754,     2,   421,     0,   721,     0,   683,   685,   684,   686,
       0,     0,   679,     0,   669,     0,   678,   689,     0,   759,
       2,   421,   994,   422,   421,   433,   412,   479,   413,   504,
     414,   511,   508,   529,   759,   530,     0,   630,   421,   631,
     948,   949,   421,   632,   634,   518,   524,     0,   595,   596,
       0,   762,     0,   700,   688,     0,   766,    21,     0,    20,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   421,     2,     0,    98,    99,
     100,   101,    82,    24,    83,    38,    81,   102,     0,     0,
     117,   119,   123,   126,   129,   134,   137,   139,   141,   143,
     145,   147,   150,     0,    26,     0,   525,     2,   102,   421,
     151,   694,   645,   515,   647,   693,     0,   644,   648,     0,
       0,     0,     0,     0,     0,     0,   776,   802,   759,   812,
     820,   824,   830,     2,   981,   421,   984,     2,    95,   421,
       3,   629,     0,   994,     0,   422,   479,   504,   511,     3,
       3,   611,   615,   625,   631,   632,     2,   805,   823,   971,
       2,     2,    23,     0,     2,   655,    24,     0,   653,   656,
     992,     0,     0,   662,   651,   650,     0,     0,   743,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   781,   838,   759,     0,   649,     2,   777,
     785,   901,   779,   780,     0,   741,     2,   834,   842,     0,
     836,   837,     0,   399,   421,   421,   485,   422,     0,   501,
     421,   976,   980,   978,   502,   721,     0,   741,   757,   379,
     387,   435,     0,   741,     2,   721,     0,   741,   698,   482,
     483,   499,   514,   520,   523,   518,   524,   542,   543,     0,
     699,   421,   639,     0,   196,   362,   421,     3,     0,   501,
     421,   740,   421,     0,   381,     2,   382,   718,   401,     0,
       0,     0,     2,   421,   757,   421,   721,     0,     2,     0,
     682,   681,   680,   675,   432,     0,   673,   690,   477,     0,
     421,   421,   950,   422,   418,   419,   420,   954,   945,   946,
     952,     2,     2,    96,     0,   910,   924,   994,   906,   759,
     759,   915,   922,   637,   421,   509,   633,   422,   505,   506,
     510,     0,   759,   960,   422,   965,   957,   421,   962,     0,
     992,   601,     0,     0,     0,   421,     0,   774,   773,   769,
     771,   772,   770,     0,   764,   767,     0,    22,   421,    89,
     421,   421,    84,   421,    91,     0,    32,     0,    33,   421,
      87,    88,   421,   421,   421,     2,    98,    99,     0,     0,
     177,     0,     0,   545,     0,   970,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,    56,    57,    61,     0,
       0,    61,     0,    85,    86,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   421,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     158,     0,   156,   157,     2,   886,   646,   883,   759,   759,
     891,   526,   421,   803,   759,   813,   821,   825,   831,     2,
     806,   808,   810,     2,   826,   828,     0,   982,   985,   421,
       0,     0,     2,    96,   910,   759,   994,   856,   759,   759,
     994,   759,   871,   759,   759,     3,   633,     0,     0,   994,
     994,   421,   421,     0,     2,   664,     0,   992,   661,   993,
       0,   657,     0,     2,   660,   663,   174,   173,     0,     2,
     421,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   759,   790,   794,   833,   759,   847,   852,   782,
     839,     0,     0,   407,   898,     0,   744,     0,   421,   745,
     400,     0,     0,     0,     0,   398,     2,   746,     0,   383,
     721,     0,   741,     2,   747,     0,     0,     0,     0,   557,
     618,   422,     3,     3,   622,   621,   817,     0,     0,   421,
     363,     0,   501,     3,    95,     3,   421,     0,     3,   722,
       2,   677,   421,   421,   671,   670,   671,   478,   476,   595,
     956,   421,   961,   422,   421,   947,   421,     0,     0,     0,
     925,     0,   759,   995,   911,   912,   638,   908,   909,   923,
     951,   955,   953,   507,   542,     0,   959,   964,   598,   993,
       0,     0,   597,     0,   992,   703,   701,     0,     0,   766,
      61,   727,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,     0,   116,   115,     0,   112,   111,
      27,     0,    28,     0,     0,     0,     0,     3,    61,     0,
      46,     0,    47,    54,     0,    53,    65,     0,    62,    63,
      66,    49,     0,    48,    52,     0,     0,    45,   118,   120,
     121,   122,   124,   125,   127,   128,   132,   133,   130,   131,
     135,   136,   138,   140,   142,   144,   146,     0,     0,   372,
       0,     0,    29,     3,   655,   152,   421,     0,     0,     0,
     887,   888,   884,   885,   696,   695,     2,   807,   809,   811,
       2,   827,   829,   421,   421,   903,   902,     2,     0,     0,
       0,     0,     0,   759,   911,   859,   876,     2,   854,   862,
     635,   857,   858,   636,     2,   869,   879,   872,   873,     0,
       3,   994,   391,     2,   987,     2,   626,   627,   605,     3,
       3,     3,     3,   649,     0,   150,     0,     3,     3,     0,
     658,     0,   652,     0,   759,     0,   421,     3,   395,   397,
       0,   759,   791,   795,   759,   848,   853,     2,   783,   786,
     788,     2,   840,   843,   845,   757,     0,   899,     3,   749,
       3,   491,   490,   493,   492,     2,   722,   750,     2,   748,
       0,   722,   751,   557,   557,   557,   421,     0,     0,   640,
       0,   366,     0,     0,   421,     0,     2,     0,     0,     0,
       0,     0,   179,     0,   296,   297,     0,     0,   335,   334,
       0,   154,   154,   341,   518,   524,   193,     0,   180,     0,
     204,   181,   182,   421,   198,   183,   184,   185,   186,     0,
     187,   188,   302,     0,   189,   190,   191,     0,   192,   200,
     501,   421,     0,   202,     0,   360,     0,     0,     0,     3,
       0,   729,   722,   710,   711,     0,     3,   706,     3,     3,
       0,   421,   687,   687,   958,   963,     2,    95,   421,     3,
     516,     3,   422,     3,   759,   918,   921,   421,     3,   907,
     913,     0,   759,   759,     0,   601,   586,   655,   602,   992,
       0,     2,   763,   765,     0,    90,   421,     0,    94,    92,
     421,     0,   106,     0,     0,     0,   110,   114,   113,   178,
       0,     0,     0,   655,   103,   171,     0,     0,    41,    42,
      79,     0,    79,    79,     0,    67,    69,    44,     0,     0,
      40,     0,    43,   149,     0,     0,     0,     0,   992,     3,
     759,   894,   897,   889,   421,   421,     3,     3,     0,   759,
     865,   868,   759,     0,   759,   759,   860,   877,   421,   421,
     988,     0,   628,   421,   421,     0,     0,     0,     0,   380,
       3,     0,     0,     0,     0,   654,   659,     3,   742,   176,
     175,     3,     0,     0,     2,   784,   787,   789,     2,   841,
     844,   846,   421,   421,   649,   759,     0,     0,     0,   722,
     752,     0,   421,   421,   421,   421,   421,   421,   540,   568,
       3,     3,   569,   501,   558,     0,     0,   799,     2,     0,
     364,    61,     0,     0,   287,   288,   201,   203,     0,     0,
       0,   421,   421,   283,     0,   281,     0,     0,     0,   655,
       0,     0,     0,     0,     0,   155,     0,     0,   342,     0,
       0,     3,   208,     0,   199,     0,   278,     0,     0,     2,
       0,   501,   759,     0,   361,   905,   904,     0,     2,     0,
     713,     2,   708,     0,   709,     0,   691,   672,   676,   674,
     421,     0,     0,     0,     3,     0,     2,   914,   916,   917,
       0,     0,    95,     0,     3,   992,   591,     0,   601,   599,
     992,     0,   589,   704,   421,   768,     0,     0,     0,    34,
       0,   107,   109,   108,   105,   104,   655,   992,     0,    60,
      76,     0,    70,    77,    78,    55,     0,     0,     0,    64,
      51,     0,   148,   371,    30,     0,     0,     2,   890,   892,
     893,     3,     3,     0,     0,   759,     2,   861,   863,   864,
       2,   878,   880,     0,   855,   870,     3,     3,   989,     3,
     613,   612,   616,   991,     2,     2,   990,     0,     3,   756,
     665,   666,     0,     0,   759,   402,   421,   421,     3,     3,
     408,   758,     0,   849,   733,     0,   735,   540,   540,   540,
     575,   545,     0,   581,   569,     0,   421,   532,   567,   563,
       0,     0,     0,     0,   570,   572,   759,   583,   583,     0,
     564,   579,   421,   367,     0,     0,    62,   291,   292,   289,
     290,     0,     0,     2,   219,     0,     0,   221,   375,   220,
     501,   421,     2,     0,   179,   257,     0,   252,   179,   284,
     282,     0,   276,   992,   285,     0,     0,     0,   323,   324,
     325,   326,     0,   316,     0,   317,   293,     0,   294,     0,
       0,   421,   210,   197,   280,   279,     0,   314,   333,     0,
     365,   759,   421,   731,   692,   421,     2,     2,   966,   967,
     968,     0,   919,   421,     3,     3,     0,   927,     0,     0,
       0,     0,   600,     0,   588,     3,    93,     0,    31,   421,
       0,   992,     0,     0,    80,     0,    68,     0,    74,     0,
      72,    39,   153,   895,   421,     0,     0,   800,   818,   421,
     421,     0,     0,     0,   421,   421,   668,     0,   388,   390,
       3,     3,     0,     0,     0,   737,   536,   538,   534,     0,
     934,     0,   576,   939,   578,   931,   759,   759,   562,   582,
     566,     0,   565,     0,     0,     0,   585,     0,   759,   559,
     573,   584,   574,   580,   620,   624,   623,     0,     2,     0,
       0,   240,     2,   222,   501,   248,   258,     0,   273,   274,
     275,   272,   261,     0,     2,   421,     0,   277,     0,     0,
       2,   300,   327,     0,   318,     2,     0,     0,     0,     0,
     305,     0,   301,   195,   194,   389,   707,     0,     0,   969,
       3,     0,     0,   926,   928,   590,     0,   992,   603,     2,
      37,    35,    36,     0,    58,   172,    71,     0,     0,     3,
     801,   819,     3,     3,   866,   881,   392,     2,   610,     3,
     609,   667,     0,     0,   792,   850,   900,     0,     0,     0,
     935,   936,   759,   561,   932,   933,   560,   541,     0,     0,
     209,   299,     0,     0,     0,   233,     2,   211,     0,     0,
       2,   242,     2,   179,   266,     0,   262,     0,   259,   250,
     253,   179,     0,     0,   214,   298,     2,   421,   295,     0,
       0,   343,     2,   303,     0,    61,     0,   315,   712,   714,
       0,   929,   930,   992,     0,   705,    59,    75,    73,     0,
       0,     0,   421,     0,   793,   851,   759,   942,   944,   937,
       0,   571,   228,   223,   226,     0,   225,   232,   231,     0,
     421,   235,   234,     2,   244,     0,   241,     2,   249,   254,
     263,   274,   272,     0,   179,     0,     2,   256,   286,     0,
     421,   421,     3,   328,   422,   332,     0,   336,     0,     0,
       0,   344,   345,   217,   306,     0,     2,     0,     2,     2,
     920,     0,   593,   896,   867,   882,   614,     2,   938,   940,
     941,   577,     0,   230,     0,   229,   213,   236,   421,   356,
     245,     2,   246,   243,   268,   267,   264,   255,   260,   251,
     216,   236,     3,   321,     0,   934,   329,   330,   331,   343,
       0,     0,     0,   343,     0,     2,   304,   311,     0,   308,
     310,   592,   421,   224,   227,     2,     3,   237,   357,   247,
       0,     0,     0,     3,   321,     0,     0,   935,     0,     0,
       0,   337,     0,   346,   218,     0,   301,     0,     3,   205,
       0,     0,     2,   270,   271,   269,   265,     0,     0,   322,
       0,   349,     0,   347,     0,   349,   307,   309,     2,     0,
       0,   207,   206,   212,     0,   215,     0,   319,   350,     0,
       0,   338,     0,   312,     2,   943,   320,     0,     0,     0,
       0,   313,   351,   352,     0,   348,   339,     0,     0,   340,
     353
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1714,  5328,  5707, -1714,    -1,   550,   579,     7, -1714,  1641,
   -1714,   400, -1714,  -679,   680,   780,  -925, -1049, -1714,   260,
    6554,  1539, -1714,   415, -1714,  1362,   667,   805,   806,   642,
     810,  1314,  1327,  1328,  1326,  1329, -1714,    -5,  -158,  8150,
     908, -1714,  1633, -1714, -1714,  -651,  2787, -1084,  3212, -1714,
     -87, -1714,   900,    73, -1714, -1714, -1714,   465,   143, -1714,
   -1433, -1495,   343,   129, -1714, -1714, -1714,   350,   264, -1714,
   -1714, -1714, -1714,    89, -1713,   242, -1714, -1714,    95, -1714,
   -1714, -1714,   108,   504,   505,   192, -1714, -1714, -1714, -1714,
    -599, -1714,   132,    96, -1714,   210, -1714,  -123, -1714, -1714,
   -1714,   919,  -738,  -920, -1305, -1714,    12, -1034,    67,  6149,
    -879,  -780, -1714,  -273, -1714,    26,  -148,   174,  -139,  -233,
    3510,  6522,  -617, -1714,    75,   214,   382,   213, -1714,  2037,
   -1714,    77,  3629,  -296, -1714, -1714,     3, -1714, -1714,  1765,
     248,  4358,  2690,   -36,  1841,  -249, -1714, -1714, -1714, -1714,
   -1714,  -132,  2492,  4744, -1714,  -354,   278, -1714,   580,   309,
   -1714,   252,   768, -1714,   571,    -4, -1714, -1714, -1714,  5132,
    -622, -1138,  -709,  -516,  -302,  1163, -1714, -1167,  -155,   148,
    1757,   939,  7836,  -279,  -483,  -254,  -166,  -436,  1311, -1714,
    1629,  -215,  1229,  1519, -1714, -1714, -1714, -1714,   348,  -154,
     104,  -868, -1714,   208, -1714, -1714,   682,   514, -1714, -1714,
   -1714,  2111,  -757,  -433,  -890,   -23, -1714, -1714, -1714, -1714,
   -1714, -1714,   359,  -846,  -124, -1662,  -182,  7739,   -58,  6212,
   -1714,  1194, -1714,  2155,  -219,  -202,  -196,  -191,     5,   -55,
     -49,   -28,   241,   -43,   -14,    17,  -168,   -62,  -159,  -136,
    -121,  -728,  -628,  -611,  -608,  -708,  -113,  -594, -1714, -1714,
    -693,  1382,  1383,  1385,  1818,  7010,  -557,  -567,  -554,  -552,
    -686, -1714, -1580, -1616, -1597, -1595,  -592,    21,  -227, -1714,
   -1714,   -17,   159,   -44, -1714,  7785,   374,   706,  -384
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1137,   213,   382,   383,    80,    81,   384,   359,   385,
    1428,  1429,   386,   957,   958,   959,  1244,  1245,  1246,  1440,
     408,   388,   389,   390,   668,   669,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1056,   670,
    1366,   731,   207,   733,   404,   798,  1138,  1139,  1140,  1141,
    1142,  1143,  1144,  1970,  1145,  1146,  1371,  1545,  1844,  1845,
    1785,  1786,  1787,  1946,  1947,  1147,  1556,  1557,  1703,  1148,
    1149,  1150,  1151,  1152,  1153,  1379,  1721,  1889,  1817,  1154,
    1155,  1573,  1956,  1574,  1575,  1872,  1156,  1157,  1158,  1369,
    1880,  1881,  1882,  1999,  2014,  1907,  1908,   284,   285,   859,
     860,  1110,    83,    84,    85,    86,    87,    88,   441,    90,
      91,    92,    93,    94,   221,   558,   443,   412,   444,    97,
     294,    99,   100,   101,   324,   325,   104,   105,   166,   106,
     878,   326,   152,   109,   241,   110,   153,   250,   328,   329,
     330,   154,   405,   115,   116,   332,   117,   549,   848,   846,
     847,  1517,   333,   334,   120,   121,  1106,  1334,  1523,  1524,
    1661,  1662,  1335,  1512,  1680,  1525,   122,   632,  1610,   335,
     630,   912,  1049,   449,   450,   852,   853,   451,   452,   854,
     337,   553,  1162,   414,   415,   208,   469,   470,   471,   472,
     473,   313,  1182,   314,   876,   874,   583,   315,   353,   316,
     317,   416,   124,   172,   173,   125,  1176,  1177,  1178,  1179,
       2,  1095,  1096,   575,  1171,   126,   304,   305,   252,   262,
     532,   127,   211,   128,   222,  1058,   839,   499,   164,   129,
     643,   644,   645,   130,   224,   225,   226,   227,   299,   132,
     133,   134,   135,   136,   137,   138,   230,   300,   232,   233,
     234,   766,   767,   768,   769,   770,   235,   772,   773,   774,
     736,   737,   738,   739,   500,   139,   607,   608,   609,   610,
     611,   612,  1664,  1665,  1666,  1667,   597,   454,   340,   341,
     342,   417,   199,   141,   142,   143,   344,   790,   613
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   493,   529,    79,   111,  1180,   131,   336,   516,  1816,
     557,   914,   966,  1163,   322,   181,   148,   485,   183,   354,
     403,   673,   901,   486,   184,   946,   297,    95,   487,   787,
     186,   832,   834,   231,   887,   176,  1025,  1249,  1360,   190,
     497,   475,  1001,  1767,   280,   185,  1547,   888,   895,   889,
     350,   488,   939,  -715,    79,    79,  1026,    79,  1847,   187,
     489,   111,  1768,   131,  1769,   198,  1256,  1419,    89,   155,
     658,   149,    79,  1052,   615,  1903,   102,   493,   107,  1029,
    1771,    79,   144,   490,    95,  1036,  1101,   196,  1853,    79,
     188,  1067,   836,   485,    79,   289,   674,    79,   491,   486,
     228,    79,   843,   253,   487,   260,   505,   263,   564,   566,
      57,   625,   513,  1172,   256,   628,  1480,  1481,  1342,  1343,
    1846,  1159,   292,  1911,   478,    89,   420,   488,   279,  1290,
     198,   527,   421,   102,  1019,   107,   489,   634,   423,    79,
     636,   537,    79,   870,    79,  1537,  -716,   436,   111,    79,
     131,  1020,   916,   422,  1021,    79,   209,  1169,   494,   490,
     140,   483,    79,   140,   183,   246,   658,   424,  1022,   257,
     184,    95,    57,   402,   491,    96,   186,   209,   150,    79,
      79,    57,   592,   339,   358,   196,  1329,  1291,  -358,  1854,
    1839,   185,  1442,   498,    79,   887,   615,   561,   425,   623,
     457,  1782,  1783,   626,   466,   187,  1318,   895,   888,    79,
     889,  1321,    89,  1848,   156,   103,   279,   140,    79,    79,
     102,  1292,   107,   896,  1912,   196,  -741,  1330,   522,  1904,
    1905,  1579,    96,  1344,   494,    79,   188,   569,   544,    19,
     183,  1214,  1846,   823,    79,   210,   184,  -741,   713,   112,
     196,   278,  1577,  1816,    79,   498,   194,    79,  -358,  1005,
     162,   140,   279,   533,    79,  1050,  1050,   185,  1347,  1237,
     161,   868,   103,   819,    79,    79,  1547,    79,  -359,  1539,
    1612,   805,  1389,  1784,  1050,   522,   111,   806,   672,   863,
     714,  1209,   807,  1927,    79,    79,  1276,  1163,  1580,   175,
     592,   196,    79,   496,   140,   791,   112,  1263,  1767,    79,
      79,   249,   248,   287,    79,   808,  1277,  1548,  1548,    96,
    1228,   194,   269,   197,   809,   530,  1331,  1768,   177,  1769,
     771,  1578,  1029,  1782,  1783,   964,   229,  1200,  1447,   254,
     453,  1319,   598,   264,   615,  1771,    79,   810,  -359,   357,
    1050,    79,   819,  1852,    79,   642,   565,  1301,   560,   103,
     107,   178,   811,   882,   249,   434,   565,   805,   615,   922,
    1448,   924,   925,   806,   926,   615,  1346,  1839,   807,   278,
     928,   426,  1030,   930,   931,   932,  1033,  1878,   830,  1019,
     604,   758,   198,   112,   835,  1046,  1047,  1626,  1628,  1630,
     907,   808,  1329,  1329,  1329,  1159,  1020,  1098,   249,  1021,
     809,   842,  1945,  1902,   274,  1803,   518,   826,    79,   521,
     587,   197,   829,  1268,   457,   322,  1945,   190,   420,  1505,
     820,  1988,   680,   810,   421,   911,   458,   681,   179,   837,
     423,    79,    79,  1330,  1330,  1330,  1127,  1292,   811,   844,
     887,   880,  1972,    79,    79,   422,   201,    96,   587,   191,
     598,   197,    79,   888,   466,   889,   580,  1200,  1339,   424,
    1697,   249,   531,   538,  1706,   900,   521,  1480,  1481,  1051,
    1051,   202,    79,   941,   550,   266,   197,  1340,   906,   267,
      57,    79,   270,  1865,   272,   581,   582,   457,  1051,   534,
     425,   249,    13,    14,    15,    16,    17,   249,  1408,   820,
      57,    79,  1852,   682,   420,   216,  1625,    79,   683,   204,
     421,   201,   243,     6,     7,     8,     9,    10,    11,    12,
     205,   112,    57,    62,    63,  1704,   249,   248,    -3,  1852,
    1705,   422,  1331,  1331,  1331,   157,   206,  1548,   158,   159,
    1050,   160,   869,   418,   146,    79,   236,    79,  1258,  1886,
      57,   672,    57,   260,  1051,   111,   672,   941,    79,   506,
      79,  -415,   672,   498,    79,  1378,   457,  1952,   111,  1415,
     131,    75,    57,   194,    79,  1529,  1472,   504,    79,   572,
     509,   672,  1887,   498,   339,   179,   901,  1339,   276,   179,
    1451,    95,   615,  -875,  1530,   170,   170,  -537,   857,   757,
     279,   749,   526,  1546,  1558,   498,  1590,  1040,  1797,   523,
      79,  -419,   536,  1060,   274,    13,    14,    15,    16,    17,
     266,   868,    79,   278,   615,   598,    57,   560,  1341,   107,
     170,  1006,    89,    57,  1810,   498,    57,   293,  1673,  1811,
     102,   771,   107,  1669,  1185,   703,   704,  1282,   249,   248,
     458,   426,  1535,   498,    57,  1896,   884,    57,   453,  -406,
    1529,  1548,  1670,   279,  1932,  1678,   523,   941,  1069,  1933,
      79,   544,    79,    57,    79,  -358,    57,   352,    79,  1672,
     170,    79,  -406,   170,  1679,   600,  1707,  1085,  1397,   705,
     706,  1772,  1747,   311,  1748,  -730,   170,   266,   267,  1859,
     619,  1566,   272,   348,   179,  1027,    79,  1867,  1678,   602,
    1773,  -643,  1034,   990,  1232,  1077,   602,   355,    57,   498,
     150,  1233,   249,   458,   140,    57,    96,  1776,    57,   453,
    1089,   584,  1184,  1081,  -420,   585,  1304,   498,   941,    96,
     498,  1097,   249,  1208,  1099,  1252,    57,   356,  1102,    57,
     170,    79,  1248,    79,  1051,  1308,   357,  1780,   427,   498,
     402,  1420,   249,   248,   236,    79,   189,    63,   652,   428,
    1917,  1055,    79,   201,   467,   322,   696,  1623,   466,   103,
    1984,    79,  1203,   697,   698,  1985,   429,  1437,   430,  1289,
      79,    79,    79,   693,   694,   170,   249,  1406,   588,   274,
     112,   602,  1439,   600,  1457,   170,  1548,  1466,   498,  1248,
      79,   498,   431,   112,   693,   547,   170,   432,   552,  1086,
     249,   531,   543,    63,   884,  1470,  1548,   249,  1897,   602,
     456,    61,   498,  1546,   189,    63,    64,    65,    66,    67,
      68,    69,    70,   170,   693,   715,    79,    79,   466,   716,
     170,   170,   111,   460,   827,   170,   453,    72,   461,   741,
     249,   269,    61,   742,  1548,   217,   218,    64,    65,    66,
      67,    68,    69,    70,  1173,    95,   838,   734,   418,   418,
      74,   498,   841,   784,   474,  1296,   845,   170,    77,    78,
      72,   476,   170,  1314,    79,   170,   402,   453,    79,   479,
    1275,   771,    79,   266,   868,   792,   793,   480,   642,   794,
    1520,    74,   481,  1563,   753,    72,    89,  1521,   498,   453,
     453,    77,    78,   482,  1161,   934,   107,   237,   238,  1174,
     239,  1430,   495,   615,   240,   601,   935,   936,   453,   602,
     496,   856,   514,    79,   339,   857,    77,   603,   278,   915,
     426,    79,   498,   585,  1410,  -416,   506,  1558,   815,   604,
     498,   515,  1511,   525,    13,    14,    15,    16,    17,    72,
     157,  1479,  1253,   158,   159,   629,   160,   917,   902,  1737,
      79,   585,   170,   466,    13,    14,    15,    16,    17,   601,
    1621,   535,   918,   602,   170,   170,   919,   554,   940,   418,
      77,    78,   941,  1320,   453,   572,    79,   426,   140,   498,
    1010,   209,    79,    79,   498,  -417,  1345,   576,   354,   354,
      72,   140,    57,    96,    13,    14,    15,    16,    17,   699,
     700,  1064,   785,  1364,   467,  1065,  1175,   707,   708,   900,
    1659,  1390,    57,    79,   498,  1027,   590,   426,  1055,   602,
    1500,    77,    78,   278,   249,   248,   506,   498,   322,   622,
     498,   701,   702,   103,   646,   249,   531,    61,  1549,  -874,
     168,   169,    64,    65,    66,    67,    68,    69,    70,  1336,
    -587,  1091,    57,   633,    61,   941,   249,   248,   868,    64,
      65,    66,    67,    68,    69,    70,   146,   112,   191,   676,
     968,   635,  1909,  1527,  1823,   466,   418,   111,    79,    79,
      79,   243,     6,     7,     8,     9,    10,    11,    12,  1482,
     943,   944,  1909,  1488,  1489,   170,  1885,  1093,   647,   879,
      95,   941,   466,   572,    61,   650,   111,   498,    79,    64,
      65,    66,    67,    68,    69,    70,    79,  1247,   651,    79,
      79,  1248,  1204,    79,   260,   253,   263,   590,   676,    95,
    1948,   904,    72,   256,    79,  1396,  1698,  1699,  1700,   742,
     265,    89,  1425,   170,  1042,  1043,  1248,   655,   941,  1161,
    1618,   107,  1017,    74,  1619,  1689,   602,   676,  1701,   941,
      79,  1100,   695,    77,    78,   710,   466,  1702,  1709,   598,
      89,  1710,   941,  1711,   453,  1065,    79,   941,  1161,   709,
     107,  1777,  1698,  1861,  1700,   742,  1855,   246,   257,   712,
     941,   950,   466,   952,   941,   955,   711,   339,   717,   963,
      79,  1936,   967,   743,  1862,  1248,  1986,   418,   744,  1452,
     941,   433,   322,  -180,    61,  1538,  1540,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   745,   992,  1044,  1045,
    1528,   746,    79,   140,  -116,  -116,  -116,  -116,  -116,  -116,
    1332,  1568,  1569,  1570,  1571,  1572,   402,   402,    96,   747,
    2010,  1235,  1065,  1588,  2007,  1527,  -115,  -115,  -115,  -115,
    -115,  -115,   140,   493,    -3,  1336,  1336,  1336,  1549,  1513,
    1336,    13,    14,    15,    16,    17,   938,    96,   485,  2017,
     140,  1250,  1251,  2018,   486,   748,   170,   775,   103,   487,
    1958,    79,  -418,   170,  1962,    79,   910,   148,    79,   -17,
    1430,   788,  1759,   789,  1222,   976,   977,   978,   979,  1226,
     812,  1068,   488,  1070,   111,   111,   799,   103,   466,   813,
    1234,   489,   112,   969,   970,   971,   941,  1254,   467,  -151,
    -151,   785,    18,   249,   248,  1656,  1657,  1658,   466,   814,
      79,  1322,  1323,  1324,   490,   816,   533,  1044,  1388,  1445,
    1446,   112,   149,    13,    14,    15,    16,    17,  1227,   491,
     822,   254,   264,  1450,  1446,   249,   531,  1109,   170,   170,
      47,    48,    49,    50,    51,    52,    53,    54,    89,    89,
     817,   339,  1454,  1446,  1016,  1438,  1551,  1551,   107,   107,
     824,   453,   453,  1490,  1438,   466,  1173,  1482,  1814,  1815,
      79,  1016,  1502,   818,   322,    79,    79,    79,   530,  1631,
    1065,   286,  1528,  1782,  1783,  1202,  1745,  1065,   819,   170,
     494,   840,  1674,   170,   243,     6,     7,     8,     9,    10,
      11,    12,   805,  1746,  1446,  1756,  1757,  -535,   806,  1766,
     941,  -533,   637,   807,   140,  1712,  1827,  1446,  1828,  1446,
    1482,  1174,   849,  1059,  2007,  2008,  1332,  1332,  1332,   150,
    1510,  1514,  1443,  1444,   972,   973,   808,   974,   975,   858,
     140,   140,   871,    79,   873,   809,   877,   980,   981,    79,
     890,    79,   892,  1681,  1681,    96,    96,   604,    79,   652,
    1398,  1399,   909,  1243,   911,   920,   913,   921,   810,   942,
      82,  1243,   466,   147,   945,   989,   249,   948,   994,  1015,
    1016,   466,  1023,   811,   260,   418,   638,  1062,  1071,  1072,
    1527,  1073,   902,   256,  1074,   103,   103,  1075,  1076,  1092,
    1243,   639,  1094,   467,   640,   641,    64,    65,    66,    67,
      68,    69,    70,   140,   111,  -719,  1103,  1718,   466,  1104,
    1105,  1164,  1181,  -619,   249,   531,  1165,    82,  1175,   112,
     112,  1194,  1195,   693,  1819,  1196,  1206,  1173,  1212,  1211,
      79,  1217,   180,   339,  1215,   820,  1218,   246,   257,  1219,
    1210,    82,   534,  1238,  1843,   615,    79,  1220,    79,  1221,
    1223,  1224,  1225,  1243,   220,  1230,  1231,   245,  1431,  1432,
    1433,    82,  1239,  1255,  1260,  1434,  1435,  1261,    89,  1262,
    1269,  1270,  1271,  1272,  1295,  -607,  1551,  1280,   107,  -606,
    1303,  1315,  1174,  -720,  1337,  1348,   170,  1338,  1873,   170,
     170,   170,  1368,    79,  1351,  1352,    79,  1482,   147,  1361,
    1685,  1362,  1363,  -642,    82,  1370,   147,   466,  1372,   296,
     302,   466,   941,   170,  1378,  1384,  1382,  1385,  1386,   170,
    1422,   321,  1392,   466,  1423,  1394,   493,   552,   111,   466,
     140,  1436,  1438,  1465,   170,  1453,  1483,  1528,   409,   180,
     180,  1478,   485,  1484,  1809,  1485,    79,    79,   486,  1486,
     147,   439,  1873,   487,   245,    79,  1446,  1491,   819,  1494,
     140,  1503,  1504,  1506,  1943,  1516,  1843,  1518,  1519,  1926,
    1341,   170,  1542,  1559,   140,    96,   488,  1581,   220,   220,
    1560,   530,  1583,   249,   248,   489,  1562,  1426,  1586,  1175,
    1593,  1564,    89,  1960,  1576,   296,  1584,   402,    79,  1591,
    1551,  1585,   107,  1594,    82,   466,   467,  1596,   490,   466,
    1875,   466,   453,   453,  1597,   103,  1598,   245,  1599,  1600,
    1602,  1607,  1614,   491,  1616,  1611,  1617,  1620,  1624,  1632,
    1646,   466,   467,  1633,  1637,  1375,  1638,   426,  1879,  1648,
    1243,  1490,  1688,  1655,  1722,  1521,  1668,   302,  1248,   112,
    1690,  1692,   210,   302,   296,   296,  1715,  1717,  1729,  1733,
      61,   147,  2009,   168,   169,    64,    65,    66,    67,    68,
      69,    70,   466,   111,  1875,  1734,   466,  1735,  1738,  1736,
     321,   605,   614,   494,   140,   466,   569,  1744,  1761,   183,
    1790,  1750,    79,   111,    79,   184,  1751,   321,  1754,    96,
    1755,   321,  1764,  1765,  1792,   466,   170,   466,   466,   170,
     418,  1802,  1793,  1997,  1806,   820,   185,   402,  1801,   402,
    1808,  1812,  1813,  1127,  1825,  -608,  1821,   249,   531,  2006,
     466,   111,  1822,  1376,   409,  1421,  1826,    89,  1834,   103,
    1835,  1836,  1837,    79,    79,  1551,   402,   107,  1838,   170,
     196,   498,  1857,  -518,   466,  1864,  1879,    89,   467,  1866,
    1879,  1879,  1876,  1890,   466,  1551,  1868,   107,   409,  1877,
    1979,   735,  1892,   112,  1893,  1757,    79,   447,   180,  1894,
    1895,  1906,   457,  1929,  1455,  1930,  1982,  1914,  1931,   466,
    1915,   466,  1920,  1935,   147,    89,  1934,  1938,   439,  1941,
     453,  1950,   764,  1551,   614,   107,  1955,   466,  1998,  1959,
    1951,  1090,  1998,   466,  1966,   402,  1961,  1967,  1980,  1981,
    1983,  1720,  1993,   466,  1995,  2004,  1996,    79,  2001,   140,
    2000,  2005,  2016,  2015,  2012,   467,  2019,    79,   679,  1741,
    1243,  1536,   220,   982,    96,  1243,  1243,  1243,  1449,   140,
     937,   220,    13,    14,    15,    16,    17,   983,   985,   984,
    1367,   732,   986,  1374,    96,  1994,  1719,  1944,   508,  1193,
    1953,   296,  1804,   409,   409,  1800,  1989,   296,  1888,   321,
    1863,  1987,  1978,  1922,   103,    61,  1963,   140,   168,   169,
      64,    65,    66,    67,    68,    69,    70,  1713,  1714,   170,
    1921,  2002,    96,  1383,   103,   167,   595,   249,  1841,   618,
      57,   524,  1901,   170,  1671,  1515,  1380,   296,   112,  1682,
    1061,   795,   170,   595,   875,  1183,  1615,   595,   296,  1726,
     296,     3,   321,  1213,    82,     0,   997,   998,   112,   999,
       0,  1609,   103,     0,     0,     0,  1613,     0,     0,   321,
     439,     0,   614,     0,     0,     0,     0,     0,  1353,   170,
     605,     0,     0,  1622,   605,     0,     0,     0,     0,    72,
       0,     0,     0,   321,     0,     0,   112,     0,     0,  1259,
       0,   170,     0,   614,     0,     0,   321,     0,     0,   601,
       0,     0,     0,   602,   147,     0,  1266,  1267,     0,     0,
      77,   603,    13,    14,    15,    16,    17,   409,     0,   147,
     147,     0,   409,     0,     0,     0,     0,     0,   409,     0,
       0,   147,   147,   147,   595,     0,  1243,     0,  1243,    13,
      14,    15,    16,    17,     0,     0,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,   182,  1884,
     740,     0,     0,     0,     0,     0,     0,     0,   170,     0,
      57,     0,   170,     0,    72,     0,   751,     0,     0,   754,
     223,     0,     0,    57,   170,     0,     0,   439,     0,     0,
     170,     0,     0,     0,  1520,    74,     0,    57,     0,  1708,
       0,  1521,     0,   735,   735,    77,    78,   170,     0,     0,
       0,   409,     0,     0,    61,   447,   170,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   439,    72,
       0,   764,     0,   764,     0,   298,   508,     0,   855,     0,
       0,     0,    72,     0,   684,     0,   685,   686,   687,   734,
     321,   321,     0,   498,     0,     0,    72,  1743,     0,     0,
      77,    78,   219,    74,     0,     0,   170,     0,   447,   321,
     170,   296,   170,    77,    78,   688,  1659,     0,   689,   690,
     498,     0,     0,   691,   692,   595,   447,    77,    78,     0,
     296,    61,   170,     0,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,   484,   223,     0,     0,    61,   595,
       0,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   298,   595,     0,     0,     0,     0,     0,   409,     0,
       0,     0,     0,   170,     0,   321,     0,   170,     0,     0,
      18,   147,   409,     0,     0,     0,   170,     0,     0,     0,
     321,     0,  1188,     0,     0,  1928,     0,  1461,  1462,    57,
       0,     0,     0,   605,  1355,     0,   170,  1357,   170,   170,
       0,  1476,  1477,  1824,     0,     0,     0,     0,     0,     0,
     570,   298,    51,    52,    53,    54,     0,     0,     0,     0,
      61,   170,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   439,     0,     0,  1498,  1499,     0,     0,     0,
       0,    61,     0,   447,     0,   170,    64,    65,    66,    67,
      68,    69,    70,   118,    61,   170,   118,     0,     0,    64,
      65,    66,    67,    68,    69,    70,   961,    61,  1273,    74,
     543,    63,    64,    65,    66,    67,    68,    69,    70,     0,
     170,     0,   170,     0,   447,     0,     0,     0,     0,  1891,
      74,     0,     0,   784,   306,   307,   308,   309,   170,   735,
       0,     0,     0,     0,   170,     0,   962,     0,     0,     0,
     118,     0,   740,   740,   170,     0,   764,     0,  2013,   991,
       0,     0,  1008,   764,     0,  1011,     0,     0,  2020,     0,
       0,     0,    61,     0,   118,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,   855,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   765,     0,
       0,     0,     0,     0,     0,   321,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,     0,
       0,     0,     0,     0,   310,     0,   508,     0,     0,     0,
    1079,   118,     0,     0,  1083,     0,     0,   118,   804,   118,
       0,     0,   311,   855,     0,   147,   595,   223,     0,   618,
       0,     0,     0,   409,     0,     0,     0,     0,     0,  1650,
    1651,     0,     0,     0,     0,     0,     0,   298,     0,    61,
      57,   118,     0,   298,    64,    65,    66,    67,    68,    69,
      70,  1240,   409,   118,     0,  1241,     0,  1242,     0,     0,
       0,   114,     0,     0,   114,     0,     0,     0,   447,   245,
      82,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   298,   296,     0,     0,     0,    74,     0,
     147,  1441,     0,     0,   867,     0,   298,   439,     0,    72,
       0,     0,     0,     0,   118,     0,     0,   118,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,   114,   219,
      74,     0,     0,   855,     0,   439,     0,     0,     0,   147,
      77,    78,     0,     0,     0,     0,  1730,     0,    57,     0,
     855,   855,   114,     0,    61,   118,     0,   345,   346,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   251,     0,
       0,     0,   114,     0,   118,     0,     0,  1749,     0,    61,
       0,     0,  1752,  1753,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,   740,     0,
       0,     0,   321,   321,     0,    75,     0,    72,     0,   114,
     347,     0,     0,     0,     0,   114,     0,   114,     0,     0,
       0,   251,     0,     0,     0,     0,     0,    73,    74,     0,
       0,   318,   114,   349,     0,     0,     0,     0,    77,    78,
       0,   147,   147,   147,   147,   147,   147,   118,     0,   413,
       0,  1522,   302,     0,     0,     0,     0,     0,     0,     0,
       0,   114,   413,     0,     0,   251,     0,     0,     0,  1306,
     409,   409,  1310,     0,     0,     0,     0,     0,     0,     0,
       0,   118,    13,    14,    15,    16,    17,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1018,     0,   765,
     245,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,   114,     0,     0,   114,     0,     0,     0,   439,
       0,     0,     0,     0,     0,     0,     0,     0,   251,     0,
       0,     0,     0,   595,     0,     0,     0,   298,     0,     0,
      57,    61,     0,   147,     0,   548,    64,    65,    66,    67,
      68,    69,    70,   114,     0,     0,   298,     0,   251,     0,
       0,   447,     0,     0,   251,     0,     0,     0,     0,     0,
       0,    61,   114,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,   118,   118,     0,  1273,
      74,   114,     0,   251,   114,     0,     0,     0,     0,    72,
       0,   855,   855,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   114,     0,     0,   855,   855,     0,     0,   762,
      74,     0,     0,   602,     0,     0,     0,     0,   118,  1660,
      77,   763,   118,  1522,   118,   409,     0,     0,     0,  1522,
       0,  1522,     0,     0,     0,   413,     0,   118,     0,   855,
     855,     0,     0,    57,    61,     0,     0,     0,  1459,    64,
      65,    66,    67,    68,    69,    70,   953,  1468,     0,   302,
     147,    13,    14,    15,    16,    17,     0,     0,     0,   413,
       0,     0,     0,     0,    61,  1968,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
     409,     0,     0,     0,     0,   114,   954,   118,     0,   413,
       0,   321,    72,     0,   147,   251,     0,     0,     0,     0,
     118,     0,   118,   118,     0,   118,     0,     0,     0,    57,
       0,   118,   295,    74,   118,   118,   118,     0,   147,     0,
       0,     0,   671,    77,    78,   447,     0,     0,     0,     0,
       0,     0,  1018,     0,     0,     0,     0,     0,  1274,   765,
      61,     0,     0,   321,   321,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,  1660,  1660,
       0,     0,     0,     0,   413,   413,     0,     0,    72,   251,
     114,     0,     0,  1522,    61,     0,  1522,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,    73,    74,
       0,     0,     0,   302,   118,     0,     0,     0,     0,    77,
      78,   114,     0,     0,   409,     0,   114,     0,     0,   251,
     114,     0,   114,   855,   855,     0,     0,    57,     0,     0,
       0,     0,     0,   114,     0,   114,    61,   296,     0,     0,
     578,    64,    65,    66,    67,    68,    69,    70,  1240,   349,
     114,   413,  1241,   251,  1242,     0,     0,     0,    61,  1686,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,   831,   833,     0,   114,     0,     0,   251,     0,  1660,
       0,   548,     0,     0,   251,    74,    72,   114,  1522,   908,
       0,     0,     0,     0,     0,   114,     0,     0,  1663,     0,
     298,     0,     0,     0,     0,     0,  1520,    74,   413,     0,
     114,   114,     0,   413,     0,     0,   147,    77,    78,   413,
       0,   118,   114,   114,   114,     0,     0,     0,     0,     0,
     855,     0,     0,     0,   118,   118,     0,     0,     0,     0,
       0,   321,     0,     0,     0,     0,     0,    61,     0,  1660,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   147,
       0,   855,     0,     0,     0,     0,   855,   855,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,   413,   147,
     147,    61,  1925,   302,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,  1924,    74,     0,  1473,   498,
       0,     0,   413,     0,     0,   671,    77,    78,     0,     0,
     671,     0,     0,     0,     0,     0,   671,   147,     0,   413,
      61,     0,     0,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,  1925,  1925,   671,     0,  1201,    57,     0,
       0,   114,   114,     0,     0,     0,     0,  1663,  1663,     0,
       0,     0,     0,     0,     0,     0,     0,  1526,     0,     0,
     114,     0,     0,     0,     0,     0,  1925,     0,   456,    61,
       0,   988,   217,   218,    64,    65,    66,    67,    68,    69,
      70,    98,     0,     0,   151,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    61,    72,     0,     0,
       0,    64,    65,    66,    67,    68,    69,    70,  1240,     0,
       0,   251,  1241,     0,  1242,     0,     0,  1924,    74,   413,
       0,   498,   251,     0,     0,     0,   114,     0,    77,    78,
       0,     0,   114,   413,     0,     0,     0,     0,    98,     0,
       0,   114,   595,  1190,   413,    74,   114,     0,  1627,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1663,     0,
       0,     0,   195,     0,     0,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,     0,
      61,     0,   258,   545,   546,    64,    65,    66,    67,    68,
      69,    70,     0,   413,    13,    14,    15,    16,    17,     0,
     108,     0,    61,     0,     0,   118,   595,    64,    65,    66,
      67,    68,    69,    70,  1240,     0,     0,     0,  1241,   288,
    1242,     0,     0,   118,  1899,    98,     0,     0,  1663,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,  1526,
       0,     0,   323,   118,     0,  1675,     0,  1526,     0,     0,
       0,    74,    57,     0,  1629,     0,   114,   108,     0,     0,
     419,  1663,     0,     0,     0,     0,     0,     0,     0,   855,
       0,   288,   445,   114,   114,     0,     0,     0,     0,     0,
       0,     0,   118,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,    61,
     492,   259,   168,   169,    64,    65,    66,    67,    68,    69,
      70,    72,  1663,  1663,     0,     0,   512,     0,     0,     0,
       0,   517,   519,     0,     0,   195,   114,     0,     0,     0,
       0,  1924,    74,     0,     0,   498,     0,     0,     0,     0,
       0,     0,    77,    78,   108,  1663,     0,   539,     0,     0,
     541,     0,   542,     0,     0,     0,     0,     0,     0,     0,
       0,   327,     0,   559,     0,     0,   114,     0,     0,     0,
       0,     0,     0,     0,   413,     0,   571,     0,     0,     0,
       0,     0,     0,     0,   118,   118,   118,   118,   118,   118,
       0,   446,     0,     0,     0,     0,     0,     0,     0,  1778,
       0,   593,  1526,   413,   617,     0,     0,     0,     0,     0,
       0,     0,     0,   118,   118,     0,     0,     0,   624,     0,
     251,   114,   624,     0,     0,     0,     0,     0,     0,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,   114,     0,     0,     0,     0,   657,     0,   413,     0,
       0,     0,  1190,   298,     0,     0,     0,    72,    13,    14,
      15,    16,    17,     0,  1418,     0,   540,     0,     0,     0,
       0,    13,    14,    15,    16,    17,   413,   762,    74,     0,
     114,   602,   108,     0,     0,     0,   118,    61,    77,   763,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
       0,   604,     0,     0,  1526,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   288,    57,     0,     0,   593,
     594,     0,     0,   259,   114,   114,     0,     0,     0,    57,
       0,     0,     0,     0,     0,   460,     0,   594,   114,   114,
       0,   594,   657,   114,   114,     0,     0,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   114,   114,     0,    72,     0,     0,   118,     0,
       0,     0,   114,   114,   114,   114,   114,   114,    72,     0,
       0,     0,     0,   251,     0,   295,    74,     0,   298,     0,
     445,     0,     0,     0,     0,     0,    77,    78,  1520,    74,
       0,   413,   413,   118,     0,     0,     0,     0,    61,    77,
      78,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,   851,     0,     0,     0,     0,   519,     0,   594,     0,
     862,   251,   559,   118,     0,     0,    72,     0,     0,   570,
     298,     0,     0,   323,     0,    98,     0,   118,     0,     0,
     413,     0,     0,     0,     0,     0,   219,    74,     0,     0,
     624,   883,     0,     0,     0,     0,     0,    77,    78,     0,
       0,   118,   298,     0,   114,   894,     0,     0,     0,     0,
       0,     0,     0,     0,   593,     0,     0,     0,     0,   903,
       0,     0,     0,     0,     0,  1541,     0,   624,  1544,  1555,
       0,     0,     0,     0,  1561,     0,     0,     0,  1565,   446,
    1567,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,    72,
     327,     0,     0,     0,     0,     0,   114,   114,     0,   259,
       0,   108,     0,     0,     0,     0,     0,   118,     0,   295,
      74,     0,   446,     0,   108,     0,   413,     0,     0,     0,
      77,    78,     0,   678,     0,     0,    75,   376,     0,   594,
     446,     0,   114,     0,     0,     0,    61,     0,   445,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     251,   114,     0,   594,     0,  1000,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   594,     0,   719,   720,
     721,   722,   723,   724,   725,   726,   727,   728,   729,   883,
       0,   413,   204,     0,  1024,     0,     0,    75,     0,     0,
       0,     0,   114,     0,     0,   114,     0,     0,     0,  1654,
       0,   445,   445,   114,     0,     0,     0,     0,     0,   730,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
     445,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1687,     0,     0,   114,     0,     0,     0,  1350,   114,
     114,     0,     0,  1693,   114,   114,     0,     0,   851,     0,
    1696,     0,   118,     0,     0,     0,     0,   446,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,   118,     0,     0,     0,     0,     0,  1544,  1160,
       0,     0,     0,     0,     0,     0,   445,     0,     0,     0,
       0,     0,   151,     0,   251,     0,     0,     0,   446,     0,
       0,   624,     0,     0,  1192,   413,   851,     0,     0,     0,
     118,  1198,     0,     0,     0,     0,     0,     0,     0,     0,
     327,   327,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   327,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   323,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   327,     0,     0,
     261,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1789,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,  1796,
    1798,     0,  1555,     0,     0,   327,     0,   114,     0,     0,
       0,     0,     0,   113,     0,     0,   851,     0,     0,     0,
     594,     0,     0,   259,     0,   327,     0,     0,     0,     0,
     331,     0,   114,   851,   851,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     448,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,   114,   446,     0,   251,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   445,     0,     0,     0,
       0,     0,  1860,     0,     0,     0,     0,     0,     0,     0,
       0,  1587,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1333,     0,     0,     0,
       0,     0,     0,     0,  1160,   327,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,   113,   327,   327,     0,     0,     0,     0,     0,     0,
    1916,     0,  1918,  1160,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1381,     0,     0,     0,     0,     0,     0,     0,   596,
       0,     0,   261,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   327,   596,     0,   593,     0,
     596,     0,     0,     0,     0,     0,     0,   517,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1965,   323,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1973,  1975,  1976,
       0,     0,     0,   108,     0,   119,     0,     0,   119,     0,
       0,     0,     0,     0,     0,  1691,     0,     0,     0,     0,
       0,     0,     0,     0,  1695,     0,     0,     0,     0,     0,
       0,     0,   108,     0,   851,   851,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   851,   851,
     259,     0,     0,   445,   445,     0,     0,   596,     0,     0,
       0,  1724,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   594,     0,     0,
       0,     0,   851,   851,     0,     0,   119,     0,     0,     0,
       0,     0,  1333,  1333,  1333,   151,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   446,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1550,  1550,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   448,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,   119,
       0,   119,     0,   327,   327,     0,     0,     0,     0,     0,
    1781,     0,     0,     0,  1791,     0,     0,   327,   327,   331,
     323,     0,   327,   327,     0,     0,  1799,     0,   261,     0,
     113,     0,  1805,   119,     0,     0,     0,     0,     0,     0,
       0,   448,     0,   113,   151,   119,     0,     0,     0,     0,
       0,   327,   327,     0,     0,     0,     0,     0,   596,   448,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   596,     0,     0,     0,     0,     0,     0,     0,
     108,   108,     0,     0,     0,   596,   119,     0,     0,   119,
       0,     0,     0,     0,   119,     0,     0,     0,  1851,     0,
       0,     0,  1856,     0,  1858,     0,   851,   851,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1883,     0,     0,   119,     0,   446,
       0,     0,  1677,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   851,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1694,     0,     0,     0,  1910,     0,     0,     0,  1913,
       0,     0,     0,     0,     0,     0,   448,     0,  1919,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1550,     0,     0,     0,     0,     0,     0,  1937,     0,
    1939,  1940,   323,     0,     0,   151,     0,     0,     0,     0,
       0,     0,     0,   851,     0,     0,     0,   448,     0,   119,
       0,     0,     0,  1949,     0,   327,   327,     0,     0,     0,
       0,     0,     0,   123,     0,     0,   123,     0,     0,   331,
     331,     0,     0,     0,   851,     0,     0,  1964,     0,   851,
     851,     0,     0,   119,   445,   445,     0,  1969,   331,     0,
       0,   327,     0,     0,     0,     0,     0,     0,     0,     0,
    1770,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     259,     0,  1992,     0,  1969,     0,   331,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2003,     0,     0,     0,     0,     0,  1992,     0,     0,     0,
     108,     0,     0,     0,   123,  1550,  2011,   113,     0,     0,
       0,   327,     0,     0,   331,     0,     0,     0,     0,     0,
       0,     0,   327,     0,   123,     0,     0,     0,     0,   596,
       0,     0,   261,     0,   331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,   119,
       0,     0,     0,   327,     0,     0,     0,     0,   327,   327,
       0,   123,     0,   327,   327,     0,     0,   123,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   448,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,   119,     0,   119,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,  1874,     0,   119,
       0,     0,     0,   123,     0,     0,     0,     0,     1,     0,
       0,   145,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,   445,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   331,     0,     0,     0,     0,     0,
    1550,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,   331,     0,   123,     0,     0,   123,     0,   119,
    1550,  1874,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   119,   119,     0,   119,     0,     0,
       0,     0,     0,   119,   192,     0,   119,   119,   119,     0,
       0,     0,     0,     0,     0,   123,     0,     0,  1550,     0,
       0,     0,     0,     0,   331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,  1957,   594,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   851,     0,     0,     0,     0,     0,     0,     0,
       0,   327,     0,   283,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
     594,   113,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   261,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,   123,     0,     0,     0,     0,   596,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   283,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,   327,   520,     0,   448,     0,     0,     0,     0,     0,
       0,     0,   283,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   283,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,   551,   555,     0,     0,
       0,     0,     0,   562,   563,     0,   119,   119,     0,     0,
       0,     0,   331,   331,     0,     0,     0,     0,     0,   573,
       0,     0,     0,     0,     0,     0,   331,   331,     0,     0,
       0,   331,   331,     0,     0,     0,   123,   123,   591,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,   123,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   677,     0,     0,   123,     0,   113,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   718,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   448,     0,
       0,   756,     0,     0,     0,   759,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,   123,   123,   781,   123,     0,     0,   782,   783,
       0,   123,   786,   203,   123,   123,   123,     0,     0,   214,
     215,     0,     0,     0,     0,     0,     0,   800,   801,   802,
     803,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   825,     0,     0,     0,
       0,     0,     0,   277,   828,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,   331,   331,     0,     0,   119,     0,
       0,     0,   283,     0,     0,     0,     0,     0,  1842,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
     331,     0,     0,   866,     0,     0,     0,     0,     0,     0,
     551,     0,     0,     0,     0,   119,   872,     0,     0,   261,
       0,     0,     0,     0,     0,   360,     0,     0,     0,   361,
       0,   362,     0,     0,     0,   119,     0,     0,     0,   886,
     891,     0,     0,     0,     0,     0,     0,     0,   363,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,     0,     0,   119,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,   123,   331,   933,     0,     0,    72,   331,   331,   567,
       0,     0,   331,   331,   123,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   996,   113,     0,     0,   119,   119,   119,   119,
     119,   119,     0,     0,     0,     0,     0,  1013,     0,     0,
       0,  1014,     0,     0,     0,     0,     0,     0,     0,     0,
     886,     0,     0,     0,     0,   119,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1054,     0,     0,     0,     0,     0,     0,     0,
       0,  1063,     0,     0,     0,     0,     0,  1066,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   760,     0,   761,     0,     0,
       0,     0,     0,     0,     0,     0,   777,   778,     0,     0,
       0,     0,     0,     0,     1,   596,     0,     0,   119,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,   596,
       0,   193,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,  1216,     0,     0,     0,     0,     0,     0,     0,   165,
     119,     0,     0,     0,   861,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,   123,     0,   119,   193,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   193,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   193,     0,
       0,   165,     0,     0,  1264,     0,     0,     0,  1265,   119,
       0,   442,     0,     0,   165,   886,   165,     0,     0,     0,
       0,     0,   123,     0,     0,  1278,     0,     0,     0,     0,
       0,     0,  1279,   119,     0,     0,     0,     0,     0,     0,
       0,  1283,     0,  1284,     0,     0,   351,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   351,   193,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1312,     0,     0,     0,  1313,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   145,     0,     0,     1,     0,     0,     0,
       0,   165,     0,     0,     0,   165,     0,     0,   165,   165,
       0,     0,   165,     0,     0,   165,   165,     0,     0,   119,
       0,   193,     0,     0,   123,   123,   123,   123,   123,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1039,   123,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
     165,     0,     0,     0,  1400,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,  1424,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,   193,  1107,
    1108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1166,  1167,  1168,     0,     0,  1170,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   268,  1496,   271,     0,   273,  1497,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,   119,     0,  1236,     0,     0,     0,     0,     0,
       0,     0,     0,   193,   193,     0,  1532,     0,     0,   442,
       0,     0,     0,   247,     0,   271,   273,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   351,     0,     0,     0,     0,
    1257,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,   123,     0,     0,  1592,   247,     0,  1595,
       0,     0,   193,     0,     0,     0,     0,   123,     0,     0,
       0,     0,   387,     0,  1603,     0,     0,     0,     0,     0,
     442,     0,     0,     0,     0,     0,     0,  1281,     0,     0,
       0,   123,     0,     0,     0,     0,  1285,  1286,  1287,  1288,
       0,     0,     0,   193,  1293,  1294,     0,     0,     0,     0,
       0,     0,     0,     0,  1302,     0,     0,     0,     0,     0,
     247,   351,   271,   273,   193,  1634,     0,     0,     0,     0,
       0,     0,     0,     0,  1639,  1316,     0,  1317,  1640,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,  1644,  1645,     0,     0,   247,     0,     0,     0,
       0,   165,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
    1373,   620,     0,   273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   442,     0,     0,
       0,     0,     0,     0,     0,     0,  1387,     0,     0,     0,
       0,     0,     0,  1391,     0,  1393,  1395,     0,     0,     0,
       0,   193,     0,     0,  1401,     0,  1402,     0,  1403,     0,
    1405,     0,     0,     0,     0,  1413,     0,     0,   442,     0,
       0,     0,     0,     0,   649,     0,     0,   387,   654,     0,
       0,     0,     0,     0,  1727,  1728,     0,   660,   661,     0,
     442,   442,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,   387,   387,     0,     0,     0,     0,     0,   442,
     165,   165,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,   387,     0,     0,  1456,   247,     0,   620,
     273,     0,     0,  1463,  1464,     0,     0,   165,     0,     0,
     165,   165,   123,   165,     0,   165,   165,     0,     0,     0,
       0,     0,     0,   387,     0,     0,     0,  1487,     0,     0,
       0,     0,   123,     0,  1492,     0,     0,     0,  1493,     0,
       0,     0,     0,     0,   247,   442,     0,     0,     0,     0,
       0,     0,   193,     0,   165,     0,     0,     0,   165,     0,
       0,     0,     0,     0,   247,     0,     0,     0,   214,   247,
     123,   247,     0,  1807,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,   247,   247,     0,     0,  1595,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1582,     0,
       0,   247,   193,     0,     0,  1832,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,     0,     0,     0,
       0,  1601,  1850,     0,     0,   247,     0,   620,   273,  1606,
       0,  1608,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1870,     0,     0,  1871,     0,     0,     0,     0,   247,
     620,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1635,  1636,
       0,     0,   338,     0,     0,     0,     0,     0,     0,   247,
     268,     0,     0,  1641,  1642,     0,  1643,     0,     0,     0,
       0,     0,     0,     0,     0,  1647,     0,     0,     0,     0,
       0,   435,   338,     0,     0,  1652,  1653,     0,     0,     0,
       0,     0,     0,     0,     0,   442,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1942,     0,     0,     0,     0,
       0,     0,     0,   501,     0,   165,     0,     0,     0,     0,
     501,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,   165,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   501,     0,
       0,  1731,  1732,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1739,     0,     0,     0,     0,     0,     0,   387,
     193,   338,   606,     0,     0,     0,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   627,     0,     0,     0,     0,  1762,  1763,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,   165,     0,     0,     0,
       0,     0,     0,     0,   165,   165,   247,     0,     0,     0,
       0,     0,   501,     0,     0,   247,     0,     0,     0,     0,
       0,     0,   442,   442,     0,     0,     0,  1820,   501,   752,
       0,   501,   755,     0,     0,     0,     0,     0,     0,   338,
       0,     0,     0,   606,     0,     0,  1829,     0,     0,  1830,
    1831,     0,     0,     0,     0,   387,  1833,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,   387,   165,     0,   165,   165,   387,     0,
       0,     0,     0,     0,   501,     0,     0,     0,   501,   387,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,   165,     0,     0,
     338,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,     0,     0,     0,     0,   247,     0,     0,   193,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1923,
     501,     0,     0,   338,     0,   360,     0,     0,     0,   361,
       0,   362,     0,     0,   165,     0,     0,     0,     0,     0,
     881,   338,     0,     0,     0,     0,     0,     0,   363,     0,
       0,   606,     0,     0,     0,   606,     0,     0,     0,     0,
       0,     0,   899,     0,   338,     0,     0,     0,     0,  1954,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,  1794,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,  1971,   370,   371,   372,     0,   373,   374,
    1977,     0,     0,     0,     0,   193,    72,     0,   387,     0,
       0,     0,     0,     0,     0,  1990,     0,   165,     0,     0,
       0,     0,   247,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,   165,     0,     0,     0,
    1795,  -179,     0,     0,   247,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,   338,     0,
       0,     0,   165,     0,     0,     0,     0,     0,   165,     0,
       0,   193,   387,     0,   501,   501,     0,     0,     0,     0,
       0,     0,     0,     0,   501,  1009,     0,   501,  1012,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   338,
       0,     0,   606,     0,   606,   606,     0,   387,   387,   387,
       0,   606,     0,     0,   387,   387,   163,     0,     0,     0,
       0,   338,   338,   442,   442,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,   387,     0,
     338,     0,     0,     0,   501,     0,     0,     0,   501,     0,
       0,     0,   501,  1080,     0,     0,   501,  1084,     0,     0,
       0,     0,     0,     0,  1087,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   387,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,   275,     0,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
       0,   281,     0,   282,     0,     0,   338,   501,   165,   165,
       0,     0,     0,   255,     0,     0,   351,     0,     0,     0,
     165,   171,   174,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,   606,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   212,     0,     0,     0,
       0,     0,   200,     0,     0,     0,   303,     0,     0,     0,
       0,     0,     0,   338,     0,     0,     0,   343,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,   502,   503,     0,     0,   507,
       0,     0,   510,   511,     0,     0,   290,   455,     0,   291,
     459,   442,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,   312,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     501,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   606,   606,     0,
     200,     0,     0,     0,   606,     0,     0,     0,     0,     0,
       0,     0,     0,   255,     0,     0,   477,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   589,     0,
     360,     0,     0,     0,   361,     0,   362,     0,     0,     0,
       0,     0,   247,   621,     0,     0,   338,     0,     0,   459,
       0,   501,  1307,   363,   501,  1311,     0,   200,     0,     0,
       0,   528,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,     0,     0,   599,     0,   616,     0,
     364,   365,   171,   462,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,     0,   165,     0,     0,   574,
     387,    72,     0,     0,     0,     0,   577,   579,     0,     0,
       0,   586,     0,     0,     0,     0,     0,     0,     0,     0,
     675,   375,    74,     0,   463,   464,     0,   750,     0,   465,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,     0,   631,     0,     0,     0,     0,   312,     0,
       0,   312,     0,     0,   200,     0,     0,     0,   338,   247,
       0,     0,     0,     0,   606,  1409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,   599,     0,   338,     0,     0,     0,
     776,     0,     0,     0,   821,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     501,  1460,     0,     0,     0,     0,     0,     0,   212,   501,
    1469,     0,   606,     0,     0,     0,     0,     0,     0,     0,
     779,   780,   247,   338,   338,     0,     0,     0,     0,   200,
     200,     0,     0,     0,     0,   455,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,     0,   440,     0,     0,     0,     0,     0,   897,   898,
       0,     0,     0,     0,     0,   468,     0,   468,   343,     0,
       0,   905,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,     0,     0,     0,   455,     0,   885,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,   599,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,   312,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   675,     0,   675,   675,     0,   675,     0,
       0,     0,     0,   568,   675,     0,     0,   675,   675,   675,
       0,     0,     0,     0,     0,     0,   387,     0,   387,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   631,
       0,     0,     0,     0,     0,     0,     0,  1002,  1003,   247,
       0,     0,     0,  1007,     0,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   455,  1028,     0,     0,  1031,  1032,   387,
    1035,     0,  1037,  1038,     0,     0,     0,     0,     0,     0,
     501,     0,     0,     0,     0,     0,     0,   200,     0,     0,
       0,     0,     0,     0,     0,     0,   501,     0,     0,     0,
       0,     0,     0,     0,   455,     0,     0,     0,     0,     0,
       0,  1078,     0,     0,   387,  1082,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   455,   455,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   455,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   338,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1041,     0,     0,   468,     0,     0,     0,  1053,
       0,   468,     0,     0,     0,     0,   797,     0,     0,     0,
       0,  1199,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   455,     0,     0,   338,   338,     0,     0,   200,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   776,   501,
     501,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,     0,  1111,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   865,     0,     0,     0,     0,   343,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   440,     0,     0,     0,  1205,     0,     0,     0,   631,
       0,     0,     0,     0,   893,     0,     0,     0,     0,     0,
       0,     0,  1199,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     501,     0,     0,     0,     0,     0,     0,     0,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1298,     0,   927,     0,     0,     0,     0,
    1305,     0,     0,  1309,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   797,   947,     0,     0,
     949,     0,   951,     0,     0,     0,     0,     0,   960,     0,
     965,   960,   338,     0,     0,     0,   501,  1900,     0,     0,
     501,   455,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   993,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   995,     0,   501,     0,     0,     0,     0,     0,     0,
       0,   675,  1004,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   440,     0,     0,   993,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1407,   501,   501,  1057,     0,     0,   468,
       0,  1416,  1417,     0,     0,   255,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1354,  1356,  1358,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   501,     0,     0,
       0,     0,     0,   599,     0,  1088,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1377,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1458,
    1111,   343,     0,     0,     0,   675,     0,     0,  1467,     0,
       0,  1471,     0,  1474,  1475,     0,     0,     0,     0,     0,
       0,     0,     0,   411,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1189,  1191,     0,     0,   631,     0,     0,
       0,   440,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1501,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,   455,
     960,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   993,     0,     0,     0,     0,     0,     0,
       0,  1229,     0,  1197,     0,     0,     0,     0,   960,     0,
      13,    14,    15,    16,    17,     0,     0,   675,   675,   675,
       0,   675,   675,     0,     0,     0,     0,     0,   459,     0,
       0,  1589,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
     361,     0,   362,     0,   468,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,   363,
       0,     0,     0,     0,     0,     0,   255,     0,     0,     0,
       0,     0,  1531,     0,     0,  1533,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   343,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,  1471,   370,   371,   372,     0,   373,
     374,   468,     0,  1297,     0,  1300,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1649,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,     0,     0,     0,   377,   438,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1365,  1365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,   200,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
    1725,    45,    46,     0,     0,   255,     0,     0,     0,     0,
       0,     0,  1404,     0,     0,     0,     0,     0,  1414,     0,
       0,    57,     0,     0,     0,     0,     0,   468,     0,     0,
       0,     0,     0,     0,     0,  1683,   440,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   343,     0,     0,
       0,     0,   656,   468,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   960,     0,
       0,   797,     0,     0,   675,  1774,  1775,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1779,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   455,
     455,     0,     0,     0,     0,     0,     0,   -16,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   631,     0,     0,
       0,     0,     0,  1495,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,     0,     0,     0,   361,
       0,   362,     0,     0,     0,     0,     0,     0,     0,   255,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,   960,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,  1840,   797,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
     947,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1604,  1605,     0,  1818,     0,     0,   375,  1232,     0,    75,
     376,     0,   631,     0,  1233,  1898,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,   468,     0,   797,     0,
       0,     0,   675,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1991,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,  1349,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,   361,     0,   362,   675,     0,     0,   459,
       0,     0,     0,     0,     0,     0,   411,     0,     0,     0,
    1113,  1676,   363,    -2,     0,  1115,  -238,  -238,  1116,  1117,
    1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,
    -301,  1128,  1129,  1130,  1131,  1132,     0,  1133,     0,   364,
     365,     0,   462,     0,   367,  1134,  1135,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,  1136,   370,   371,
     372,  1991,   373,   374,     0,     0,     0,  1716,     0,     0,
      72,     0,     0,     0,     0,     0,     0,  1349,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -238,
     375,     0,     0,    75,   376,     0,     0,     0,   279,     0,
     377,    77,    78,   378,   379,   380,   381,  1740,   360,     0,
    1742,     0,   361,     0,   362,  -179,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1113,
       0,   363,    -2,     0,  1115,  -239,  -239,  1116,  1117,  1118,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  -301,
    1128,  1129,  1130,  1131,  1132,     0,  1133,     0,   364,   365,
       0,   462,     0,   367,  1134,  1135,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,  1136,   370,   371,   372,
    1723,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,  1349,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -239,   375,
       0,     0,    75,   376,     0,     0,     0,   279,     0,   377,
      77,    78,   378,   379,   380,   381,     0,   360,     0,     0,
       0,   361,     0,   362,  -179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1113,     0,
     363,    -2,     0,  1115,     0,     0,  1116,  1117,  1118,  1119,
    1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  -301,  1128,
    1129,  1130,  1131,  1132,     0,  1133,     0,   364,   365,     0,
     462,     0,   367,  1134,  1135,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,  1136,   370,   371,   372,     0,
     373,   374,     0,     0,     0,   960,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,   279,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,     0,
       0,     0,     0,  -179,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,  1112,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   360,
       0,    45,    46,   361,     0,   362,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
    1113,    57,  1114,    -2,     0,  1115,     0,     0,  1116,  1117,
    1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,
    -301,  1128,  1129,  1130,  1131,  1132,     0,  1133,     0,   364,
     365,    60,   462,     0,   367,  1134,  1135,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,  1136,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -3,
     375,     0,     0,    75,   407,     0,     0,     0,   279,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1112,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   360,     0,    45,    46,   361,     0,   362,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,  1113,    57,  1114,    -2,     0,  1115,     0,     0,
    1116,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,
    1126,  1127,  -301,  1128,  1129,  1130,  1131,  1132,     0,  1133,
       0,   364,   365,    60,   462,     0,   367,  1134,  1135,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1136,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   407,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,  -179,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   360,     0,    45,    46,   361,     0,   362,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,   365,    60,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,  1552,    75,   407,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,     0,     0,     0,  1553,  1554,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   360,     0,    45,    46,   361,
       0,   362,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,    60,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     407,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,     0,     0,     0,
    1553,  1554,     4,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   360,     0,    45,
      46,   361,     0,   362,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,     0,    57,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,   365,    60,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
    1543,    75,   407,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     360,     0,    45,    46,   361,     0,   362,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,   365,    60,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   407,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   360,     0,    45,    46,   361,     0,   362,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   437,     0,     0,
       0,     0,     0,   377,   438,    78,   378,   379,   380,   381,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   360,     0,    45,    46,   361,     0,
     362,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    75,  1186,
       0,     0,     0,     0,     0,   377,  1187,    78,   378,   379,
     380,   381,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,   319,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   360,     0,
      45,    46,   361,     0,   362,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   437,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,  1849,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,     0,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,  1869,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,  1411,     0,     0,
       0,    -2,    -2,     0,    13,    14,    15,    16,    17,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,   361,     0,   362,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,     0,
       0,     0,    57,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,    -2,    -2,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,     0,
       0,   377,  1412,    78,   378,   379,   380,   381,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    59,     0,     0,     0,    60,    61,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,     0,    75,    76,     0,
       0,     0,     0,     0,     0,    77,    78,   242,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -422,  -422,     0,  -422,    45,    46,     0,  -422,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,    61,    45,    46,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,   244,     0,     0,
       0,  -732,     0,     0,    77,    78,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,     0,    57,     0,     0,     0,     0,  -354,  -354,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -354,     0,     0,     0,    75,    76,     0,     0,     0,
       0,     0,     0,    77,    78,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,     0,     0,     0,     0,  -355,  -355,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -355,     0,     0,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -422,  -422,     0,
    -422,    45,    46,     0,  -422,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,   244,     0,     0,  1325,     0,     0,
       0,    77,    78,  1326,     0,     0,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,  1327,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1328,     0,     0,     0,    75,   923,     0,     0,
    1325,     0,     0,     0,    77,    78,  1326,     0,     0,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,  1327,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1507,     0,     0,     0,    75,
     923,     0,     0,  1325,     0,     0,     0,    77,    78,  1326,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,  1327,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1508,     0,
       0,     0,    75,   923,     0,     0,  1325,     0,     0,     0,
      77,    78,  1326,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,  1327,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1509,     0,     0,     0,    75,   923,     0,     0,     0,
       0,     0,     0,    77,    78,   242,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -422,  -422,
       0,  -422,    45,    46,     0,  -422,     0,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,    57,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   242,     0,
       0,     0,     0,   656,    75,   244,     0,    13,    14,    15,
      16,    17,    77,    78,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -422,  -422,     0,  -422,    45,    46,     0,  -422,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,    57,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -421,  -421,     0,  -421,    45,    46,     0,  -421,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   301,     0,
       0,     0,     0,     0,     0,    77,    78,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -422,
    -422,     0,  -422,    45,    46,     0,  -422,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,   244,     0,     0,     0,
    -736,     0,     0,    77,    78,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -422,  -422,     0,
    -422,    45,    46,     0,  -422,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,  1349,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,   360,    75,   244,     0,   361,     0,   362,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1113,     0,   363,     0,     0,  1115,  1782,
    1783,  1116,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  -301,  1128,  1129,  1130,  1131,  1132,     0,
    1133,     0,   364,   365,     0,   462,     0,   367,  1134,  1135,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
    1136,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,  1349,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,   279,     0,   377,    77,    78,   378,   379,   380,   381,
     360,     0,     0,     0,   361,     0,   362,     0,  -179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1113,     0,   363,     0,     0,  1115,     0,     0,  1116,
    1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  -301,  1128,  1129,  1130,  1131,  1132,     0,  1133,     0,
     364,   365,     0,   462,     0,   367,  1134,  1135,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,  1136,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,   279,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,     0,     0,     0,     0,  -179,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,    62,    63,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,    72,     0,  1048,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -604,    75,   320,     0,     0,    62,
      63,     0,     0,    77,    78,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    75,     0,     0,
       0,    45,    46,     0,     0,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,  1758,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   320,     0,     0,    62,    63,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    75,     0,     0,     0,    45,
      46,     0,     0,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
    1760,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   320,     0,     0,     0,     0,     0,     0,    77,
      78,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     320,     0,     0,     0,     0,     0,     0,    77,    78,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   301,     0,
       0,     0,     0,     0,     0,    77,    78,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -422,
    -422,     0,  -422,    45,    46,     0,  -422,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -422,
    -422,     0,  -422,    45,    46,     0,  -422,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   244,     0,     0,     0,
       0,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,   662,    19,   663,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   360,     0,    45,    46,   361,     0,   362,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   664,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   665,     0,     0,     0,
     279,     0,   377,    77,    78,   666,   667,   380,   381,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   360,     0,    45,    46,   361,
       0,   362,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,   406,    75,
     407,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   360,
       0,    45,    46,   361,     0,   362,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,    75,   665,     0,     0,     0,   279,     0,
     377,    77,    78,   378,   379,   380,   381,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   360,     0,    45,    46,   361,     0,   362,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   407,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   360,     0,    45,
      46,   361,     0,   362,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   437,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   360,     0,    45,    46,   361,     0,   362,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
      76,     0,     0,     0,  -734,     0,     0,    77,    78,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
      76,     0,    13,    14,    15,    16,    17,    77,    78,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -422,  -422,     0,  -422,
      45,    46,     0,  -422,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,   301,     0,     0,     0,     0,     0,     0,
      77,    78,   556,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,    62,    63,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,    75,     0,    45,    46,    62,    63,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,  1427,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   929,    75,   923,     0,     0,    62,    63,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   923,     0,     0,     0,
       0,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,    62,    63,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   286,     0,     0,    62,
      63,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,    76,     0,
       0,     0,     0,     0,     0,    77,    78,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   433,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     320,     0,     0,     0,     0,     0,     0,    77,    78,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     286,     0,     0,    62,    63,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   433,     0,     0,     0,     0,     0,     0,    77,
      78,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -422,  -422,     0,  -422,    45,    46,
       0,  -422,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
      75,     0,    45,    46,    62,    63,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   301,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   923,     0,     0,     0,     0,
       0,     0,    77,    78,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   923,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,    13,
      14,    15,    16,    17,    77,    78,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -422,  -422,     0,  -422,    45,    46,     0,
    -422,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,    19,    57,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -422,  -422,     0,  -422,    45,    46,     0,
    -422,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     301,    62,    63,     0,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,    77,    78,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   850,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -617,    75,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1684,     0,     0,     0,     0,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,    75,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      62,    63,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -422,
    -422,     0,  -422,    45,    46,     0,  -422,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    62,    63,     0,
       0,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,    75,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -422,  -422,    75,  -422,    45,    46,     0,  -422,     0,
       0,   360,     0,     0,     0,   361,     0,   362,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,    75,     0,     0,
       0,     0,   375,   956,  1534,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,   465,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,   796,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,   279,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     375,   956,     0,    75,   376,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,   987,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,  1207,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,  1299,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,  1359,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     375,     0,  1788,    75,   376,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   375,  1974,     0,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   648,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   653,     0,
       0,    75,   376,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     659,     0,     0,    75,   376,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
       0,     0,   377,   864,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,     0,     0,   377,   438,    78,   378,   379,   380,
     381
};

static const yytype_int16 yycheck[] =
{
       1,   220,   256,     4,     1,   873,     1,   162,   241,  1722,
     283,   633,   691,   859,   162,    73,     4,   219,    73,   173,
     178,   375,   614,   219,    73,   676,   150,     1,   219,   465,
      73,   514,   515,    95,   601,    58,   764,   962,  1122,    75,
     222,   207,   735,  1659,   131,    73,  1351,   601,   605,   601,
     163,   219,   669,     0,    55,    56,   764,    58,     1,    73,
     219,    58,  1659,    58,  1659,    82,   991,  1205,     1,   115,
     366,     4,    73,   782,   323,     1,     1,   296,     1,   765,
    1660,    82,     0,   219,    58,   771,   843,    82,    73,    90,
      73,   800,   525,   295,    95,   139,   375,    98,   219,   295,
      95,   102,   535,    98,   295,   102,   229,   102,   290,   291,
      70,   338,   236,   870,   102,   342,  1283,  1284,    59,    60,
    1782,   859,   145,    73,   211,    58,   181,   295,   157,  1054,
     147,   254,   181,    58,   762,    58,   295,   352,   181,   140,
     355,   264,   143,   576,   145,   174,     0,   191,   145,   150,
     145,   762,   635,   181,   762,   156,    87,   866,   220,   295,
       1,   219,   163,     4,   219,    98,   462,   181,   762,   102,
     219,   145,    70,   178,   295,     1,   219,    87,     4,   180,
     181,    70,   321,   162,   177,   180,  1106,   131,    87,   174,
    1770,   219,  1241,   153,   195,   762,   445,   284,   181,   338,
     195,    75,    76,   342,   205,   219,  1096,   764,   762,   210,
     762,  1101,   145,   156,   149,     1,   157,    58,   219,   220,
     145,   165,   145,   607,   174,   220,   157,  1106,   245,   155,
     156,    82,    58,   174,   296,   236,   219,   295,   274,    19,
     295,   920,  1904,   497,   245,   155,   295,   157,   130,     1,
     245,   149,    95,  1966,   255,   153,    82,   258,   157,   742,
     149,   102,   157,   258,   265,   781,   782,   295,  1114,   948,
     149,   573,    58,   492,   275,   276,  1581,   278,    87,   174,
    1418,   483,  1172,   157,   800,   302,   283,   483,   375,   562,
     172,   913,   483,  1873,   295,   296,  1024,  1143,   149,   149,
     439,   296,   303,    96,   145,   471,    58,  1000,  1924,   310,
     311,    98,    98,   139,   315,   483,  1024,  1351,  1352,   145,
     937,   147,   109,    82,   483,   258,  1106,  1924,   149,  1924,
     443,   174,  1018,    75,    76,   689,    95,   894,   120,    98,
     192,  1098,   321,   102,   593,  1925,   347,   483,   157,   115,
     866,   352,   571,  1786,   355,   356,   149,  1066,   283,   145,
     283,   149,   483,   590,   151,   191,   149,   569,   617,   648,
     152,   650,   651,   569,   653,   624,  1114,  1957,   569,   149,
     659,   151,   766,   662,   663,   664,   770,   153,   512,  1017,
     173,   435,   409,   145,   518,   779,   780,  1446,  1447,  1448,
     627,   569,  1322,  1323,  1324,  1143,  1017,   840,   195,  1017,
     569,   534,  1907,   131,   152,   157,   242,   504,   419,   245,
     316,   180,   509,  1017,   419,   573,  1921,   463,   483,  1319,
     492,    73,   153,   569,   483,   173,   195,   158,   149,   526,
     483,   442,   443,  1322,  1323,  1324,    88,   165,   569,   536,
    1017,   590,  1947,   454,   455,   483,    82,   283,   354,   154,
     439,   220,   463,  1017,   465,  1017,   131,  1024,   155,   483,
    1554,   258,   258,   265,  1558,   614,   302,  1644,  1645,   781,
     782,   174,   483,   155,   276,   103,   245,   174,   627,   107,
      70,   492,   110,   165,   112,   160,   161,   492,   800,   258,
     483,   288,    12,    13,    14,    15,    16,   294,  1194,   571,
      70,   512,  1945,   153,   569,   174,  1441,   518,   158,   146,
     569,   147,     4,     5,     6,     7,     8,     9,    10,    11,
     157,   283,    70,   104,   105,   151,   323,   323,   155,  1972,
     156,   569,  1322,  1323,  1324,    56,   173,  1581,    59,    60,
    1066,    62,   575,   179,     4,   556,     3,   558,   994,    73,
      70,   648,    70,   560,   866,   562,   653,   155,   569,   149,
     571,     3,   659,   153,   575,    89,   571,   165,   575,  1201,
     575,   152,    70,   409,   585,   155,  1272,   228,   589,   149,
     231,   678,   106,   153,   573,   149,  1188,   155,   155,   149,
    1251,   575,   851,   157,   174,    55,    56,   157,   155,   435,
     157,   149,   253,  1351,  1352,   153,   174,   775,  1702,   245,
     621,   131,   263,   789,   152,    12,    13,    14,    15,    16,
     248,   933,   633,   149,   883,   614,    70,   562,   149,   562,
      90,   149,   575,    70,   151,   153,    70,   173,  1516,   156,
     575,   764,   575,   155,   881,   125,   126,  1041,   445,   445,
     419,   151,  1341,   153,    70,  1832,   591,    70,   520,   151,
     155,  1705,   174,   157,   151,   155,   302,   155,   802,   156,
     681,   717,   683,    70,   685,   157,    70,   149,   689,   174,
     140,   692,   174,   143,   174,   321,   174,   821,  1181,   169,
     170,   155,  1627,   171,  1629,   157,   156,   325,   326,  1793,
     328,  1362,   330,   163,   149,   149,   717,  1801,   155,   153,
     174,   156,   149,   716,   150,   149,   153,   149,    70,   153,
     556,   157,   519,   492,   575,    70,   562,   174,    70,   591,
     827,   151,   881,   149,   131,   155,   149,   153,   155,   575,
     153,   838,   539,   911,   841,   148,    70,   149,   845,    70,
     210,   762,   155,   764,  1066,   149,   115,   174,   151,   153,
     775,  1207,   559,   559,     3,   776,   104,   105,   363,   151,
    1864,   786,   783,   409,   205,   933,   160,  1438,   789,   575,
     151,   792,   905,   167,   168,   156,   151,  1233,   151,  1053,
     801,   802,   803,   388,   389,   255,   593,   149,   151,   152,
     562,   153,   148,   439,   149,   265,  1850,   149,   153,   155,
     821,   153,   151,   575,   409,   275,   276,   151,   278,   822,
     617,   617,   104,   105,   759,   149,  1870,   624,   149,   153,
     149,   101,   153,  1581,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   303,   439,   151,   857,   858,   859,   155,
     310,   311,   859,   149,   505,   315,   718,   129,   155,   151,
     657,   658,   101,   155,  1908,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   872,   859,   527,   149,   514,   515,
     150,   153,   533,   153,    21,  1061,   537,   347,   160,   161,
     129,   149,   352,  1085,   905,   355,   911,   759,   909,   149,
    1023,  1024,   913,   531,  1216,   152,   153,   155,   919,   156,
     149,   150,   155,  1359,   149,   129,   859,   156,   153,   781,
     782,   160,   161,   155,   859,   151,   859,    46,    47,   872,
      49,  1220,   155,  1192,    53,   149,   162,   163,   800,   153,
      96,   151,   149,   954,   933,   155,   160,   161,   149,   151,
     151,   962,   153,   155,  1197,     3,   149,  1705,   151,   173,
     153,   149,  1326,   157,    12,    13,    14,    15,    16,   129,
      56,  1283,   987,    59,    60,     9,    62,   151,   614,  1611,
     991,   155,   442,   994,    12,    13,    14,    15,    16,   149,
    1436,   157,   151,   153,   454,   455,   155,   148,   151,   635,
     160,   161,   155,  1100,   866,   149,  1017,   151,   859,   153,
     149,    87,  1023,  1024,   153,     3,  1113,   157,  1182,  1183,
     129,   872,    70,   859,    12,    13,    14,    15,    16,   162,
     163,   151,   463,  1130,   465,   155,   872,   127,   128,  1188,
     149,  1175,    70,  1054,   153,   149,   154,   151,  1063,   153,
    1314,   160,   161,   149,   851,   851,   149,   153,  1216,   151,
     153,   123,   124,   859,   151,   862,   862,   101,  1351,   157,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1106,
     157,   151,    70,   157,   101,   155,   883,   883,  1400,   106,
     107,   108,   109,   110,   111,   112,   556,   859,   154,   155,
     695,   173,  1850,  1332,  1736,  1116,   742,  1114,  1119,  1120,
    1121,     4,     5,     6,     7,     8,     9,    10,    11,  1284,
     160,   161,  1870,  1291,  1292,   585,  1815,   151,   115,   589,
    1114,   155,  1143,   149,   101,   149,  1143,   153,  1149,   106,
     107,   108,   109,   110,   111,   112,  1157,   151,   149,  1160,
    1161,   155,     9,  1164,  1161,  1160,  1161,   154,   155,  1143,
    1908,   621,   129,  1161,  1175,   151,   143,   144,   145,   155,
      63,  1114,   151,   633,   154,   155,   155,   149,   155,  1114,
     151,  1114,   149,   150,   155,   151,   153,   155,   165,   155,
    1201,   842,   166,   160,   161,   159,  1207,   174,   151,  1188,
    1143,   151,   155,   151,  1066,   155,  1217,   155,  1143,   161,
    1143,   151,   143,   144,   145,   155,   151,  1160,  1161,   129,
     155,   681,  1233,   683,   155,   685,   171,  1216,   152,   689,
    1241,   151,   692,   151,   165,   155,   151,   873,   151,  1254,
     155,   153,  1400,   174,   101,  1342,  1343,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   151,   717,   154,   155,
    1332,   151,  1273,  1114,    12,    13,    14,    15,    16,    17,
    1106,   108,   109,   110,   111,   112,  1291,  1292,  1114,   151,
     151,   154,   155,  1380,   155,  1514,    12,    13,    14,    15,
      16,    17,  1143,  1522,   154,  1322,  1323,  1324,  1581,  1326,
    1327,    12,    13,    14,    15,    16,    17,  1143,  1520,   151,
    1161,   154,   155,   155,  1520,   151,   776,   131,  1114,  1520,
    1929,  1332,   131,   783,  1933,  1336,   630,  1325,  1339,   156,
    1619,   156,  1644,   155,   929,   703,   704,   705,   706,   934,
     151,   801,  1520,   803,  1351,  1352,   149,  1143,  1359,   151,
     945,  1520,  1114,   696,   697,   698,   155,   156,   789,   154,
     155,   792,    17,  1160,  1160,  1507,  1508,  1509,  1379,   151,
    1381,  1103,  1104,  1105,  1520,   151,  1381,   154,   155,   154,
     155,  1143,  1325,    12,    13,    14,    15,    16,    17,  1520,
     149,  1160,  1161,   154,   155,  1192,  1192,   857,   858,   859,
      55,    56,    57,    58,    59,    60,    61,    62,  1351,  1352,
     151,  1400,   154,   155,   154,   155,  1351,  1352,  1351,  1352,
     154,  1283,  1284,   154,   155,  1436,  1424,  1592,   155,   156,
    1441,   154,   155,   151,  1592,  1446,  1447,  1448,  1381,   154,
     155,   153,  1514,    75,    76,   905,   154,   155,  1677,   909,
    1522,   157,  1520,   913,     4,     5,     6,     7,     8,     9,
      10,    11,  1674,   154,   155,   154,   155,   157,  1674,   154,
     155,   157,    12,  1674,  1325,  1572,   154,   155,   154,   155,
    1645,  1424,   157,   787,   155,   156,  1322,  1323,  1324,  1325,
    1326,  1327,  1242,  1243,   699,   700,  1674,   701,   702,    68,
    1351,  1352,   154,  1514,   149,  1674,    76,   707,   708,  1520,
     154,  1522,    17,  1527,  1528,  1351,  1352,   173,  1529,  1114,
    1182,  1183,   155,   954,   173,   149,   157,   174,  1674,   151,
       1,   962,  1543,     4,   151,   174,  1333,   157,   157,   154,
     154,  1552,    17,  1674,  1551,  1181,    86,   148,   151,   151,
    1779,   151,  1188,  1551,   151,  1351,  1352,   151,   151,   151,
     991,   101,   151,   994,   104,   105,   106,   107,   108,   109,
     110,   111,   112,  1424,  1581,   148,   157,  1580,  1589,   157,
     157,    68,   173,   151,  1381,  1381,   174,    58,  1424,  1351,
    1352,   151,   151,  1188,  1728,   151,   148,  1595,   151,   157,
    1611,   155,    73,  1592,   151,  1677,   151,  1550,  1551,   151,
     914,    82,  1381,   154,  1782,  1874,  1627,   155,  1629,   151,
     151,   151,   151,  1054,    95,   151,   151,    98,  1223,  1224,
    1225,   102,   154,   151,   151,  1230,  1231,   151,  1581,   151,
     151,   151,   151,   151,   148,   151,  1581,   154,  1581,   151,
     173,   151,  1595,   148,   151,   149,  1116,   155,  1807,  1119,
    1120,  1121,    13,  1674,   149,   149,  1677,  1832,   139,   149,
    1532,   149,   149,   156,   145,    72,   147,  1688,   174,   150,
     151,  1692,   155,  1143,    89,   174,   156,   154,   154,  1149,
     148,   162,   174,  1704,   148,   174,  1925,  1157,  1705,  1710,
    1551,   157,   155,   151,  1164,   174,   151,  1779,   179,   180,
     181,   154,  1924,   155,  1717,   155,  1727,  1728,  1924,   151,
     191,   192,  1871,  1924,   195,  1736,   155,   154,  1957,   151,
    1581,   151,   148,   148,  1902,   149,  1904,   174,   174,  1873,
     149,  1201,    78,   174,  1595,  1581,  1924,   149,   219,   220,
     174,  1694,   148,  1550,  1550,  1924,   174,  1217,   149,  1595,
     148,   174,  1705,  1931,   174,   236,   174,  1782,  1779,   151,
    1705,   174,  1705,   148,   245,  1786,  1207,   155,  1924,  1790,
    1807,  1792,  1644,  1645,   155,  1581,   154,   258,   154,   154,
     151,   154,   148,  1924,   151,   157,   156,   156,   118,   148,
     154,  1812,  1233,   151,   151,    76,   151,   151,  1811,   151,
    1241,   154,   151,   148,   107,   156,   174,   288,   155,  1581,
     149,   151,   155,   294,   295,   296,   149,   149,   154,   154,
     101,   302,  2000,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1853,  1850,  1871,   154,  1857,   148,   148,   157,
     321,   322,   323,  1925,  1705,  1866,  1924,   148,   154,  1924,
      73,   151,  1873,  1870,  1875,  1924,   151,   338,   151,  1705,
     151,   342,   151,   151,    73,  1886,  1336,  1888,  1889,  1339,
    1516,   148,   174,  1980,   149,  1957,  1924,  1902,   174,  1904,
     174,   151,   151,    88,   148,   151,   154,  1694,  1694,  1996,
    1911,  1908,   154,   174,   375,  1209,   148,  1850,   151,  1705,
     151,   151,   151,  1924,  1925,  1850,  1931,  1850,   151,  1379,
    1925,   153,    73,   152,  1935,   174,  1929,  1870,  1359,    73,
    1933,  1934,   156,   151,  1945,  1870,   174,  1870,   409,   174,
    1955,   412,   148,  1705,   151,   155,  1957,   192,   419,   151,
     151,   148,  1957,   156,  1258,   101,  1959,   150,   149,  1970,
     150,  1972,   148,    73,   435,  1908,   155,   149,   439,   148,
    1832,   165,   443,  1908,   445,  1908,   174,  1988,  1981,   154,
     165,   828,  1985,  1994,   107,  2000,   174,   107,   151,   156,
     151,  1586,   148,  2004,   148,    73,   151,  2008,   174,  1850,
     149,   151,   174,   151,  2007,  1436,   174,  2018,   377,  1619,
    1441,  1341,   483,   709,  1850,  1446,  1447,  1448,  1248,  1870,
     668,   492,    12,    13,    14,    15,    16,   710,   712,   711,
    1132,   408,   713,  1143,  1870,  1972,  1581,  1904,   230,   886,
    1921,   512,  1709,   514,   515,  1705,  1967,   518,  1816,   520,
    1796,  1966,  1954,  1871,  1850,   101,  1934,  1908,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1573,  1573,  1529,
    1870,  1985,  1908,  1164,  1870,    48,   321,  1874,  1779,   324,
      70,   250,  1840,  1543,  1514,  1327,  1157,   558,  1850,  1528,
     789,   472,  1552,   338,   585,   876,  1424,   342,   569,  1595,
     571,     0,   573,   919,   575,    -1,   734,   734,  1870,   734,
      -1,  1415,  1908,    -1,    -1,    -1,  1420,    -1,    -1,   590,
     591,    -1,   593,    -1,    -1,    -1,    -1,    -1,   174,  1589,
     601,    -1,    -1,  1437,   605,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,   614,    -1,    -1,  1908,    -1,    -1,   996,
      -1,  1611,    -1,   624,    -1,    -1,   627,    -1,    -1,   149,
      -1,    -1,    -1,   153,   635,    -1,  1013,  1014,    -1,    -1,
     160,   161,    12,    13,    14,    15,    16,   648,    -1,   650,
     651,    -1,   653,    -1,    -1,    -1,    -1,    -1,   659,    -1,
      -1,   662,   663,   664,   439,    -1,  1627,    -1,  1629,    12,
      13,    14,    15,    16,    -1,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    73,  1814,
     412,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1688,    -1,
      70,    -1,  1692,    -1,   129,    -1,   428,    -1,    -1,   431,
      95,    -1,    -1,    70,  1704,    -1,    -1,   718,    -1,    -1,
    1710,    -1,    -1,    -1,   149,   150,    -1,    70,    -1,  1563,
      -1,   156,    -1,   734,   735,   160,   161,  1727,    -1,    -1,
      -1,   742,    -1,    -1,   101,   520,  1736,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   759,   129,
      -1,   762,    -1,   764,    -1,   150,   488,    -1,   551,    -1,
      -1,    -1,   129,    -1,   118,    -1,   120,   121,   122,   149,
     781,   782,    -1,   153,    -1,    -1,   129,  1621,    -1,    -1,
     160,   161,   149,   150,    -1,    -1,  1786,    -1,   573,   800,
    1790,   802,  1792,   160,   161,   149,   149,    -1,   152,   153,
     153,    -1,    -1,   157,   158,   590,   591,   160,   161,    -1,
     821,   101,  1812,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   219,   220,    -1,    -1,   101,   614,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   236,   627,    -1,    -1,    -1,    -1,    -1,   859,    -1,
      -1,    -1,    -1,  1853,    -1,   866,    -1,  1857,    -1,    -1,
      17,   872,   873,    -1,    -1,    -1,  1866,    -1,    -1,    -1,
     881,    -1,   883,    -1,    -1,  1875,    -1,  1264,  1265,    70,
      -1,    -1,    -1,   894,   174,    -1,  1886,   160,  1888,  1889,
      -1,  1278,  1279,  1737,    -1,    -1,    -1,    -1,    -1,    -1,
     295,   296,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
     101,  1911,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   933,    -1,    -1,  1312,  1313,    -1,    -1,    -1,
      -1,   101,    -1,   718,    -1,  1935,   106,   107,   108,   109,
     110,   111,   112,     1,   101,  1945,     4,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   101,   149,   150,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
    1970,    -1,  1972,    -1,   759,    -1,    -1,    -1,    -1,  1823,
     150,    -1,    -1,   153,    63,    64,    65,    66,  1988,  1000,
      -1,    -1,    -1,    -1,  1994,    -1,   153,    -1,    -1,    -1,
      58,    -1,   734,   735,  2004,    -1,  1017,    -1,  2008,   153,
      -1,    -1,   744,  1024,    -1,   747,    -1,    -1,  2018,    -1,
      -1,    -1,   101,    -1,    82,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   828,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   443,    -1,
      -1,    -1,    -1,    -1,    -1,  1066,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,    -1,   808,    -1,    -1,    -1,
     812,   139,    -1,    -1,   816,    -1,    -1,   145,   483,   147,
      -1,    -1,   171,   886,    -1,  1106,   881,   492,    -1,   884,
      -1,    -1,    -1,  1114,    -1,    -1,    -1,    -1,    -1,  1496,
    1497,    -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,   101,
      70,   179,    -1,   518,   106,   107,   108,   109,   110,   111,
     112,   113,  1143,   191,    -1,   117,    -1,   119,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,   933,  1160,
    1161,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   558,  1175,    -1,    -1,    -1,   150,    -1,
    1181,   153,    -1,    -1,   569,    -1,   571,  1188,    -1,   129,
      -1,    -1,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,
      -1,    -1,   250,    -1,    -1,    -1,    -1,    -1,    58,   149,
     150,    -1,    -1,   996,    -1,  1216,    -1,    -1,    -1,  1220,
     160,   161,    -1,    -1,    -1,    -1,  1603,    -1,    70,    -1,
    1013,  1014,    82,    -1,   101,   283,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    98,    -1,
      -1,    -1,   102,    -1,   302,    -1,    -1,  1634,    -1,   101,
      -1,    -1,  1639,  1640,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1000,    -1,
      -1,    -1,  1283,  1284,    -1,   152,    -1,   129,    -1,   139,
     157,    -1,    -1,    -1,    -1,   145,    -1,   147,    -1,    -1,
      -1,   151,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,   161,   162,   163,    -1,    -1,    -1,    -1,   160,   161,
      -1,  1322,  1323,  1324,  1325,  1326,  1327,   375,    -1,   179,
      -1,  1332,  1333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   191,   192,    -1,    -1,   195,    -1,    -1,    -1,  1071,
    1351,  1352,  1074,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   409,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,    -1,   764,
    1381,    -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,
      -1,    -1,   242,    -1,    -1,   245,    -1,    -1,    -1,  1400,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   258,    -1,
      -1,    -1,    -1,  1188,    -1,    -1,    -1,   802,    -1,    -1,
      70,   101,    -1,  1424,    -1,   275,   106,   107,   108,   109,
     110,   111,   112,   283,    -1,    -1,   821,    -1,   288,    -1,
      -1,  1216,    -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   302,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,   514,   515,    -1,   149,
     150,   321,    -1,   323,   324,    -1,    -1,    -1,    -1,   129,
      -1,  1264,  1265,    -1,    -1,    -1,    -1,    -1,   338,    -1,
      -1,    -1,   342,    -1,    -1,  1278,  1279,    -1,    -1,   149,
     150,    -1,    -1,   153,    -1,    -1,    -1,    -1,   556,  1510,
     160,   161,   560,  1514,   562,  1516,    -1,    -1,    -1,  1520,
      -1,  1522,    -1,    -1,    -1,   375,    -1,   575,    -1,  1312,
    1313,    -1,    -1,    70,   101,    -1,    -1,    -1,  1260,   106,
     107,   108,   109,   110,   111,   112,   113,  1269,    -1,  1550,
    1551,    12,    13,    14,    15,    16,    -1,    -1,    -1,   409,
      -1,    -1,    -1,    -1,   101,  1942,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
    1581,    -1,    -1,    -1,    -1,   435,   153,   635,    -1,   439,
      -1,  1592,   129,    -1,  1595,   445,    -1,    -1,    -1,    -1,
     648,    -1,   650,   651,    -1,   653,    -1,    -1,    -1,    70,
      -1,   659,   149,   150,   662,   663,   664,    -1,  1619,    -1,
      -1,    -1,   375,   160,   161,  1400,    -1,    -1,    -1,    -1,
      -1,    -1,  1017,    -1,    -1,    -1,    -1,    -1,  1023,  1024,
     101,    -1,    -1,  1644,  1645,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,  1659,  1660,
      -1,    -1,    -1,    -1,   514,   515,    -1,    -1,   129,   519,
     520,    -1,    -1,  1674,   101,    -1,  1677,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   149,   150,
      -1,    -1,    -1,  1694,   742,    -1,    -1,    -1,    -1,   160,
     161,   551,    -1,    -1,  1705,    -1,   556,    -1,    -1,   559,
     560,    -1,   562,  1496,  1497,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,   573,    -1,   575,   101,  1728,    -1,    -1,
     157,   106,   107,   108,   109,   110,   111,   112,   113,   589,
     590,   591,   117,   593,   119,    -1,    -1,    -1,   101,  1532,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   514,   515,    -1,   614,    -1,    -1,   617,    -1,  1770,
      -1,   621,    -1,    -1,   624,   150,   129,   627,  1779,   629,
      -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,  1510,    -1,
    1175,    -1,    -1,    -1,    -1,    -1,   149,   150,   648,    -1,
     650,   651,    -1,   653,    -1,    -1,  1807,   160,   161,   659,
      -1,   859,   662,   663,   664,    -1,    -1,    -1,    -1,    -1,
    1603,    -1,    -1,    -1,   872,   873,    -1,    -1,    -1,    -1,
      -1,  1832,    -1,    -1,    -1,    -1,    -1,   101,    -1,  1840,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1850,
      -1,  1634,    -1,    -1,    -1,    -1,  1639,  1640,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,   718,  1870,
    1871,   101,  1873,  1874,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   149,   150,    -1,  1273,   153,
      -1,    -1,   742,    -1,    -1,   648,   160,   161,    -1,    -1,
     653,    -1,    -1,    -1,    -1,    -1,   659,  1908,    -1,   759,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,  1924,  1925,   678,    -1,   157,    70,    -1,
      -1,   781,   782,    -1,    -1,    -1,    -1,  1659,  1660,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1332,    -1,    -1,
     800,    -1,    -1,    -1,    -1,    -1,  1957,    -1,   149,   101,
      -1,   714,   104,   105,   106,   107,   108,   109,   110,   111,
     112,     1,    -1,    -1,     4,    -1,    -1,    -1,   828,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   129,    -1,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      -1,   851,   117,    -1,   119,    -1,    -1,   149,   150,   859,
      -1,   153,   862,    -1,    -1,    -1,   866,    -1,   160,   161,
      -1,    -1,   872,   873,    -1,    -1,    -1,    -1,    58,    -1,
      -1,   881,  1807,   883,   884,   150,   886,    -1,   153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1770,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,  1106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1114,    -1,    -1,    -1,
     101,    -1,   102,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   933,    12,    13,    14,    15,    16,    -1,
       1,    -1,   101,    -1,    -1,  1143,  1871,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,    -1,   117,   139,
     119,    -1,    -1,  1161,  1836,   145,    -1,    -1,  1840,    -1,
      -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1514,
      -1,    -1,   162,  1181,    -1,  1520,    -1,  1522,    -1,    -1,
      -1,   150,    70,    -1,   153,    -1,   996,    58,    -1,    -1,
     180,  1873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1942,
      -1,   191,   192,  1013,  1014,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1220,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,   101,
     220,   102,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   129,  1924,  1925,    -1,    -1,   236,    -1,    -1,    -1,
      -1,   241,   242,    -1,    -1,   245,  1066,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,   145,  1957,    -1,   267,    -1,    -1,
     270,    -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,    -1,   283,    -1,    -1,  1106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1114,    -1,   296,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1322,  1323,  1324,  1325,  1326,  1327,
      -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1674,
      -1,   321,  1677,  1143,   324,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1351,  1352,    -1,    -1,    -1,   338,    -1,
    1160,  1161,   342,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1181,    -1,    -1,    -1,    -1,   366,    -1,  1188,    -1,
      -1,    -1,  1192,  1728,    -1,    -1,    -1,   129,    12,    13,
      14,    15,    16,    -1,  1204,    -1,   267,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,  1216,   149,   150,    -1,
    1220,   153,   283,    -1,    -1,    -1,  1424,   101,   160,   161,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   173,    -1,    -1,  1779,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   435,    70,    -1,    -1,   439,
     321,    -1,    -1,   324,  1264,  1265,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,   149,    -1,   338,  1278,  1279,
      -1,   342,   462,  1283,  1284,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1312,  1313,    -1,   129,    -1,    -1,  1516,    -1,
      -1,    -1,  1322,  1323,  1324,  1325,  1326,  1327,   129,    -1,
      -1,    -1,    -1,  1333,    -1,   149,   150,    -1,  1873,    -1,
     520,    -1,    -1,    -1,    -1,    -1,   160,   161,   149,   150,
      -1,  1351,  1352,  1551,    -1,    -1,    -1,    -1,   101,   160,
     161,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   551,    -1,    -1,    -1,    -1,   556,    -1,   439,    -1,
     560,  1381,   562,  1581,    -1,    -1,   129,    -1,    -1,  1924,
    1925,    -1,    -1,   573,    -1,   575,    -1,  1595,    -1,    -1,
    1400,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
     590,   591,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,  1619,  1957,    -1,  1424,   605,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   614,    -1,    -1,    -1,    -1,   619,
      -1,    -1,    -1,    -1,    -1,  1348,    -1,   627,  1351,  1352,
      -1,    -1,    -1,    -1,  1357,    -1,    -1,    -1,  1361,   520,
    1363,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   129,
     551,    -1,    -1,    -1,    -1,    -1,  1496,  1497,    -1,   560,
      -1,   562,    -1,    -1,    -1,    -1,    -1,  1705,    -1,   149,
     150,    -1,   573,    -1,   575,    -1,  1516,    -1,    -1,    -1,
     160,   161,    -1,   149,    -1,    -1,   152,   153,    -1,   590,
     591,    -1,  1532,    -1,    -1,    -1,   101,    -1,   718,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
    1550,  1551,    -1,   614,    -1,   735,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   627,    -1,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   759,
      -1,  1581,   146,    -1,   764,    -1,    -1,   152,    -1,    -1,
      -1,    -1,  1592,    -1,    -1,  1595,    -1,    -1,    -1,  1502,
      -1,   781,   782,  1603,    -1,    -1,    -1,    -1,    -1,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1619,
     800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1534,    -1,    -1,  1634,    -1,    -1,    -1,  1116,  1639,
    1640,    -1,    -1,  1546,  1644,  1645,    -1,    -1,   828,    -1,
    1553,    -1,  1850,    -1,    -1,    -1,    -1,   718,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,  1870,    -1,    -1,    -1,    -1,    -1,  1581,   859,
      -1,    -1,    -1,    -1,    -1,    -1,   866,    -1,    -1,    -1,
      -1,    -1,   872,    -1,  1694,    -1,    -1,    -1,   759,    -1,
      -1,   881,    -1,    -1,   884,  1705,   886,    -1,    -1,    -1,
    1908,   891,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     781,   782,    -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   800,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   828,    -1,    -1,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1690,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   859,  1702,
    1703,    -1,  1705,    -1,    -1,   866,    -1,  1807,    -1,    -1,
      -1,    -1,    -1,   145,    -1,    -1,   996,    -1,    -1,    -1,
     881,    -1,    -1,   884,    -1,   886,    -1,    -1,    -1,    -1,
     162,    -1,  1832,  1013,  1014,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1870,  1871,   933,    -1,  1874,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1066,    -1,    -1,    -1,
      -1,    -1,  1795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1379,    -1,    -1,    -1,    -1,    -1,    -1,  1908,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1114,   996,    -1,    -1,    -1,    -1,
      -1,    -1,  1942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   283,  1013,  1014,    -1,    -1,    -1,    -1,    -1,    -1,
    1863,    -1,  1865,  1143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   321,
      -1,    -1,   324,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1066,   338,    -1,  1188,    -1,
     342,    -1,    -1,    -1,    -1,    -1,    -1,  1197,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1938,  1216,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1950,  1951,  1952,
      -1,    -1,    -1,  1114,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,  1543,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1552,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1143,    -1,  1264,  1265,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1278,  1279,
    1161,    -1,    -1,  1283,  1284,    -1,    -1,   439,    -1,    -1,
      -1,  1589,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1188,    -1,    -1,
      -1,    -1,  1312,  1313,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,  1322,  1323,  1324,  1325,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1216,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1351,  1352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   520,    -1,
      -1,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,   145,
      -1,   147,    -1,  1264,  1265,    -1,    -1,    -1,    -1,    -1,
    1688,    -1,    -1,    -1,  1692,    -1,    -1,  1278,  1279,   551,
    1400,    -1,  1283,  1284,    -1,    -1,  1704,    -1,   560,    -1,
     562,    -1,  1710,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   573,    -1,   575,  1424,   191,    -1,    -1,    -1,    -1,
      -1,  1312,  1313,    -1,    -1,    -1,    -1,    -1,   590,   591,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   614,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1351,  1352,    -1,    -1,    -1,   627,   242,    -1,    -1,   245,
      -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,  1786,    -1,
      -1,    -1,  1790,    -1,  1792,    -1,  1496,  1497,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1812,    -1,    -1,   283,    -1,  1400,
      -1,    -1,  1522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1532,    -1,    -1,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1551,    -1,    -1,    -1,  1853,    -1,    -1,    -1,  1857,
      -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,  1866,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1581,    -1,    -1,    -1,    -1,    -1,    -1,  1886,    -1,
    1888,  1889,  1592,    -1,    -1,  1595,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1603,    -1,    -1,    -1,   759,    -1,   375,
      -1,    -1,    -1,  1911,    -1,  1496,  1497,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,   781,
     782,    -1,    -1,    -1,  1634,    -1,    -1,  1935,    -1,  1639,
    1640,    -1,    -1,   409,  1644,  1645,    -1,  1945,   800,    -1,
      -1,  1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,
    1551,    -1,  1970,    -1,  1972,    -1,   828,    -1,    -1,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1988,    -1,    -1,    -1,    -1,    -1,  1994,    -1,    -1,    -1,
    1581,    -1,    -1,    -1,    82,  1705,  2004,   859,    -1,    -1,
      -1,  1592,    -1,    -1,   866,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1603,    -1,   102,    -1,    -1,    -1,    -1,   881,
      -1,    -1,   884,    -1,   886,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   514,   515,
      -1,    -1,    -1,  1634,    -1,    -1,    -1,    -1,  1639,  1640,
      -1,   139,    -1,  1644,  1645,    -1,    -1,   145,    -1,   147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   933,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     556,    -1,    -1,    -1,   560,    -1,   562,    -1,    -1,    -1,
      -1,   179,    -1,    -1,    -1,    -1,    -1,  1807,    -1,   575,
      -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,     0,    -1,
      -1,     3,    -1,    -1,  1705,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   996,    -1,    -1,    -1,    -1,    -1,
    1850,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1013,  1014,    -1,   242,    -1,    -1,   245,    -1,   635,
    1870,  1871,   250,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   648,    -1,   650,   651,    -1,   653,    -1,    -1,
      -1,    -1,    -1,   659,    76,    -1,   662,   663,   664,    -1,
      -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,  1908,    -1,
      -1,    -1,    -1,    -1,  1066,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   302,  1925,  1807,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1832,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1114,    -1,    -1,    -1,    -1,    -1,    -1,  1850,
      -1,    -1,    -1,    -1,    -1,    -1,   742,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1870,
    1871,  1143,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1908,    -1,    -1,
      -1,   409,    -1,    -1,    -1,    -1,  1188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   229,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,
      -1,  1942,   244,    -1,  1216,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   859,    -1,    -1,   278,   279,    -1,    -1,
      -1,    -1,    -1,   285,   286,    -1,   872,   873,    -1,    -1,
      -1,    -1,  1264,  1265,    -1,    -1,    -1,    -1,    -1,   301,
      -1,    -1,    -1,    -1,    -1,    -1,  1278,  1279,    -1,    -1,
      -1,  1283,  1284,    -1,    -1,    -1,   514,   515,   320,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1312,  1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,
      -1,    -1,   560,    -1,   562,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   376,    -1,    -1,   575,    -1,  1351,
    1352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1400,    -1,
      -1,   433,    -1,    -1,    -1,   437,    -1,   635,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     648,    -1,   650,   651,   456,   653,    -1,    -1,   460,   461,
      -1,   659,   464,    86,   662,   663,   664,    -1,    -1,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,   479,   480,   481,
     482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,    -1,
      -1,    -1,    -1,   126,   506,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1106,    -1,    -1,    -1,  1496,  1497,    -1,    -1,  1114,    -1,
      -1,    -1,   534,    -1,    -1,    -1,    -1,    -1,     1,    -1,
      -1,    -1,    -1,    -1,   742,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1143,    -1,    -1,
    1532,    -1,    -1,   565,    -1,    -1,    -1,    -1,    -1,    -1,
     572,    -1,    -1,    -1,    -1,  1161,   578,    -1,    -1,  1551,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,  1181,    -1,    -1,    -1,   601,
     602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,  1581,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1603,    -1,    -1,  1220,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,   859,  1634,   665,    -1,    -1,   129,  1639,  1640,   292,
      -1,    -1,  1644,  1645,   872,   873,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   734,  1705,    -1,    -1,  1322,  1323,  1324,  1325,
    1326,  1327,    -1,    -1,    -1,    -1,    -1,   749,    -1,    -1,
      -1,   753,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     762,    -1,    -1,    -1,    -1,  1351,  1352,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   793,    -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   438,    -1,   440,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   449,   450,    -1,    -1,
      -1,    -1,    -1,    -1,   836,  1807,    -1,    -1,  1424,    -1,
      -1,   843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   870,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1850,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1870,  1871,
      -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,  1106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1114,    -1,    -1,    -1,
      -1,   923,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,
    1516,    -1,    -1,    -1,   557,    -1,  1908,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1143,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1161,    -1,  1551,   147,    -1,    -1,    -1,
    1942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,    -1,  1181,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,   179,    -1,
      -1,   119,    -1,    -1,  1006,    -1,    -1,    -1,  1010,  1595,
      -1,   192,    -1,    -1,   132,  1017,   134,    -1,    -1,    -1,
      -1,    -1,  1220,    -1,    -1,  1027,    -1,    -1,    -1,    -1,
      -1,    -1,  1034,  1619,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1043,    -1,  1045,    -1,    -1,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   181,   245,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1077,    -1,    -1,    -1,  1081,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1095,    -1,    -1,  1098,    -1,    -1,    -1,
      -1,   219,    -1,    -1,    -1,   223,    -1,    -1,   226,   227,
      -1,    -1,   230,    -1,    -1,   233,   234,    -1,    -1,  1705,
      -1,   302,    -1,    -1,  1322,  1323,  1324,  1325,  1326,  1327,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   775,  1351,  1352,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   295,    -1,    -1,
     298,    -1,    -1,    -1,  1186,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1211,
      -1,    -1,    -1,    -1,    -1,    -1,   334,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1424,    -1,   409,   852,
     853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     863,   864,   865,    -1,    -1,   868,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,    -1,
      -1,    -1,    -1,    -1,  1850,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1870,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,  1304,   111,    -1,   113,  1308,    -1,    -1,    -1,
     428,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1516,    -1,
      -1,    -1,  1908,    -1,   947,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   514,   515,    -1,  1338,    -1,    -1,   520,
      -1,    -1,    -1,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,  1551,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,    -1,    -1,
     993,    -1,    -1,    -1,    -1,    -1,    -1,   495,    -1,    -1,
      -1,    -1,    -1,  1581,    -1,    -1,  1388,   195,    -1,  1391,
      -1,    -1,   573,    -1,    -1,    -1,    -1,  1595,    -1,    -1,
      -1,    -1,   178,    -1,  1406,    -1,    -1,    -1,    -1,    -1,
     591,    -1,    -1,    -1,    -1,    -1,    -1,  1040,    -1,    -1,
      -1,  1619,    -1,    -1,    -1,    -1,  1049,  1050,  1051,  1052,
      -1,    -1,    -1,   614,  1057,  1058,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1067,    -1,    -1,    -1,    -1,    -1,
     258,   569,   260,   261,   635,  1457,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1466,  1088,    -1,  1090,  1470,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     288,    -1,  1484,  1485,    -1,    -1,   294,    -1,    -1,    -1,
      -1,   609,   610,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   622,    -1,    -1,  1705,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,
    1143,   329,    -1,   331,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1169,    -1,    -1,    -1,
      -1,    -1,    -1,  1176,    -1,  1178,  1179,    -1,    -1,    -1,
      -1,   742,    -1,    -1,  1187,    -1,  1189,    -1,  1191,    -1,
    1193,    -1,    -1,    -1,    -1,  1198,    -1,    -1,   759,    -1,
      -1,    -1,    -1,    -1,   360,    -1,    -1,   363,   364,    -1,
      -1,    -1,    -1,    -1,  1596,  1597,    -1,   373,   374,    -1,
     781,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   419,   388,   389,    -1,    -1,    -1,    -1,    -1,   800,
     738,   739,    -1,    -1,    -1,    -1,   744,    -1,    -1,    -1,
      -1,    -1,    -1,   409,    -1,    -1,  1259,   445,    -1,   447,
     448,    -1,    -1,  1266,  1267,    -1,    -1,   765,    -1,    -1,
     768,   769,  1850,   771,    -1,   773,   774,    -1,    -1,    -1,
      -1,    -1,    -1,   439,    -1,    -1,    -1,  1290,    -1,    -1,
      -1,    -1,  1870,    -1,  1297,    -1,    -1,    -1,  1301,    -1,
      -1,    -1,    -1,    -1,   492,   866,    -1,    -1,    -1,    -1,
      -1,    -1,   873,    -1,   812,    -1,    -1,    -1,   816,    -1,
      -1,    -1,    -1,    -1,   512,    -1,    -1,    -1,  1331,   517,
    1908,   519,    -1,  1715,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   539,    -1,   541,   542,    -1,    -1,  1739,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1371,    -1,
      -1,   559,   933,    -1,    -1,  1757,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   571,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   892,    -1,    -1,    -1,    -1,    -1,
      -1,  1404,  1784,    -1,    -1,   593,    -1,   595,   596,  1412,
      -1,  1414,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1803,    -1,    -1,  1806,    -1,    -1,    -1,    -1,   617,
     618,    -1,    -1,    -1,    -1,    -1,   624,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1461,  1462,
      -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,   657,
     658,    -1,    -1,  1476,  1477,    -1,  1479,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1488,    -1,    -1,    -1,    -1,
      -1,   191,   192,    -1,    -1,  1498,  1499,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1066,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1897,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   223,    -1,  1023,    -1,    -1,    -1,    -1,
     230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   695,
     696,   697,   698,   699,   700,   701,   702,   703,   704,   705,
     706,   707,   708,   709,   710,   711,   712,   713,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1064,    -1,    -1,    -1,
      -1,    -1,    -1,  1071,    -1,    -1,  1074,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,
      -1,  1604,  1605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1615,    -1,    -1,    -1,    -1,    -1,    -1,   775,
    1181,   321,   322,    -1,    -1,    -1,    -1,  1188,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   342,    -1,    -1,    -1,    -1,  1650,  1651,    -1,
      -1,    -1,    -1,    -1,    -1,  1216,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   851,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   883,  1194,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1202,  1203,   894,    -1,    -1,    -1,
      -1,    -1,   412,    -1,    -1,   903,    -1,    -1,    -1,    -1,
      -1,    -1,  1283,  1284,    -1,    -1,    -1,  1730,   428,   429,
      -1,   431,   432,    -1,    -1,    -1,    -1,    -1,    -1,   439,
      -1,    -1,    -1,   443,    -1,    -1,  1749,    -1,    -1,  1752,
    1753,    -1,    -1,    -1,    -1,   911,  1759,    -1,    -1,    -1,
      -1,    -1,  1260,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1269,    -1,   929,  1272,    -1,  1274,  1275,   934,    -1,
      -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,   488,   945,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1000,    -1,    -1,    -1,    -1,  1315,    -1,    -1,
     520,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   987,    -1,    -1,    -1,    -1,  1024,    -1,    -1,  1400,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1872,
     570,    -1,    -1,   573,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,  1382,    -1,    -1,    -1,    -1,    -1,
     590,   591,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,   601,    -1,    -1,    -1,   605,    -1,    -1,    -1,    -1,
      -1,    -1,   612,    -1,   614,    -1,    -1,    -1,    -1,  1922,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1946,   117,   118,   119,    -1,   121,   122,
    1953,    -1,    -1,    -1,    -1,  1516,   129,    -1,  1114,    -1,
      -1,    -1,    -1,    -1,    -1,  1968,    -1,  1465,    -1,    -1,
      -1,    -1,  1160,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,  1494,    -1,    -1,    -1,
     173,   174,    -1,    -1,  1192,    -1,    -1,    -1,    -1,    -1,
    1198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,
      -1,    -1,  1520,    -1,    -1,    -1,    -1,    -1,  1526,    -1,
      -1,  1592,  1188,    -1,   734,   735,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   744,   745,    -1,   747,   748,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   759,
      -1,    -1,   762,    -1,   764,   765,    -1,  1223,  1224,  1225,
      -1,   771,    -1,    -1,  1230,  1231,    47,    -1,    -1,    -1,
      -1,   781,   782,  1644,  1645,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,  1254,    -1,
     800,    -1,    -1,    -1,   804,    -1,    -1,    -1,   808,    -1,
      -1,    -1,   812,   813,    -1,    -1,   816,   817,    -1,    -1,
      -1,    -1,    -1,    -1,   824,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1291,  1292,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1333,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,   132,    -1,   134,    -1,    -1,   866,   867,  1666,  1667,
      -1,    -1,    -1,    98,    -1,    -1,  1674,    -1,    -1,    -1,
    1678,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1381,   894,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,
      -1,    -1,   147,    -1,    -1,    -1,   151,    -1,    -1,    -1,
      -1,    -1,    -1,   933,    -1,    -1,    -1,   162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   226,   227,    -1,    -1,   230,
      -1,    -1,   233,   234,    -1,    -1,   140,   192,    -1,   143,
     195,  1832,    -1,    -1,  1772,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1000,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,  1018,    -1,
     245,    -1,    -1,    -1,  1024,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   258,    -1,    -1,   210,    -1,  1836,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   319,    -1,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,  1550,   334,    -1,    -1,  1066,    -1,    -1,   294,
      -1,  1071,  1072,    71,  1074,  1075,    -1,   302,    -1,    -1,
      -1,   255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   265,    -1,    -1,    -1,    -1,   321,    -1,   323,    -1,
      98,    99,   276,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,    -1,  1924,    -1,    -1,   303,
    1586,   129,    -1,    -1,    -1,    -1,   310,   311,    -1,    -1,
      -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     375,   149,   150,    -1,   152,   153,    -1,   428,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,   347,    -1,    -1,    -1,    -1,   352,    -1,
      -1,   355,    -1,    -1,   409,    -1,    -1,    -1,  1188,  1677,
      -1,    -1,    -1,    -1,  1194,  1195,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1694,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   439,    -1,  1216,    -1,    -1,    -1,
     445,    -1,    -1,    -1,   495,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1260,  1261,    -1,    -1,    -1,    -1,    -1,    -1,   442,  1269,
    1270,    -1,  1272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     454,   455,  1770,  1283,  1284,    -1,    -1,    -1,    -1,   514,
     515,    -1,    -1,    -1,    -1,   520,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    -1,    -1,  1782,    -1,    -1,    -1,
      -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,   609,   610,
      -1,    -1,    -1,    -1,    -1,   205,    -1,   207,   573,    -1,
      -1,   622,    -1,    -1,    -1,    -1,    -1,    -1,  1814,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   591,    -1,   593,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1874,    -1,    -1,   614,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1400,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     635,   585,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   648,    -1,   650,   651,    -1,   653,    -1,
      -1,    -1,    -1,   293,   659,    -1,    -1,   662,   663,   664,
      -1,    -1,    -1,    -1,    -1,    -1,  1902,    -1,  1904,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   738,   739,  1957,
      -1,    -1,    -1,   744,    -1,  1931,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   718,   765,    -1,    -1,   768,   769,  1955,
     771,    -1,   773,   774,    -1,    -1,    -1,    -1,    -1,    -1,
    1510,    -1,    -1,    -1,    -1,    -1,    -1,   742,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1526,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   759,    -1,    -1,    -1,    -1,    -1,
      -1,   812,    -1,    -1,  2000,   816,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   781,   782,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   800,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   776,    -1,    -1,   465,    -1,    -1,    -1,   783,
      -1,   471,    -1,    -1,    -1,    -1,   476,    -1,    -1,    -1,
      -1,   892,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   866,    -1,    -1,  1644,  1645,    -1,    -1,   873,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,  1659,
    1660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1675,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   858,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   563,    -1,    -1,    -1,    -1,   933,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   591,    -1,    -1,    -1,   909,    -1,    -1,    -1,   913,
      -1,    -1,    -1,    -1,   604,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1778,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1064,    -1,   655,    -1,    -1,    -1,    -1,
    1071,    -1,    -1,  1074,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   676,   677,    -1,    -1,
     680,    -1,   682,    -1,    -1,    -1,    -1,    -1,   688,    -1,
     690,   691,  1832,    -1,    -1,    -1,  1836,  1837,    -1,    -1,
    1840,  1066,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   731,    -1,  1873,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1106,   742,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   756,    -1,    -1,   759,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1194,  1924,  1925,   786,    -1,    -1,   789,
      -1,  1202,  1203,    -1,    -1,  1160,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1119,  1120,  1121,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1181,  1957,    -1,    -1,
      -1,    -1,    -1,  1188,    -1,   825,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1149,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1260,
    1164,  1216,    -1,    -1,    -1,  1220,    -1,    -1,  1269,    -1,
      -1,  1272,    -1,  1274,  1275,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   883,   884,    -1,    -1,  1201,    -1,    -1,
      -1,   891,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1315,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1283,  1284,
     920,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   941,    -1,     5,    -1,    -1,    -1,    -1,   948,    -1,
      12,    13,    14,    15,    16,    -1,    -1,  1322,  1323,  1324,
      -1,  1326,  1327,    -1,    -1,    -1,    -1,    -1,  1333,    -1,
      -1,  1382,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,   994,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,  1381,    -1,    -1,    -1,
      -1,    -1,  1336,    -1,    -1,  1339,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1400,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1465,   117,   118,   119,    -1,   121,
     122,  1061,    -1,  1063,    -1,  1065,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1494,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1131,  1132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,  1516,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
    1591,    50,    51,    -1,    -1,  1550,    -1,    -1,    -1,    -1,
      -1,    -1,  1192,    -1,    -1,    -1,    -1,    -1,  1198,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,  1207,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1529,  1216,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1592,    -1,    -1,
      -1,    -1,   101,  1233,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1248,    -1,
      -1,  1251,    -1,    -1,  1619,  1666,  1667,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1678,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1644,
    1645,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1611,    -1,    -1,
      -1,    -1,    -1,  1303,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1694,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,  1341,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1359,
      -1,  1772,  1362,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
    1400,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1410,  1411,    -1,  1727,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,  1736,    -1,   157,  1836,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,  1436,    -1,  1438,    -1,
      -1,    -1,  1807,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1832,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,  1871,    -1,    -1,  1874,
      -1,    -1,    -1,    -1,    -1,    -1,  1516,    -1,    -1,    -1,
      69,  1521,    71,    72,    -1,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,     1,   121,   122,    -1,    -1,    -1,  1577,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,  1617,    48,    -1,
    1620,    -1,    52,    -1,    54,   174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      -1,    71,    72,    -1,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    -1,    96,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
       1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,   174,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,
      71,    72,    -1,    74,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    -1,    96,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,  1815,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   174,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    48,
      -1,    50,    51,    52,    -1,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,
      69,    70,    71,    72,    -1,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,   100,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    48,    -1,    50,    51,    52,    -1,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      67,    -1,    69,    70,    71,    72,    -1,    74,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
      -1,    98,    99,   100,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   173,   174,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    48,    -1,    50,    51,    52,
      -1,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    67,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,   100,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     173,   174,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    48,    -1,    50,
      51,    52,    -1,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    67,    -1,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,   100,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
     151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      48,    -1,    50,    51,    52,    -1,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    67,
      -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    99,   100,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,    -1,    50,    51,
      52,    -1,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,    -1,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    67,    -1,    -1,    70,     5,    -1,    -1,
      -1,    75,    76,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    96,    -1,    -1,    -1,   100,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,   116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    70,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,   101,    50,    51,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      67,    -1,    -1,    70,    -1,    -1,    -1,    -1,    75,    76,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    67,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,     3,    -1,    -1,
      -1,   160,   161,     9,    -1,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,
       3,    -1,    -1,    -1,   160,   161,     9,    -1,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,   152,
     153,    -1,    -1,     3,    -1,    -1,    -1,   160,   161,     9,
      -1,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,
      -1,    -1,   152,   153,    -1,    -1,     3,    -1,    -1,    -1,
     160,   161,     9,    -1,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,    70,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,   101,   152,   153,    -1,    12,    13,    14,
      15,    16,   160,   161,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,    70,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    48,   152,   153,    -1,    52,    -1,    54,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    71,    -1,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    -1,    71,    -1,    -1,    74,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    -1,    96,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    12,    13,    14,
      15,    16,    17,    70,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,   104,   105,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,   153,    -1,    -1,   104,
     105,    -1,    -1,   160,   161,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   152,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    12,    13,    14,    15,    16,
      17,    70,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,   104,   105,    -1,
      -1,   160,   161,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,   152,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    19,    70,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    48,    -1,    50,    51,    52,    -1,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    48,    -1,    50,    51,    52,
      -1,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    48,
      -1,    50,    51,    52,    -1,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    48,    -1,    50,
      51,    52,    -1,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    48,    -1,    50,    51,    52,    -1,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    12,    13,    14,    15,    16,   160,   161,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    12,    13,    14,    15,    16,    17,    70,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,   104,   105,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    12,    13,    14,    15,    16,
      17,    70,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,   152,    -1,    50,    51,   104,   105,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    76,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,   152,   153,    -1,    -1,   104,   105,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    12,    13,    14,
      15,    16,    17,    70,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,   104,   105,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   104,
     105,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    12,
      13,    14,    15,    16,    17,    70,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,   104,
     105,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,   104,   105,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    12,    13,    14,    15,    16,    17,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,   104,   105,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,   104,   105,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    17,    70,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,   104,   105,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    12,    13,    14,    15,    16,    17,
      70,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
     152,    -1,    50,    51,   104,   105,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    -1,   104,   105,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    12,    13,    14,    15,
      16,    17,    70,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   104,   105,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    12,
      13,    14,    15,    16,   160,   161,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    19,    70,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,   104,   105,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,   152,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,   152,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
     104,   105,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,   152,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,   152,    49,    50,    51,    -1,    53,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    48,   121,   122,    -1,    52,    -1,    54,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,   152,    -1,    -1,
      -1,    -1,   149,   150,   151,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    48,   121,   122,    -1,    52,    -1,    54,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,   156,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,    -1,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    48,   121,   122,    -1,    52,    -1,    54,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    48,   121,   122,    -1,    52,    -1,    54,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   176,   385,   386,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    50,    51,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    67,    70,    71,    96,
     100,   101,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   116,   129,   149,   150,   152,   153,   160,   161,   179,
     180,   181,   196,   277,   278,   279,   280,   281,   282,   283,
     284,   285,   286,   287,   288,   290,   292,   294,   295,   296,
     297,   298,   299,   300,   301,   302,   304,   306,   307,   308,
     310,   311,   315,   316,   317,   318,   319,   321,   327,   328,
     329,   330,   341,   344,   377,   380,   390,   396,   398,   404,
     408,   413,   414,   415,   416,   417,   418,   419,   420,   440,
     457,   458,   459,   460,     0,   176,   180,   196,   281,   283,
     292,   295,   307,   311,   316,   115,   149,    56,    59,    60,
      62,   149,   149,   402,   403,   404,   303,   304,   104,   105,
     180,   357,   378,   379,   357,   149,   390,   149,   149,   149,
     196,   403,   408,   414,   415,   416,   418,   419,   420,   104,
     318,   154,   176,   284,   292,   295,   413,   417,   456,   457,
     460,   461,   174,   177,   146,   157,   173,   217,   360,    87,
     155,   397,   357,   177,   177,   177,   174,   104,   105,   149,
     196,   289,   399,   408,   409,   410,   411,   412,   413,   417,
     421,   422,   423,   424,   425,   431,     3,    46,    47,    49,
      53,   309,     3,     4,   153,   196,   283,   296,   300,   302,
     312,   317,   393,   413,   417,   460,   281,   283,   295,   307,
     311,   316,   394,   413,   417,    63,   301,   301,   296,   302,
     301,   296,   301,   296,   152,   402,   155,   177,   149,   157,
     225,   402,   402,   176,   272,   273,   153,   292,   295,   458,
     357,   357,   390,   173,   295,   149,   196,   399,   408,   413,
     422,   153,   196,   460,   391,   392,    63,    64,    65,    66,
     153,   171,   357,   366,   368,   372,   374,   375,   317,    55,
     153,   196,   291,   295,   299,   300,   306,   307,   313,   314,
     315,   316,   320,   327,   328,   344,   353,   355,   440,   452,
     453,   454,   455,   460,   461,   104,   105,   157,   180,   317,
     431,   404,   149,   373,   374,   149,   149,   115,   182,   183,
      48,    52,    54,    71,    98,    99,   101,   103,   113,   114,
     117,   118,   119,   121,   122,   149,   153,   159,   162,   163,
     164,   165,   178,   179,   182,   184,   187,   195,   196,   197,
     198,   201,   202,   203,   204,   205,   206,   207,   208,   209,
     210,   211,   212,   213,   219,   317,   151,   153,   195,   196,
     212,   214,   292,   317,   358,   359,   376,   456,   461,   295,
     414,   415,   416,   418,   419,   420,   151,   151,   151,   151,
     151,   151,   151,   153,   292,   440,   458,   153,   160,   196,
     214,   283,   284,   291,   293,   295,   307,   314,   316,   348,
     349,   352,   353,   354,   452,   460,   149,   413,   417,   460,
     149,   155,   101,   152,   153,   157,   179,   181,   214,   361,
     362,   363,   364,   365,    21,   361,   149,   357,   225,   149,
     155,   155,   155,   403,   408,   410,   411,   412,   421,   423,
     424,   425,   295,   409,   422,   155,    96,   401,   153,   402,
     439,   440,   402,   402,   397,   272,   149,   402,   439,   397,
     402,   402,   295,   399,   149,   149,   294,   295,   292,   295,
     176,   292,   456,   461,   319,   157,   397,   272,   357,   360,
     283,   300,   395,   413,   417,   157,   397,   272,   378,   295,
     307,   295,   295,   104,   318,   104,   105,   180,   317,   322,
     378,   176,   180,   356,   148,   176,     3,   288,   290,   295,
     299,   225,   176,   176,   401,   149,   401,   177,   214,   403,
     408,   295,   149,   176,   357,   388,   157,   357,   157,   357,
     131,   160,   161,   371,   151,   155,   357,   375,   151,   402,
     154,   176,   293,   295,   307,   314,   316,   451,   452,   460,
     461,   149,   153,   161,   173,   196,   440,   441,   442,   443,
     444,   445,   446,   463,   196,   320,   460,   295,   314,   301,
     296,   402,   151,   293,   295,   453,   293,   440,   453,     9,
     345,   357,   342,   157,   366,   173,   366,    12,    86,   101,
     104,   105,   179,   405,   406,   407,   151,   115,   149,   195,
     149,   149,   198,   149,   195,   149,   101,   295,   308,   149,
     195,   195,    18,    20,    83,   153,   162,   163,   199,   200,
     214,   221,   225,   330,   358,   460,   155,   176,   149,   184,
     153,   158,   153,   158,   118,   120,   121,   122,   149,   152,
     153,   157,   158,   198,   198,   166,   160,   167,   168,   162,
     163,   123,   124,   125,   126,   169,   170,   127,   128,   161,
     159,   171,   129,   130,   172,   151,   155,   152,   176,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     173,   216,   217,   218,   149,   196,   435,   436,   437,   438,
     439,   151,   155,   151,   151,   151,   151,   151,   151,   149,
     402,   439,   440,   149,   439,   440,   176,   292,   458,   176,
     177,   177,   149,   161,   196,   408,   426,   427,   428,   429,
     430,   431,   432,   433,   434,   131,   460,   177,   177,   357,
     357,   176,   176,   176,   153,   181,   176,   362,   156,   155,
     462,   361,   152,   153,   156,   365,   150,   214,   220,   149,
     176,   176,   176,   176,   408,   410,   411,   412,   421,   423,
     424,   425,   151,   151,   151,   151,   151,   151,   151,   409,
     422,   402,   149,   360,   154,   176,   225,   397,   176,   225,
     399,   221,   359,   221,   359,   399,   388,   225,   397,   401,
     157,   397,   272,   388,   225,   397,   324,   325,   323,   157,
     131,   295,   350,   351,   354,   355,   151,   155,    68,   274,
     275,   177,   295,   288,   160,   214,   176,   408,   349,   390,
     388,   154,   176,   149,   370,   368,   369,    76,   305,   180,
     293,   440,   453,   295,   299,   460,   176,   442,   443,   444,
     154,   176,    17,   214,   295,   441,   463,   402,   402,   440,
     293,   451,   461,   295,   180,   402,   293,   453,   317,   155,
     462,   173,   346,   157,   345,   151,   359,   151,   151,   155,
     149,   174,   358,   153,   358,   358,   358,   214,   358,   151,
     358,   358,   358,   176,   151,   162,   163,   200,    17,   297,
     151,   155,   151,   160,   161,   151,   220,   214,   157,   214,
     180,   214,   180,   113,   153,   180,   150,   188,   189,   190,
     214,   113,   153,   180,   330,   214,   188,   180,   198,   201,
     201,   201,   202,   202,   203,   203,   204,   204,   204,   204,
     205,   205,   206,   207,   208,   209,   210,   156,   221,   174,
     182,   153,   180,   214,   157,   214,   176,   436,   437,   438,
     295,   435,   402,   402,   214,   359,   149,   402,   439,   440,
     149,   439,   440,   176,   176,   154,   154,   149,   408,   427,
     428,   429,   432,    17,   295,   426,   430,   149,   402,   445,
     463,   402,   402,   463,   149,   402,   445,   402,   402,   177,
     213,   357,   154,   155,   154,   155,   463,   463,   131,   347,
     348,   349,   347,   357,   176,   212,   213,   214,   400,   462,
     361,   363,   148,   176,   151,   155,   176,   347,   180,   399,
     180,   151,   151,   151,   151,   151,   151,   149,   402,   439,
     440,   149,   402,   439,   440,   399,   182,   440,   214,   225,
     350,   151,   151,   151,   151,   386,   387,   225,   388,   225,
     397,   387,   225,   157,   157,   157,   331,   177,   177,   180,
     276,   357,    17,    69,    71,    74,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    90,    91,
      92,    93,    94,    96,   104,   105,   116,   176,   221,   222,
     223,   224,   225,   226,   227,   229,   230,   240,   244,   245,
     246,   247,   248,   249,   254,   255,   261,   262,   263,   277,
     295,   299,   357,   398,    68,   174,   177,   177,   177,   347,
     177,   389,   387,   281,   283,   292,   381,   382,   383,   384,
     376,   173,   367,   367,   293,   453,   153,   160,   196,   214,
     317,   214,   295,   350,   151,   151,   151,     5,   295,   402,
     441,   157,   180,   431,     9,   357,   148,   157,   213,   345,
     462,   157,   151,   406,   188,   151,   176,   155,   151,   151,
     155,   151,   198,   151,   151,   151,   198,    17,   297,   214,
     151,   151,   150,   157,   198,   154,   177,   188,   154,   154,
     113,   117,   119,   181,   191,   192,   193,   151,   155,   191,
     154,   155,   148,   212,   156,   151,   191,   177,   362,   350,
     151,   151,   151,   435,   176,   176,   350,   350,   432,   151,
     151,   151,   151,   149,   408,   431,   426,   430,   176,   176,
     154,   177,   463,   176,   176,   177,   177,   177,   177,   360,
     191,   131,   165,   177,   177,   148,   361,   214,   402,   150,
     214,   347,   177,   173,   149,   402,   439,   440,   149,   402,
     439,   440,   176,   176,   401,   151,   177,   177,   389,   387,
     225,   389,   331,   331,   331,     3,     9,    71,   148,   278,
     285,   286,   292,   295,   332,   337,   456,   151,   155,   155,
     174,   149,    59,    60,   174,   225,   277,   398,   149,    17,
     223,   149,   149,   174,   357,   174,   357,   160,   357,   157,
     222,   149,   149,   149,   225,   214,   215,   215,    13,   264,
      72,   231,   174,   177,   227,    76,   174,   357,    89,   250,
     356,   295,   156,   276,   174,   154,   154,   177,   155,   389,
     399,   177,   174,   177,   174,   177,   151,   359,   373,   373,
     176,   177,   177,   177,   214,   177,   149,   402,   445,   440,
     294,     5,   160,   177,   214,   345,   402,   402,   317,   346,
     362,   462,   148,   148,   176,   151,   180,    76,   185,   186,
     358,   198,   198,   198,   198,   198,   157,   362,   155,   148,
     194,   153,   192,   194,   194,   154,   155,   120,   152,   190,
     154,   220,   212,   174,   154,   462,   177,   149,   402,   439,
     440,   350,   350,   177,   177,   151,   149,   402,   439,   440,
     149,   402,   445,   408,   402,   402,   350,   350,   154,   349,
     352,   352,   353,   151,   155,   155,   151,   177,   213,   213,
     154,   154,   177,   177,   151,   214,   176,   176,   350,   350,
     360,   402,   155,   151,   148,   389,   148,   148,   148,   148,
     292,   330,   338,   456,   292,   337,   149,   326,   174,   174,
     149,   156,   196,   333,   334,   340,   408,   409,   422,   155,
     174,   357,   176,   357,   151,   188,   189,   174,   225,   174,
     225,   221,    78,   151,   221,   232,   277,   279,   282,   288,
     295,   299,   151,   173,   174,   221,   241,   242,   277,   174,
     174,   221,   174,   362,   174,   221,   220,   221,   108,   109,
     110,   111,   112,   256,   258,   259,   174,    95,   174,    82,
     149,   149,   177,   148,   174,   174,   149,   223,   225,   402,
     174,   151,   176,   148,   148,   176,   155,   155,   154,   154,
     154,   177,   151,   176,   214,   214,   177,   154,   177,   462,
     343,   157,   346,   462,   148,   381,   151,   156,   151,   155,
     156,   362,   462,   220,   118,   191,   192,   153,   192,   153,
     192,   154,   148,   151,   176,   177,   177,   151,   151,   176,
     176,   177,   177,   177,   176,   176,   154,   177,   151,   402,
     350,   350,   177,   177,   221,   148,   326,   326,   326,   149,
     196,   335,   336,   439,   447,   448,   449,   450,   174,   155,
     174,   333,   174,   376,   403,   408,   214,   295,   155,   174,
     339,   340,   339,   357,   131,   354,   355,   221,   151,   151,
     149,   223,   151,   221,   295,   223,   221,   222,   143,   144,
     145,   165,   174,   243,   151,   156,   222,   174,   462,   151,
     151,   151,   225,   258,   259,   149,   214,   149,   182,   232,
     198,   251,   107,     1,   223,   402,   382,   176,   176,   154,
     350,   177,   177,   154,   154,   148,   157,   345,   148,   177,
     214,   186,   214,   462,   148,   154,   154,   191,   191,   350,
     151,   151,   350,   350,   151,   151,   154,   155,   131,   349,
     131,   154,   177,   177,   151,   151,   154,   448,   449,   450,
     295,   447,   155,   174,   402,   402,   174,   151,   408,   402,
     174,   223,    75,    76,   157,   235,   236,   237,   151,   221,
      73,   223,    73,   174,   104,   173,   221,   222,   221,   223,
     242,   174,   148,   157,   237,   223,   149,   176,   174,   182,
     151,   156,   151,   151,   155,   156,   249,   253,   357,   399,
     177,   154,   154,   345,   462,   148,   148,   154,   154,   177,
     177,   177,   176,   177,   151,   151,   151,   151,   151,   447,
     402,   334,     1,   213,   233,   234,   400,     1,   156,     1,
     176,   223,   235,    73,   174,   151,   223,    73,   223,   222,
     221,   144,   165,   243,   174,   165,    73,   222,   174,     1,
     176,   176,   260,   293,   295,   456,   156,   174,   153,   182,
     265,   266,   267,   223,   198,   188,    73,   106,   250,   252,
     151,   462,   148,   151,   151,   151,   352,   149,   402,   439,
     440,   336,   131,     1,   155,   156,   148,   270,   271,   277,
     223,    73,   174,   223,   150,   150,   221,   222,   221,   223,
     148,   270,   260,   177,   149,   196,   399,   447,   180,   156,
     101,   149,   151,   156,   155,    73,   151,   223,   149,   223,
     223,   148,   176,   213,   233,   236,   238,   239,   277,   223,
     165,   165,   165,   238,   177,   174,   257,   295,   265,   154,
     213,   174,   265,   267,   223,   221,   107,   107,   350,   223,
     228,   177,   236,   221,   150,   221,   221,   177,   257,   212,
     151,   156,   182,   151,   151,   156,   151,   253,    73,   248,
     177,     1,   223,   148,   228,   148,   151,   225,   182,   268,
     149,   174,   268,   223,    73,   151,   225,   155,   156,   213,
     151,   223,   182,   180,   269,   151,   174,   151,   155,   174,
     180
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   175,   176,   177,   178,   178,   178,   178,   178,   179,
     179,   179,   179,   179,   179,   179,   180,   180,   181,   181,
     182,   183,   183,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   185,   185,   186,   186,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   188,   188,   189,   189,   190,   190,   191,   191,   192,
     192,   192,   192,   192,   192,   192,   193,   193,   193,   194,
     194,   195,   195,   195,   195,   195,   195,   195,   195,   195,
     195,   195,   195,   195,   195,   196,   196,   196,   197,   197,
     197,   197,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   199,   199,   199,   199,   200,   200,   201,   201,   202,
     202,   202,   202,   203,   203,   203,   204,   204,   204,   205,
     205,   205,   205,   205,   206,   206,   206,   207,   207,   208,
     208,   209,   209,   210,   210,   211,   211,   212,   212,   212,
     213,   214,   214,   214,   215,   215,   216,   216,   217,   217,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   218,
     218,   219,   219,   220,   220,   220,   220,   221,   221,   222,
     222,   223,   223,   223,   223,   223,   223,   223,   223,   223,
     223,   223,   223,   223,   224,   224,   225,   225,   226,   226,
     227,   227,   227,   227,   227,   228,   228,   228,   229,   229,
     230,   230,   230,   230,   230,   230,   230,   231,   231,   232,
     232,   232,   232,   233,   233,   233,   234,   234,   235,   235,
     235,   235,   235,   236,   236,   237,   238,   238,   239,   239,
     240,   240,   240,   240,   240,   240,   240,   240,   240,   240,
     240,   240,   241,   241,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   243,   243,   243,   243,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   245,   245,   246,   247,
     248,   249,   249,   250,   250,   251,   251,   252,   253,   253,
     253,   253,   253,   253,   254,   254,   255,   255,   255,   256,
     256,   257,   257,   258,   258,   258,   258,   259,   260,   260,
     260,   260,   260,   261,   262,   262,   263,   263,   263,   263,
     263,   264,   264,   265,   265,   266,   266,   267,   267,   268,
     268,   268,   269,   269,   270,   270,   271,   271,   272,   272,
     273,   273,   274,   274,   275,   275,   276,   276,   277,   277,
     277,   278,   278,   279,   279,   279,   279,   279,   280,   280,
     280,   281,   281,   281,   282,   282,   282,   282,   282,   283,
     283,   284,   284,   285,   285,   285,   286,   286,   286,   286,
     286,   287,   287,   288,   288,   288,   288,   289,   289,   290,
     290,   290,   291,   291,   291,   292,   292,   292,   293,   293,
     293,   294,   294,   295,   295,   296,   296,   297,   297,   297,
     297,   297,   298,   299,   299,   299,   300,   300,   301,   301,
     301,   301,   301,   301,   301,   301,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   303,   303,   304,   305,   305,   306,
     306,   306,   306,   306,   307,   307,   308,   308,   308,   308,
     309,   309,   309,   309,   309,   309,   310,   310,   310,   310,
     311,   312,   311,   311,   313,   313,   313,   313,   314,   314,
     314,   315,   315,   315,   315,   316,   316,   316,   317,   317,
     317,   317,   317,   317,   318,   318,   318,   319,   319,   320,
     320,   322,   321,   323,   321,   324,   321,   325,   321,   321,
     326,   326,   327,   327,   328,   328,   329,   329,   329,   330,
     330,   330,   330,   330,   330,   330,   330,   331,   331,   332,
     332,   332,   332,   332,   332,   332,   332,   332,   332,   333,
     333,   333,   334,   334,   334,   335,   335,   335,   336,   337,
     337,   338,   338,   339,   339,   340,   341,   342,   341,   341,
     341,   343,   341,   341,   341,   344,   344,   345,   345,   345,
     345,   346,   346,   346,   347,   347,   347,   347,   347,   347,
     347,   348,   348,   348,   348,   349,   349,   350,   350,   350,
     350,   351,   351,   351,   351,   352,   352,   352,   352,   352,
     353,   353,   353,   353,   353,   354,   354,   355,   355,   356,
     356,   357,   357,   357,   358,   358,   358,   359,   359,   360,
     360,   360,   360,   361,   361,   362,   362,   362,   362,   362,
     363,   363,   364,   364,   365,   365,   365,   365,   365,   366,
     366,   367,   367,   369,   368,   370,   368,   368,   368,   371,
     371,   371,   371,   372,   372,   372,   372,   373,   373,   374,
     374,   375,   375,   376,   376,   376,   376,   377,   377,   377,
     378,   378,   379,   379,   380,   380,   381,   381,   382,   382,
     383,   383,   383,   384,   384,   385,   385,   386,   386,   387,
     387,   388,   389,   390,   390,   390,   390,   390,   391,   390,
     392,   390,   393,   390,   394,   390,   395,   390,   396,   396,
     396,   397,   397,   398,   398,   398,   398,   398,   398,   398,
     398,   398,   398,   399,   399,   399,   400,   401,   401,   402,
     402,   403,   403,   404,   405,   405,   406,   406,   406,   407,
     407,   407,   407,   407,   407,   408,   408,   409,   409,   409,
     409,   410,   410,   410,   410,   411,   411,   411,   411,   411,
     411,   411,   412,   412,   412,   412,   413,   413,   413,   414,
     414,   414,   414,   414,   415,   415,   415,   415,   416,   416,
     416,   416,   416,   416,   417,   417,   417,   418,   418,   418,
     418,   418,   419,   419,   419,   419,   420,   420,   420,   420,
     420,   420,   421,   421,   422,   422,   422,   422,   423,   423,
     423,   423,   424,   424,   424,   424,   424,   424,   424,   425,
     425,   425,   425,   425,   426,   426,   426,   426,   426,   427,
     427,   427,   428,   428,   428,   428,   429,   429,   429,   430,
     430,   430,   430,   430,   431,   431,   432,   432,   432,   433,
     433,   434,   434,   435,   435,   435,   436,   436,   436,   436,
     436,   437,   437,   437,   437,   438,   438,   438,   439,   439,
     439,   439,   440,   440,   440,   440,   441,   441,   441,   441,
     442,   442,   442,   442,   442,   443,   443,   443,   443,   444,
     444,   444,   445,   445,   445,   446,   446,   446,   446,   446,
     446,   447,   447,   447,   448,   448,   448,   448,   448,   449,
     449,   449,   449,   450,   450,   451,   451,   451,   452,   452,
     453,   453,   453,   453,   453,   453,   454,   454,   454,   454,
     454,   454,   454,   454,   454,   454,   455,   455,   455,   455,
     456,   456,   456,   457,   457,   458,   458,   458,   458,   458,
     458,   459,   459,   459,   459,   459,   459,   460,   460,   460,
     461,   461,   462,   462,   463,   463
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     3,     3,
       5,     6,     2,     2,     1,     3,     3,     3,     1,     6,
       4,     4,     4,     4,     4,     3,     3,     3,     3,     3,
       2,     5,     3,     3,     3,     5,     2,     2,     7,     8,
       5,     0,     1,     1,     3,     1,     1,     1,     3,     1,
       2,     4,     3,     5,     3,     5,     2,     2,     2,     0,
       2,     1,     1,     1,     2,     2,     2,     2,     2,     2,
       4,     2,     4,     6,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     5,     5,     4,     5,     5,     5,
       4,     2,     2,     3,     3,     1,     1,     1,     3,     1,
       3,     3,     3,     1,     3,     3,     1,     3,     3,     1,
       3,     3,     3,     3,     1,     3,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     5,     4,
       1,     1,     3,     6,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     7,     1,     1,     3,     3,     1,     3,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     4,     2,     6,     1,     2,
       1,     2,     1,     2,     1,     1,     2,     2,     2,     5,
       3,     5,    10,     7,     5,    10,     7,     5,     7,     1,
       1,     1,     2,     1,     3,     1,     1,     3,     2,     3,
       3,     2,     2,     1,     2,     2,     0,     1,     2,     3,
       4,     6,     5,     7,     6,     7,     7,     8,     4,     6,
       5,     7,     1,     3,     4,     5,     4,     1,     2,     3,
       5,     2,     3,     4,     5,     7,     3,     5,     5,     7,
       7,     7,     1,     1,     1,     1,     3,     4,     2,     3,
       3,     2,     3,     2,     3,     3,     6,     2,     2,     3,
       3,     3,     3,     3,     3,     5,     1,     1,     5,     5,
       4,     0,     1,     4,     6,     1,     3,     4,     3,     5,
       3,     3,     6,     7,     3,     5,     3,     3,     4,     8,
       9,     0,     2,     1,     1,     1,     1,     2,     1,     2,
       2,     2,     1,     3,     1,     1,     6,     8,    10,    12,
      14,     0,     1,     0,     1,     1,     3,     4,     7,     0,
       1,     3,     1,     3,     0,     1,     1,     2,     0,     1,
       4,     5,     0,     1,     3,     4,     1,     3,     2,     2,
       1,     7,     5,     1,     1,     1,     1,     1,     2,     3,
       6,     3,     3,     4,     1,     2,     2,     3,     8,     8,
       8,     5,     9,     2,     2,     5,     3,     5,     4,     3,
       4,     4,     7,     2,     1,     1,     1,     3,     6,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     2,     3,     1,     2,     1,     1,
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
       4,     4,     3,     2,     2,     3,     3,     2,     1,     0,
       1,     4,     1,     2,     2,     0,     1,     4,     1,     2,
       3,     1,     2,     0,     1,     2,     6,     0,     8,     7,
       9,     0,    12,    11,     1,     3,     3,     2,     2,     4,
       5,     0,     2,     5,     0,     1,     1,     1,     5,     5,
       5,     1,     5,     5,     9,     1,     5,     0,     1,     1,
       5,     1,     1,     5,     5,     1,     3,     3,     4,     1,
       1,     1,     1,     2,     1,     3,     3,     2,     3,     1,
       3,     1,     1,     1,     1,     1,     2,     1,     1,     0,
       2,     2,     4,     1,     4,     0,     1,     2,     3,     4,
       2,     2,     1,     2,     2,     5,     5,     7,     6,     1,
       3,     0,     2,     0,     5,     0,     5,     3,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       2,     5,     6,     1,     1,     3,     3,     2,     3,     3,
       2,     4,     1,     4,     7,    10,     1,     4,     2,     2,
       1,     1,     5,     2,     5,     0,     1,     3,     4,     0,
       1,     0,     0,     1,     1,     1,     2,     5,     0,     6,
       0,     8,     0,     7,     0,     7,     0,     8,     1,     2,
       3,     0,     5,     3,     4,     4,     4,     4,     5,     5,
       5,     5,     6,     1,     1,     1,     3,     0,     5,     0,
       1,     1,     2,     6,     1,     3,     0,     1,     4,     1,
       1,     1,     1,     1,     1,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     8,     9,     3,     4,     2,     1,     2,     6,
       8,     9,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     1,     1,     1,     4,     8,     9,
       3,     4,     2,     3,     3,     4,     4,     5,     4,     5,
       3,     4,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     6,
       8,     9,     3,     4,     2,     4,     1,     2,     2,     2,
       3,     4,     2,     4,     4,     3,     6,     8,     3,     2,
       4,     1,     2,     2,     1,     1,     2,     3,     4,     2,
       4,     6,     8,     1,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     5,     8,     3,     2,     3,
       7,     1,     5,     5,     6,     6,     1,     3,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     5,
       8,     3,     1,     2,     1,     2,     6,     5,     6,     7,
       7,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     8,     3,     1,     1,     2,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     2,     4,     3,
       2,     3,     2,     4,     3,     2,     6,     6,     6,     7,
       1,     2,     1,     1,     1,     2,     3,     2,     3,     2,
       3,     3,     4,     2,     3,     4,     2,     5,     6,     7,
       6,     6,     0,     1,     0,     2
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
#line 546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7074 "Parser/parser.cc"
    break;

  case 3:
#line 550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7080 "Parser/parser.cc"
    break;

  case 4:
#line 557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7086 "Parser/parser.cc"
    break;

  case 5:
#line 558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7092 "Parser/parser.cc"
    break;

  case 6:
#line 559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7098 "Parser/parser.cc"
    break;

  case 7:
#line 560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7104 "Parser/parser.cc"
    break;

  case 8:
#line 561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7110 "Parser/parser.cc"
    break;

  case 19:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7116 "Parser/parser.cc"
    break;

  case 20:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7122 "Parser/parser.cc"
    break;

  case 21:
#line 590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7128 "Parser/parser.cc"
    break;

  case 22:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7138 "Parser/parser.cc"
    break;

  case 23:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7144 "Parser/parser.cc"
    break;

  case 24:
#line 605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7150 "Parser/parser.cc"
    break;

  case 25:
#line 609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7156 "Parser/parser.cc"
    break;

  case 27:
#line 612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7162 "Parser/parser.cc"
    break;

  case 28:
#line 614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7168 "Parser/parser.cc"
    break;

  case 29:
#line 616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7174 "Parser/parser.cc"
    break;

  case 30:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7180 "Parser/parser.cc"
    break;

  case 31:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7190 "Parser/parser.cc"
    break;

  case 32:
#line 630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Adjacent identifiers are not meaningful in an expression. "
											   "Possible problem is identifier \"", *(yyvsp[-1].tok).str,
											   "\" is a misspelled typename or an incorrectly specified type name, "
											   "e.g., missing generic parameter or missing struct/union/enum before typename." ) );
			(yyval.en) = nullptr;
 		}
#line 7202 "Parser/parser.cc"
    break;

  case 33:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7212 "Parser/parser.cc"
    break;

  case 35:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7223 "Parser/parser.cc"
    break;

  case 36:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7232 "Parser/parser.cc"
    break;

  case 37:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7238 "Parser/parser.cc"
    break;

  case 39:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7244 "Parser/parser.cc"
    break;

  case 40:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7250 "Parser/parser.cc"
    break;

  case 41:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7256 "Parser/parser.cc"
    break;

  case 42:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7262 "Parser/parser.cc"
    break;

  case 43:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7272 "Parser/parser.cc"
    break;

  case 44:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7278 "Parser/parser.cc"
    break;

  case 45:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7284 "Parser/parser.cc"
    break;

  case 46:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7290 "Parser/parser.cc"
    break;

  case 47:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7296 "Parser/parser.cc"
    break;

  case 48:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7302 "Parser/parser.cc"
    break;

  case 49:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 50:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7314 "Parser/parser.cc"
    break;

  case 51:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 52:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7326 "Parser/parser.cc"
    break;

  case 53:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7332 "Parser/parser.cc"
    break;

  case 54:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7338 "Parser/parser.cc"
    break;

  case 55:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7344 "Parser/parser.cc"
    break;

  case 56:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7350 "Parser/parser.cc"
    break;

  case 57:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7356 "Parser/parser.cc"
    break;

  case 58:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7362 "Parser/parser.cc"
    break;

  case 59:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7368 "Parser/parser.cc"
    break;

  case 60:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7378 "Parser/parser.cc"
    break;

  case 61:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7384 "Parser/parser.cc"
    break;

  case 64:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7390 "Parser/parser.cc"
    break;

  case 65:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7396 "Parser/parser.cc"
    break;

  case 68:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7402 "Parser/parser.cc"
    break;

  case 70:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7408 "Parser/parser.cc"
    break;

  case 71:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7414 "Parser/parser.cc"
    break;

  case 72:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7420 "Parser/parser.cc"
    break;

  case 73:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7426 "Parser/parser.cc"
    break;

  case 74:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7432 "Parser/parser.cc"
    break;

  case 75:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7438 "Parser/parser.cc"
    break;

  case 76:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7444 "Parser/parser.cc"
    break;

  case 77:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7450 "Parser/parser.cc"
    break;

  case 78:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7458 "Parser/parser.cc"
    break;

  case 79:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7464 "Parser/parser.cc"
    break;

  case 80:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7473 "Parser/parser.cc"
    break;

  case 83:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7479 "Parser/parser.cc"
    break;

  case 84:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7485 "Parser/parser.cc"
    break;

  case 85:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			switch ( (yyvsp[-1].op) ) {
			  case OperKinds::AddressOf:
				(yyval.en) = new ExpressionNode( new AddressExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) );
				break;
			  case OperKinds::PointTo:
				(yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) );
				break;
			  case OperKinds::And:
				(yyval.en) = new ExpressionNode( new AddressExpr( new AddressExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ) );
				break;
			  default:
				assert( false );
			}
		}
#line 7505 "Parser/parser.cc"
    break;

  case 86:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7511 "Parser/parser.cc"
    break;

  case 87:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7517 "Parser/parser.cc"
    break;

  case 88:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7523 "Parser/parser.cc"
    break;

  case 89:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7529 "Parser/parser.cc"
    break;

  case 90:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7535 "Parser/parser.cc"
    break;

  case 91:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7541 "Parser/parser.cc"
    break;

  case 92:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7547 "Parser/parser.cc"
    break;

  case 93:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7553 "Parser/parser.cc"
    break;

  case 94:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7562 "Parser/parser.cc"
    break;

  case 95:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7568 "Parser/parser.cc"
    break;

  case 96:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7574 "Parser/parser.cc"
    break;

  case 97:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7580 "Parser/parser.cc"
    break;

  case 98:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7586 "Parser/parser.cc"
    break;

  case 99:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7592 "Parser/parser.cc"
    break;

  case 100:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7598 "Parser/parser.cc"
    break;

  case 101:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7604 "Parser/parser.cc"
    break;

  case 103:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7610 "Parser/parser.cc"
    break;

  case 104:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7616 "Parser/parser.cc"
    break;

  case 105:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7622 "Parser/parser.cc"
    break;

  case 106:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7628 "Parser/parser.cc"
    break;

  case 107:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7634 "Parser/parser.cc"
    break;

  case 108:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7640 "Parser/parser.cc"
    break;

  case 109:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7646 "Parser/parser.cc"
    break;

  case 110:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7652 "Parser/parser.cc"
    break;

  case 118:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7658 "Parser/parser.cc"
    break;

  case 120:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7664 "Parser/parser.cc"
    break;

  case 121:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7670 "Parser/parser.cc"
    break;

  case 122:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7676 "Parser/parser.cc"
    break;

  case 124:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7682 "Parser/parser.cc"
    break;

  case 125:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7688 "Parser/parser.cc"
    break;

  case 127:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7694 "Parser/parser.cc"
    break;

  case 128:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7700 "Parser/parser.cc"
    break;

  case 130:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7706 "Parser/parser.cc"
    break;

  case 131:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7712 "Parser/parser.cc"
    break;

  case 132:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 133:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7724 "Parser/parser.cc"
    break;

  case 135:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7730 "Parser/parser.cc"
    break;

  case 136:
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7736 "Parser/parser.cc"
    break;

  case 138:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7742 "Parser/parser.cc"
    break;

  case 140:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7748 "Parser/parser.cc"
    break;

  case 142:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7754 "Parser/parser.cc"
    break;

  case 144:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7760 "Parser/parser.cc"
    break;

  case 146:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7766 "Parser/parser.cc"
    break;

  case 148:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7772 "Parser/parser.cc"
    break;

  case 149:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7778 "Parser/parser.cc"
    break;

  case 152:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7790 "Parser/parser.cc"
    break;

  case 153:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7796 "Parser/parser.cc"
    break;

  case 154:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7802 "Parser/parser.cc"
    break;

  case 158:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7808 "Parser/parser.cc"
    break;

  case 159:
#line 1012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7814 "Parser/parser.cc"
    break;

  case 160:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7820 "Parser/parser.cc"
    break;

  case 161:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7826 "Parser/parser.cc"
    break;

  case 162:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7832 "Parser/parser.cc"
    break;

  case 163:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7838 "Parser/parser.cc"
    break;

  case 164:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7844 "Parser/parser.cc"
    break;

  case 165:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7850 "Parser/parser.cc"
    break;

  case 166:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7856 "Parser/parser.cc"
    break;

  case 167:
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7862 "Parser/parser.cc"
    break;

  case 168:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7868 "Parser/parser.cc"
    break;

  case 169:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7874 "Parser/parser.cc"
    break;

  case 170:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7880 "Parser/parser.cc"
    break;

  case 171:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7886 "Parser/parser.cc"
    break;

  case 172:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7892 "Parser/parser.cc"
    break;

  case 174:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7898 "Parser/parser.cc"
    break;

  case 175:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7904 "Parser/parser.cc"
    break;

  case 176:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7910 "Parser/parser.cc"
    break;

  case 178:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7916 "Parser/parser.cc"
    break;

  case 179:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7922 "Parser/parser.cc"
    break;

  case 191:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7928 "Parser/parser.cc"
    break;

  case 193:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7934 "Parser/parser.cc"
    break;

  case 194:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7940 "Parser/parser.cc"
    break;

  case 195:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 7951 "Parser/parser.cc"
    break;

  case 196:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7957 "Parser/parser.cc"
    break;

  case 197:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 199:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7969 "Parser/parser.cc"
    break;

  case 200:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 201:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 202:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 203:
#line 1121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 206:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7999 "Parser/parser.cc"
    break;

  case 207:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8005 "Parser/parser.cc"
    break;

  case 208:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8011 "Parser/parser.cc"
    break;

  case 209:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8017 "Parser/parser.cc"
    break;

  case 210:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8023 "Parser/parser.cc"
    break;

  case 211:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8029 "Parser/parser.cc"
    break;

  case 212:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8043 "Parser/parser.cc"
    break;

  case 213:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8049 "Parser/parser.cc"
    break;

  case 214:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8055 "Parser/parser.cc"
    break;

  case 215:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8064 "Parser/parser.cc"
    break;

  case 216:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8070 "Parser/parser.cc"
    break;

  case 217:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8076 "Parser/parser.cc"
    break;

  case 218:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8082 "Parser/parser.cc"
    break;

  case 219:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8088 "Parser/parser.cc"
    break;

  case 220:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8094 "Parser/parser.cc"
    break;

  case 221:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8100 "Parser/parser.cc"
    break;

  case 222:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8106 "Parser/parser.cc"
    break;

  case 223:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8112 "Parser/parser.cc"
    break;

  case 224:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8118 "Parser/parser.cc"
    break;

  case 226:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8124 "Parser/parser.cc"
    break;

  case 227:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8130 "Parser/parser.cc"
    break;

  case 228:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8136 "Parser/parser.cc"
    break;

  case 229:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8142 "Parser/parser.cc"
    break;

  case 230:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8148 "Parser/parser.cc"
    break;

  case 231:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8154 "Parser/parser.cc"
    break;

  case 232:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8160 "Parser/parser.cc"
    break;

  case 234:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8166 "Parser/parser.cc"
    break;

  case 235:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 236:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8178 "Parser/parser.cc"
    break;

  case 238:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8184 "Parser/parser.cc"
    break;

  case 239:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8190 "Parser/parser.cc"
    break;

  case 240:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8196 "Parser/parser.cc"
    break;

  case 241:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8205 "Parser/parser.cc"
    break;

  case 242:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8211 "Parser/parser.cc"
    break;

  case 243:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8217 "Parser/parser.cc"
    break;

  case 244:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8223 "Parser/parser.cc"
    break;

  case 245:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8232 "Parser/parser.cc"
    break;

  case 246:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8238 "Parser/parser.cc"
    break;

  case 247:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8244 "Parser/parser.cc"
    break;

  case 248:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8250 "Parser/parser.cc"
    break;

  case 249:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8259 "Parser/parser.cc"
    break;

  case 250:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8265 "Parser/parser.cc"
    break;

  case 251:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8271 "Parser/parser.cc"
    break;

  case 253:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8290 "Parser/parser.cc"
    break;

  case 254:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8296 "Parser/parser.cc"
    break;

  case 255:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8302 "Parser/parser.cc"
    break;

  case 256:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8308 "Parser/parser.cc"
    break;

  case 257:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8315 "Parser/parser.cc"
    break;

  case 258:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8322 "Parser/parser.cc"
    break;

  case 259:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8328 "Parser/parser.cc"
    break;

  case 260:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8334 "Parser/parser.cc"
    break;

  case 261:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8340 "Parser/parser.cc"
    break;

  case 262:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8347 "Parser/parser.cc"
    break;

  case 263:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 264:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 265:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 266:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8375 "Parser/parser.cc"
    break;

  case 267:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8381 "Parser/parser.cc"
    break;

  case 268:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8387 "Parser/parser.cc"
    break;

  case 269:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8393 "Parser/parser.cc"
    break;

  case 270:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8399 "Parser/parser.cc"
    break;

  case 271:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8405 "Parser/parser.cc"
    break;

  case 272:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8411 "Parser/parser.cc"
    break;

  case 273:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8417 "Parser/parser.cc"
    break;

  case 274:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8423 "Parser/parser.cc"
    break;

  case 275:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8429 "Parser/parser.cc"
    break;

  case 276:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8435 "Parser/parser.cc"
    break;

  case 277:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8441 "Parser/parser.cc"
    break;

  case 278:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8447 "Parser/parser.cc"
    break;

  case 279:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8453 "Parser/parser.cc"
    break;

  case 280:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8459 "Parser/parser.cc"
    break;

  case 281:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8465 "Parser/parser.cc"
    break;

  case 282:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8471 "Parser/parser.cc"
    break;

  case 283:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8477 "Parser/parser.cc"
    break;

  case 284:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8483 "Parser/parser.cc"
    break;

  case 285:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8489 "Parser/parser.cc"
    break;

  case 286:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8495 "Parser/parser.cc"
    break;

  case 287:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8501 "Parser/parser.cc"
    break;

  case 288:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8507 "Parser/parser.cc"
    break;

  case 289:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8513 "Parser/parser.cc"
    break;

  case 290:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8519 "Parser/parser.cc"
    break;

  case 291:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 292:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8531 "Parser/parser.cc"
    break;

  case 293:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8537 "Parser/parser.cc"
    break;

  case 294:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8543 "Parser/parser.cc"
    break;

  case 295:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 298:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8555 "Parser/parser.cc"
    break;

  case 299:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8561 "Parser/parser.cc"
    break;

  case 300:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8567 "Parser/parser.cc"
    break;

  case 301:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8573 "Parser/parser.cc"
    break;

  case 303:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8579 "Parser/parser.cc"
    break;

  case 304:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8585 "Parser/parser.cc"
    break;

  case 306:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8591 "Parser/parser.cc"
    break;

  case 307:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8597 "Parser/parser.cc"
    break;

  case 308:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8603 "Parser/parser.cc"
    break;

  case 309:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8609 "Parser/parser.cc"
    break;

  case 310:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8615 "Parser/parser.cc"
    break;

  case 311:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8621 "Parser/parser.cc"
    break;

  case 312:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8627 "Parser/parser.cc"
    break;

  case 313:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8633 "Parser/parser.cc"
    break;

  case 314:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8639 "Parser/parser.cc"
    break;

  case 315:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8645 "Parser/parser.cc"
    break;

  case 316:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8651 "Parser/parser.cc"
    break;

  case 317:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8657 "Parser/parser.cc"
    break;

  case 318:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8663 "Parser/parser.cc"
    break;

  case 319:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8669 "Parser/parser.cc"
    break;

  case 320:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8675 "Parser/parser.cc"
    break;

  case 321:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8681 "Parser/parser.cc"
    break;

  case 322:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8687 "Parser/parser.cc"
    break;

  case 323:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8693 "Parser/parser.cc"
    break;

  case 324:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8699 "Parser/parser.cc"
    break;

  case 325:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8705 "Parser/parser.cc"
    break;

  case 326:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8711 "Parser/parser.cc"
    break;

  case 327:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 329:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8723 "Parser/parser.cc"
    break;

  case 330:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8729 "Parser/parser.cc"
    break;

  case 331:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8735 "Parser/parser.cc"
    break;

  case 336:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8741 "Parser/parser.cc"
    break;

  case 337:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8747 "Parser/parser.cc"
    break;

  case 338:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8753 "Parser/parser.cc"
    break;

  case 339:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8759 "Parser/parser.cc"
    break;

  case 340:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8765 "Parser/parser.cc"
    break;

  case 341:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8771 "Parser/parser.cc"
    break;

  case 342:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8777 "Parser/parser.cc"
    break;

  case 343:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8783 "Parser/parser.cc"
    break;

  case 346:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8789 "Parser/parser.cc"
    break;

  case 347:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8795 "Parser/parser.cc"
    break;

  case 348:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8801 "Parser/parser.cc"
    break;

  case 349:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8807 "Parser/parser.cc"
    break;

  case 350:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8813 "Parser/parser.cc"
    break;

  case 351:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8819 "Parser/parser.cc"
    break;

  case 352:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8828 "Parser/parser.cc"
    break;

  case 353:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8837 "Parser/parser.cc"
    break;

  case 354:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8843 "Parser/parser.cc"
    break;

  case 357:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8849 "Parser/parser.cc"
    break;

  case 358:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8855 "Parser/parser.cc"
    break;

  case 360:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8861 "Parser/parser.cc"
    break;

  case 361:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8867 "Parser/parser.cc"
    break;

  case 368:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 8878 "Parser/parser.cc"
    break;

  case 371:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8884 "Parser/parser.cc"
    break;

  case 372:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8890 "Parser/parser.cc"
    break;

  case 376:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8896 "Parser/parser.cc"
    break;

  case 378:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8902 "Parser/parser.cc"
    break;

  case 379:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8908 "Parser/parser.cc"
    break;

  case 380:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8914 "Parser/parser.cc"
    break;

  case 381:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8920 "Parser/parser.cc"
    break;

  case 382:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8926 "Parser/parser.cc"
    break;

  case 383:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8932 "Parser/parser.cc"
    break;

  case 385:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 386:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8944 "Parser/parser.cc"
    break;

  case 387:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8950 "Parser/parser.cc"
    break;

  case 388:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8961 "Parser/parser.cc"
    break;

  case 389:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8967 "Parser/parser.cc"
    break;

  case 390:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8973 "Parser/parser.cc"
    break;

  case 391:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8979 "Parser/parser.cc"
    break;

  case 392:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8985 "Parser/parser.cc"
    break;

  case 393:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8994 "Parser/parser.cc"
    break;

  case 394:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9003 "Parser/parser.cc"
    break;

  case 395:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9012 "Parser/parser.cc"
    break;

  case 396:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9021 "Parser/parser.cc"
    break;

  case 397:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9030 "Parser/parser.cc"
    break;

  case 398:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9039 "Parser/parser.cc"
    break;

  case 399:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9048 "Parser/parser.cc"
    break;

  case 400:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9057 "Parser/parser.cc"
    break;

  case 401:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9065 "Parser/parser.cc"
    break;

  case 402:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9073 "Parser/parser.cc"
    break;

  case 403:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9079 "Parser/parser.cc"
    break;

  case 407:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9085 "Parser/parser.cc"
    break;

  case 408:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9091 "Parser/parser.cc"
    break;

  case 416:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9102 "Parser/parser.cc"
    break;

  case 421:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9108 "Parser/parser.cc"
    break;

  case 424:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9114 "Parser/parser.cc"
    break;

  case 427:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9120 "Parser/parser.cc"
    break;

  case 428:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9126 "Parser/parser.cc"
    break;

  case 429:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9132 "Parser/parser.cc"
    break;

  case 430:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9138 "Parser/parser.cc"
    break;

  case 432:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9144 "Parser/parser.cc"
    break;

  case 434:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9150 "Parser/parser.cc"
    break;

  case 435:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 437:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 438:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9168 "Parser/parser.cc"
    break;

  case 439:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9174 "Parser/parser.cc"
    break;

  case 440:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9180 "Parser/parser.cc"
    break;

  case 441:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9186 "Parser/parser.cc"
    break;

  case 442:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9192 "Parser/parser.cc"
    break;

  case 443:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9198 "Parser/parser.cc"
    break;

  case 444:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9204 "Parser/parser.cc"
    break;

  case 445:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9210 "Parser/parser.cc"
    break;

  case 446:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9216 "Parser/parser.cc"
    break;

  case 447:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9222 "Parser/parser.cc"
    break;

  case 448:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9228 "Parser/parser.cc"
    break;

  case 449:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9234 "Parser/parser.cc"
    break;

  case 450:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9240 "Parser/parser.cc"
    break;

  case 451:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9246 "Parser/parser.cc"
    break;

  case 452:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9252 "Parser/parser.cc"
    break;

  case 453:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9258 "Parser/parser.cc"
    break;

  case 454:
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9264 "Parser/parser.cc"
    break;

  case 455:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9270 "Parser/parser.cc"
    break;

  case 456:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9276 "Parser/parser.cc"
    break;

  case 457:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9282 "Parser/parser.cc"
    break;

  case 458:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9288 "Parser/parser.cc"
    break;

  case 459:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9294 "Parser/parser.cc"
    break;

  case 460:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9300 "Parser/parser.cc"
    break;

  case 461:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9306 "Parser/parser.cc"
    break;

  case 462:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9312 "Parser/parser.cc"
    break;

  case 463:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9318 "Parser/parser.cc"
    break;

  case 464:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9324 "Parser/parser.cc"
    break;

  case 465:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9330 "Parser/parser.cc"
    break;

  case 466:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9336 "Parser/parser.cc"
    break;

  case 467:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9342 "Parser/parser.cc"
    break;

  case 468:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9348 "Parser/parser.cc"
    break;

  case 469:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9354 "Parser/parser.cc"
    break;

  case 470:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9360 "Parser/parser.cc"
    break;

  case 471:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9366 "Parser/parser.cc"
    break;

  case 472:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9372 "Parser/parser.cc"
    break;

  case 474:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9378 "Parser/parser.cc"
    break;

  case 476:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9384 "Parser/parser.cc"
    break;

  case 477:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9390 "Parser/parser.cc"
    break;

  case 478:
#line 2022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9396 "Parser/parser.cc"
    break;

  case 480:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 481:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9408 "Parser/parser.cc"
    break;

  case 482:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9414 "Parser/parser.cc"
    break;

  case 483:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9420 "Parser/parser.cc"
    break;

  case 485:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 487:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 488:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9438 "Parser/parser.cc"
    break;

  case 489:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9444 "Parser/parser.cc"
    break;

  case 490:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9450 "Parser/parser.cc"
    break;

  case 491:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9456 "Parser/parser.cc"
    break;

  case 492:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9462 "Parser/parser.cc"
    break;

  case 493:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9468 "Parser/parser.cc"
    break;

  case 494:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9474 "Parser/parser.cc"
    break;

  case 495:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9480 "Parser/parser.cc"
    break;

  case 496:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9491 "Parser/parser.cc"
    break;

  case 497:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9497 "Parser/parser.cc"
    break;

  case 498:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9503 "Parser/parser.cc"
    break;

  case 499:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 500:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9520 "Parser/parser.cc"
    break;

  case 501:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9526 "Parser/parser.cc"
    break;

  case 502:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9532 "Parser/parser.cc"
    break;

  case 503:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9541 "Parser/parser.cc"
    break;

  case 505:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 506:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9553 "Parser/parser.cc"
    break;

  case 507:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9559 "Parser/parser.cc"
    break;

  case 509:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9565 "Parser/parser.cc"
    break;

  case 510:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9571 "Parser/parser.cc"
    break;

  case 512:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9577 "Parser/parser.cc"
    break;

  case 513:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9583 "Parser/parser.cc"
    break;

  case 514:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9589 "Parser/parser.cc"
    break;

  case 516:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9595 "Parser/parser.cc"
    break;

  case 517:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 518:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 519:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 520:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9619 "Parser/parser.cc"
    break;

  case 522:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 523:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 524:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9637 "Parser/parser.cc"
    break;

  case 525:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9643 "Parser/parser.cc"
    break;

  case 526:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9649 "Parser/parser.cc"
    break;

  case 527:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9660 "Parser/parser.cc"
    break;

  case 531:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9666 "Parser/parser.cc"
    break;

  case 532:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9672 "Parser/parser.cc"
    break;

  case 533:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9681 "Parser/parser.cc"
    break;

  case 534:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "aggregate_type1 %s\n", $3.str->c_str() );
			// if ( $2 )
			// 	for ( Attribute * attr: reverseIterate( $2->attributes ) ) {
			// 		printf( "copySpecifiers12 %s\n", attr->name.c_str() );
			// 	} // for
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
			// printf( "aggregate_type2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			// 	printf( "aggregate_type3 %s\n", attr->name.c_str() );
			// } // for
		}
#line 9698 "Parser/parser.cc"
    break;

  case 535:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9707 "Parser/parser.cc"
    break;

  case 536:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9717 "Parser/parser.cc"
    break;

  case 537:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9726 "Parser/parser.cc"
    break;

  case 538:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9736 "Parser/parser.cc"
    break;

  case 540:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9742 "Parser/parser.cc"
    break;

  case 541:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9748 "Parser/parser.cc"
    break;

  case 542:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9758 "Parser/parser.cc"
    break;

  case 543:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].decl)->type->symbolic.name, (yyvsp[0].decl)->type->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
			(yyvsp[0].decl)->type->symbolic.name = nullptr;
			(yyvsp[0].decl)->type->symbolic.actuals = nullptr;
			delete (yyvsp[0].decl);
		}
#line 9773 "Parser/parser.cc"
    break;

  case 546:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9779 "Parser/parser.cc"
    break;

  case 547:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9785 "Parser/parser.cc"
    break;

  case 548:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9791 "Parser/parser.cc"
    break;

  case 549:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9797 "Parser/parser.cc"
    break;

  case 550:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9803 "Parser/parser.cc"
    break;

  case 551:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9809 "Parser/parser.cc"
    break;

  case 552:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9815 "Parser/parser.cc"
    break;

  case 553:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9821 "Parser/parser.cc"
    break;

  case 554:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9827 "Parser/parser.cc"
    break;

  case 555:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9833 "Parser/parser.cc"
    break;

  case 556:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9839 "Parser/parser.cc"
    break;

  case 557:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9845 "Parser/parser.cc"
    break;

  case 558:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9851 "Parser/parser.cc"
    break;

  case 559:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9864 "Parser/parser.cc"
    break;

  case 560:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9870 "Parser/parser.cc"
    break;

  case 561:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9883 "Parser/parser.cc"
    break;

  case 562:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9889 "Parser/parser.cc"
    break;

  case 565:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9895 "Parser/parser.cc"
    break;

  case 566:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9901 "Parser/parser.cc"
    break;

  case 569:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9907 "Parser/parser.cc"
    break;

  case 571:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 572:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9919 "Parser/parser.cc"
    break;

  case 573:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9925 "Parser/parser.cc"
    break;

  case 574:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9931 "Parser/parser.cc"
    break;

  case 575:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9937 "Parser/parser.cc"
    break;

  case 577:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9943 "Parser/parser.cc"
    break;

  case 579:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9949 "Parser/parser.cc"
    break;

  case 580:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9955 "Parser/parser.cc"
    break;

  case 582:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 583:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9967 "Parser/parser.cc"
    break;

  case 585:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9973 "Parser/parser.cc"
    break;

  case 586:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9979 "Parser/parser.cc"
    break;

  case 587:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9985 "Parser/parser.cc"
    break;

  case 588:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9991 "Parser/parser.cc"
    break;

  case 589:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9997 "Parser/parser.cc"
    break;

  case 590:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10008 "Parser/parser.cc"
    break;

  case 591:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10017 "Parser/parser.cc"
    break;

  case 592:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10025 "Parser/parser.cc"
    break;

  case 593:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10035 "Parser/parser.cc"
    break;

  case 595:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10041 "Parser/parser.cc"
    break;

  case 596:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10047 "Parser/parser.cc"
    break;

  case 597:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10053 "Parser/parser.cc"
    break;

  case 598:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10059 "Parser/parser.cc"
    break;

  case 599:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10065 "Parser/parser.cc"
    break;

  case 600:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10071 "Parser/parser.cc"
    break;

  case 601:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10077 "Parser/parser.cc"
    break;

  case 602:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10083 "Parser/parser.cc"
    break;

  case 603:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10089 "Parser/parser.cc"
    break;

  case 604:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10095 "Parser/parser.cc"
    break;

  case 605:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10101 "Parser/parser.cc"
    break;

  case 608:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 609:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10113 "Parser/parser.cc"
    break;

  case 610:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10119 "Parser/parser.cc"
    break;

  case 612:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10125 "Parser/parser.cc"
    break;

  case 613:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10131 "Parser/parser.cc"
    break;

  case 614:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 616:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 617:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10149 "Parser/parser.cc"
    break;

  case 618:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10155 "Parser/parser.cc"
    break;

  case 620:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10161 "Parser/parser.cc"
    break;

  case 623:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 624:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 626:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 627:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 628:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 633:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 635:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10203 "Parser/parser.cc"
    break;

  case 636:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10209 "Parser/parser.cc"
    break;

  case 637:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10215 "Parser/parser.cc"
    break;

  case 638:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10221 "Parser/parser.cc"
    break;

  case 639:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 640:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10233 "Parser/parser.cc"
    break;

  case 646:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 649:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10245 "Parser/parser.cc"
    break;

  case 650:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10251 "Parser/parser.cc"
    break;

  case 651:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10257 "Parser/parser.cc"
    break;

  case 652:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10263 "Parser/parser.cc"
    break;

  case 653:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10269 "Parser/parser.cc"
    break;

  case 654:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10275 "Parser/parser.cc"
    break;

  case 655:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10281 "Parser/parser.cc"
    break;

  case 657:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10287 "Parser/parser.cc"
    break;

  case 658:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10293 "Parser/parser.cc"
    break;

  case 659:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10299 "Parser/parser.cc"
    break;

  case 661:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10305 "Parser/parser.cc"
    break;

  case 663:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10311 "Parser/parser.cc"
    break;

  case 664:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10317 "Parser/parser.cc"
    break;

  case 665:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10323 "Parser/parser.cc"
    break;

  case 666:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10329 "Parser/parser.cc"
    break;

  case 667:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10335 "Parser/parser.cc"
    break;

  case 668:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10341 "Parser/parser.cc"
    break;

  case 670:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10347 "Parser/parser.cc"
    break;

  case 671:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10353 "Parser/parser.cc"
    break;

  case 672:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10359 "Parser/parser.cc"
    break;

  case 673:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10370 "Parser/parser.cc"
    break;

  case 674:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10376 "Parser/parser.cc"
    break;

  case 675:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10382 "Parser/parser.cc"
    break;

  case 676:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10388 "Parser/parser.cc"
    break;

  case 677:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10397 "Parser/parser.cc"
    break;

  case 678:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 679:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10409 "Parser/parser.cc"
    break;

  case 680:
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10415 "Parser/parser.cc"
    break;

  case 681:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10421 "Parser/parser.cc"
    break;

  case 682:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10427 "Parser/parser.cc"
    break;

  case 683:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10433 "Parser/parser.cc"
    break;

  case 684:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10439 "Parser/parser.cc"
    break;

  case 685:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10445 "Parser/parser.cc"
    break;

  case 686:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10451 "Parser/parser.cc"
    break;

  case 687:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10457 "Parser/parser.cc"
    break;

  case 690:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 691:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 692:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10475 "Parser/parser.cc"
    break;

  case 693:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 695:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10487 "Parser/parser.cc"
    break;

  case 696:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10493 "Parser/parser.cc"
    break;

  case 697:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10499 "Parser/parser.cc"
    break;

  case 698:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10505 "Parser/parser.cc"
    break;

  case 699:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 700:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 701:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10523 "Parser/parser.cc"
    break;

  case 702:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10532 "Parser/parser.cc"
    break;

  case 703:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10541 "Parser/parser.cc"
    break;

  case 704:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10547 "Parser/parser.cc"
    break;

  case 705:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 707:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 712:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10565 "Parser/parser.cc"
    break;

  case 713:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10571 "Parser/parser.cc"
    break;

  case 714:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10577 "Parser/parser.cc"
    break;

  case 716:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10583 "Parser/parser.cc"
    break;

  case 717:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10589 "Parser/parser.cc"
    break;

  case 718:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10595 "Parser/parser.cc"
    break;

  case 719:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10601 "Parser/parser.cc"
    break;

  case 721:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10607 "Parser/parser.cc"
    break;

  case 722:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10613 "Parser/parser.cc"
    break;

  case 723:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 726:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10628 "Parser/parser.cc"
    break;

  case 727:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10634 "Parser/parser.cc"
    break;

  case 728:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10643 "Parser/parser.cc"
    break;

  case 729:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10653 "Parser/parser.cc"
    break;

  case 730:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10662 "Parser/parser.cc"
    break;

  case 731:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10672 "Parser/parser.cc"
    break;

  case 732:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10681 "Parser/parser.cc"
    break;

  case 733:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10691 "Parser/parser.cc"
    break;

  case 734:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10700 "Parser/parser.cc"
    break;

  case 735:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10710 "Parser/parser.cc"
    break;

  case 736:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10719 "Parser/parser.cc"
    break;

  case 737:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10729 "Parser/parser.cc"
    break;

  case 739:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 740:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 741:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10747 "Parser/parser.cc"
    break;

  case 742:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 10759 "Parser/parser.cc"
    break;

  case 743:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10770 "Parser/parser.cc"
    break;

  case 744:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10779 "Parser/parser.cc"
    break;

  case 745:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10788 "Parser/parser.cc"
    break;

  case 746:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 747:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 748:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 749:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10815 "Parser/parser.cc"
    break;

  case 750:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10821 "Parser/parser.cc"
    break;

  case 751:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10827 "Parser/parser.cc"
    break;

  case 752:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10833 "Parser/parser.cc"
    break;

  case 756:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10839 "Parser/parser.cc"
    break;

  case 757:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10845 "Parser/parser.cc"
    break;

  case 758:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10855 "Parser/parser.cc"
    break;

  case 759:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10861 "Parser/parser.cc"
    break;

  case 762:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10867 "Parser/parser.cc"
    break;

  case 763:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10873 "Parser/parser.cc"
    break;

  case 765:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 766:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10885 "Parser/parser.cc"
    break;

  case 767:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10891 "Parser/parser.cc"
    break;

  case 768:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10897 "Parser/parser.cc"
    break;

  case 773:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10903 "Parser/parser.cc"
    break;

  case 774:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10909 "Parser/parser.cc"
    break;

  case 775:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10915 "Parser/parser.cc"
    break;

  case 776:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10921 "Parser/parser.cc"
    break;

  case 777:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10927 "Parser/parser.cc"
    break;

  case 779:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10933 "Parser/parser.cc"
    break;

  case 780:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10939 "Parser/parser.cc"
    break;

  case 781:
#line 3083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10945 "Parser/parser.cc"
    break;

  case 782:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10951 "Parser/parser.cc"
    break;

  case 783:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10957 "Parser/parser.cc"
    break;

  case 784:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10963 "Parser/parser.cc"
    break;

  case 785:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 786:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10975 "Parser/parser.cc"
    break;

  case 787:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 788:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 789:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10993 "Parser/parser.cc"
    break;

  case 790:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10999 "Parser/parser.cc"
    break;

  case 791:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11005 "Parser/parser.cc"
    break;

  case 792:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11011 "Parser/parser.cc"
    break;

  case 793:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11017 "Parser/parser.cc"
    break;

  case 794:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11023 "Parser/parser.cc"
    break;

  case 795:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11029 "Parser/parser.cc"
    break;

  case 796:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11035 "Parser/parser.cc"
    break;

  case 798:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11041 "Parser/parser.cc"
    break;

  case 799:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11047 "Parser/parser.cc"
    break;

  case 800:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11053 "Parser/parser.cc"
    break;

  case 801:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 802:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11065 "Parser/parser.cc"
    break;

  case 803:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11071 "Parser/parser.cc"
    break;

  case 804:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11077 "Parser/parser.cc"
    break;

  case 805:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11083 "Parser/parser.cc"
    break;

  case 806:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11089 "Parser/parser.cc"
    break;

  case 807:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11095 "Parser/parser.cc"
    break;

  case 808:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 809:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 810:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 811:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 812:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11125 "Parser/parser.cc"
    break;

  case 813:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 817:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11137 "Parser/parser.cc"
    break;

  case 818:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11143 "Parser/parser.cc"
    break;

  case 819:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 820:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11155 "Parser/parser.cc"
    break;

  case 821:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 822:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 823:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11173 "Parser/parser.cc"
    break;

  case 824:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11179 "Parser/parser.cc"
    break;

  case 825:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 826:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11191 "Parser/parser.cc"
    break;

  case 827:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 828:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 829:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 830:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11215 "Parser/parser.cc"
    break;

  case 831:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11221 "Parser/parser.cc"
    break;

  case 832:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11230 "Parser/parser.cc"
    break;

  case 833:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11236 "Parser/parser.cc"
    break;

  case 834:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11242 "Parser/parser.cc"
    break;

  case 836:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11248 "Parser/parser.cc"
    break;

  case 837:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11254 "Parser/parser.cc"
    break;

  case 838:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11260 "Parser/parser.cc"
    break;

  case 839:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 840:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11272 "Parser/parser.cc"
    break;

  case 841:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11278 "Parser/parser.cc"
    break;

  case 842:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11284 "Parser/parser.cc"
    break;

  case 843:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11290 "Parser/parser.cc"
    break;

  case 844:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11296 "Parser/parser.cc"
    break;

  case 845:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11302 "Parser/parser.cc"
    break;

  case 846:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11308 "Parser/parser.cc"
    break;

  case 847:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11314 "Parser/parser.cc"
    break;

  case 848:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11320 "Parser/parser.cc"
    break;

  case 849:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11326 "Parser/parser.cc"
    break;

  case 850:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11332 "Parser/parser.cc"
    break;

  case 851:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11338 "Parser/parser.cc"
    break;

  case 852:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11344 "Parser/parser.cc"
    break;

  case 853:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11350 "Parser/parser.cc"
    break;

  case 854:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11356 "Parser/parser.cc"
    break;

  case 855:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11362 "Parser/parser.cc"
    break;

  case 857:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11368 "Parser/parser.cc"
    break;

  case 858:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11374 "Parser/parser.cc"
    break;

  case 859:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11380 "Parser/parser.cc"
    break;

  case 860:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11386 "Parser/parser.cc"
    break;

  case 861:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11392 "Parser/parser.cc"
    break;

  case 862:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11398 "Parser/parser.cc"
    break;

  case 863:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11404 "Parser/parser.cc"
    break;

  case 864:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11410 "Parser/parser.cc"
    break;

  case 865:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11416 "Parser/parser.cc"
    break;

  case 866:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11422 "Parser/parser.cc"
    break;

  case 867:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11428 "Parser/parser.cc"
    break;

  case 868:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11434 "Parser/parser.cc"
    break;

  case 869:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11440 "Parser/parser.cc"
    break;

  case 870:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11446 "Parser/parser.cc"
    break;

  case 872:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11452 "Parser/parser.cc"
    break;

  case 873:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11458 "Parser/parser.cc"
    break;

  case 874:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11464 "Parser/parser.cc"
    break;

  case 875:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11470 "Parser/parser.cc"
    break;

  case 876:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11476 "Parser/parser.cc"
    break;

  case 877:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11482 "Parser/parser.cc"
    break;

  case 878:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11488 "Parser/parser.cc"
    break;

  case 879:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11494 "Parser/parser.cc"
    break;

  case 880:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11500 "Parser/parser.cc"
    break;

  case 881:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11506 "Parser/parser.cc"
    break;

  case 882:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11512 "Parser/parser.cc"
    break;

  case 884:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11518 "Parser/parser.cc"
    break;

  case 885:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11524 "Parser/parser.cc"
    break;

  case 886:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11530 "Parser/parser.cc"
    break;

  case 887:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11536 "Parser/parser.cc"
    break;

  case 888:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11542 "Parser/parser.cc"
    break;

  case 889:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11548 "Parser/parser.cc"
    break;

  case 890:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11554 "Parser/parser.cc"
    break;

  case 892:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11560 "Parser/parser.cc"
    break;

  case 893:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11566 "Parser/parser.cc"
    break;

  case 894:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11572 "Parser/parser.cc"
    break;

  case 895:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11578 "Parser/parser.cc"
    break;

  case 896:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11584 "Parser/parser.cc"
    break;

  case 897:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11590 "Parser/parser.cc"
    break;

  case 898:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11596 "Parser/parser.cc"
    break;

  case 899:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11602 "Parser/parser.cc"
    break;

  case 900:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11608 "Parser/parser.cc"
    break;

  case 902:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11614 "Parser/parser.cc"
    break;

  case 903:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11620 "Parser/parser.cc"
    break;

  case 904:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11626 "Parser/parser.cc"
    break;

  case 905:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11632 "Parser/parser.cc"
    break;

  case 907:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11638 "Parser/parser.cc"
    break;

  case 908:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11644 "Parser/parser.cc"
    break;

  case 909:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11650 "Parser/parser.cc"
    break;

  case 910:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11656 "Parser/parser.cc"
    break;

  case 911:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11662 "Parser/parser.cc"
    break;

  case 912:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11668 "Parser/parser.cc"
    break;

  case 913:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11674 "Parser/parser.cc"
    break;

  case 914:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11680 "Parser/parser.cc"
    break;

  case 916:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11686 "Parser/parser.cc"
    break;

  case 917:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11692 "Parser/parser.cc"
    break;

  case 918:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11698 "Parser/parser.cc"
    break;

  case 919:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11704 "Parser/parser.cc"
    break;

  case 920:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11710 "Parser/parser.cc"
    break;

  case 921:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11716 "Parser/parser.cc"
    break;

  case 923:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11722 "Parser/parser.cc"
    break;

  case 925:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11728 "Parser/parser.cc"
    break;

  case 926:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11734 "Parser/parser.cc"
    break;

  case 927:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11740 "Parser/parser.cc"
    break;

  case 928:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11746 "Parser/parser.cc"
    break;

  case 929:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11752 "Parser/parser.cc"
    break;

  case 930:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11758 "Parser/parser.cc"
    break;

  case 932:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11764 "Parser/parser.cc"
    break;

  case 933:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11770 "Parser/parser.cc"
    break;

  case 934:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 935:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11782 "Parser/parser.cc"
    break;

  case 936:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11788 "Parser/parser.cc"
    break;

  case 937:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11794 "Parser/parser.cc"
    break;

  case 938:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11800 "Parser/parser.cc"
    break;

  case 940:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11806 "Parser/parser.cc"
    break;

  case 941:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11812 "Parser/parser.cc"
    break;

  case 942:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11818 "Parser/parser.cc"
    break;

  case 943:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11824 "Parser/parser.cc"
    break;

  case 944:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11830 "Parser/parser.cc"
    break;

  case 947:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11836 "Parser/parser.cc"
    break;

  case 950:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11842 "Parser/parser.cc"
    break;

  case 951:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11848 "Parser/parser.cc"
    break;

  case 952:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11854 "Parser/parser.cc"
    break;

  case 953:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11860 "Parser/parser.cc"
    break;

  case 954:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11866 "Parser/parser.cc"
    break;

  case 955:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11872 "Parser/parser.cc"
    break;

  case 956:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11878 "Parser/parser.cc"
    break;

  case 957:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 958:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 959:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 960:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11902 "Parser/parser.cc"
    break;

  case 961:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11908 "Parser/parser.cc"
    break;

  case 962:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11914 "Parser/parser.cc"
    break;

  case 963:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11920 "Parser/parser.cc"
    break;

  case 964:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11926 "Parser/parser.cc"
    break;

  case 965:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11932 "Parser/parser.cc"
    break;

  case 966:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11938 "Parser/parser.cc"
    break;

  case 967:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11944 "Parser/parser.cc"
    break;

  case 968:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11950 "Parser/parser.cc"
    break;

  case 969:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11956 "Parser/parser.cc"
    break;

  case 971:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11962 "Parser/parser.cc"
    break;

  case 975:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11968 "Parser/parser.cc"
    break;

  case 976:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11974 "Parser/parser.cc"
    break;

  case 977:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11980 "Parser/parser.cc"
    break;

  case 978:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11986 "Parser/parser.cc"
    break;

  case 979:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11992 "Parser/parser.cc"
    break;

  case 980:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11998 "Parser/parser.cc"
    break;

  case 981:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12004 "Parser/parser.cc"
    break;

  case 982:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12010 "Parser/parser.cc"
    break;

  case 983:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12016 "Parser/parser.cc"
    break;

  case 984:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12022 "Parser/parser.cc"
    break;

  case 985:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12028 "Parser/parser.cc"
    break;

  case 986:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12034 "Parser/parser.cc"
    break;

  case 987:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12040 "Parser/parser.cc"
    break;

  case 988:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12046 "Parser/parser.cc"
    break;

  case 989:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12052 "Parser/parser.cc"
    break;

  case 990:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12058 "Parser/parser.cc"
    break;

  case 991:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12064 "Parser/parser.cc"
    break;

  case 994:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12070 "Parser/parser.cc"
    break;

  case 995:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12076 "Parser/parser.cc"
    break;


#line 12080 "Parser/parser.cc"

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
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
