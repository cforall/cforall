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
#define YYLAST   20043

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  994
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2018

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
    2436,  2442,  2445,  2451,  2452,  2454,  2455,  2456,  2458,  2460,
    2467,  2468,  2470,  2472,  2477,  2478,  2484,  2485,  2487,  2488,
    2493,  2494,  2495,  2497,  2505,  2506,  2508,  2511,  2513,  2517,
    2518,  2519,  2521,  2523,  2528,  2530,  2535,  2537,  2546,  2548,
    2553,  2554,  2555,  2559,  2560,  2561,  2566,  2567,  2572,  2573,
    2574,  2575,  2579,  2580,  2585,  2586,  2587,  2588,  2589,  2603,
    2604,  2609,  2610,  2616,  2618,  2621,  2623,  2625,  2648,  2649,
    2655,  2656,  2662,  2661,  2671,  2670,  2674,  2680,  2686,  2687,
    2689,  2693,  2698,  2700,  2702,  2704,  2710,  2711,  2715,  2716,
    2721,  2723,  2730,  2732,  2733,  2735,  2740,  2742,  2744,  2749,
    2751,  2756,  2761,  2769,  2771,  2776,  2777,  2782,  2783,  2787,
    2788,  2789,  2794,  2796,  2802,  2804,  2809,  2811,  2817,  2818,
    2822,  2826,  2830,  2832,  2833,  2834,  2839,  2842,  2841,  2853,
    2852,  2864,  2863,  2875,  2874,  2886,  2885,  2899,  2905,  2907,
    2913,  2914,  2925,  2932,  2937,  2943,  2946,  2949,  2953,  2959,
    2962,  2965,  2970,  2971,  2972,  2976,  2982,  2983,  2993,  2994,
    2998,  2999,  3004,  3009,  3010,  3016,  3017,  3019,  3024,  3025,
    3026,  3027,  3028,  3030,  3065,  3067,  3072,  3074,  3075,  3077,
    3082,  3084,  3086,  3088,  3093,  3095,  3097,  3099,  3101,  3103,
    3105,  3110,  3112,  3114,  3116,  3125,  3127,  3128,  3133,  3135,
    3137,  3139,  3141,  3146,  3148,  3150,  3152,  3157,  3159,  3161,
    3163,  3165,  3167,  3179,  3180,  3181,  3185,  3187,  3189,  3191,
    3193,  3198,  3200,  3202,  3204,  3209,  3211,  3213,  3215,  3217,
    3219,  3234,  3239,  3244,  3246,  3247,  3249,  3254,  3256,  3258,
    3260,  3265,  3267,  3269,  3271,  3273,  3275,  3277,  3282,  3284,
    3286,  3288,  3290,  3300,  3302,  3304,  3305,  3307,  3312,  3314,
    3316,  3321,  3323,  3325,  3327,  3332,  3334,  3336,  3350,  3352,
    3354,  3355,  3357,  3362,  3364,  3369,  3371,  3373,  3378,  3380,
    3385,  3387,  3404,  3405,  3407,  3412,  3414,  3416,  3418,  3420,
    3425,  3426,  3428,  3430,  3435,  3437,  3439,  3445,  3447,  3449,
    3452,  3456,  3458,  3460,  3462,  3496,  3497,  3499,  3501,  3506,
    3508,  3510,  3512,  3514,  3519,  3520,  3522,  3524,  3529,  3531,
    3533,  3539,  3540,  3542,  3551,  3554,  3556,  3559,  3561,  3563,
    3577,  3578,  3580,  3585,  3587,  3589,  3591,  3593,  3598,  3599,
    3601,  3603,  3608,  3610,  3618,  3619,  3620,  3625,  3626,  3631,
    3633,  3635,  3637,  3639,  3641,  3648,  3650,  3652,  3654,  3656,
    3659,  3661,  3663,  3665,  3667,  3672,  3674,  3676,  3681,  3707,
    3708,  3710,  3714,  3715,  3719,  3721,  3723,  3725,  3727,  3729,
    3736,  3738,  3740,  3742,  3744,  3746,  3751,  3753,  3755,  3762,
    3764,  3782,  3784,  3789,  3790
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

#define YYPACT_NINF (-1732)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-875)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      95, 11972,   140,   203, 16241,   111, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732,    83,   883,   107,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732,   142,   261, -1732,
   -1732, -1732, -1732, -1732, -1732,  4261,  4261,   187, 11972,   193,
     241, -1732, -1732,   247, -1732, -1732, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732,  2611, -1732,   379,   256, -1732, -1732, -1732,
   -1732, -1732, 16091, -1732, -1732,   281,   373,   458,   313, -1732,
    4261,   373,   373,   373,   357,  4650,   561,   863, 12131, -1732,
   -1732, -1732, 15941,  2106, -1732, -1732, -1732,  2516,   572, 12183,
    1021,  1081,  2516,  1095,   460, -1732, -1732, -1732, -1732,   571,
   -1732, -1732, -1732, -1732,   489, -1732, -1732, -1732, -1732, -1732,
     530,   500,   571, -1732,   571,   549, -1732, -1732, -1732, 16797,
    4261, -1732, -1732,  4261, -1732, 11972,   495, 16849, -1732, -1732,
    4783, 17861, -1732,   886,   886,   562,  2346, -1732, -1732, -1732,
   -1732,   293, 14551,  3722,   571, -1732, -1732, -1732, -1732, -1732,
   -1732,   588, -1732,   560,   605,   608, -1732,   652, 19518, 15171,
    3790,  2611,   447,   628,   655,   667,   674,   699,   709, -1732,
   -1732, 16999, 11001,   730, -1732, 16384, -1732, -1732, -1732, -1732,
     747, -1732, -1732,   722, -1732,  8016,   885, 18870, -1732,   768,
    4261,   500,   771,   767,   776,   778, -1732, -1732, -1732,  3180,
    2843,   796,   862,   -12, -1732, -1732,   571,   571,    76,   112,
     100,    76, -1732,   571,   571, -1732,  3309, -1732, -1732,   819,
     822,   886, 14129, -1732, -1732, 16091, -1732, -1732,  2516, -1732,
    1443,   460,   833,   911,   112,  4261,   458, -1732, 13527, -1732,
     886,   886,   835,   911,   112,  4261, -1732, 13276, -1732, -1732,
     886, -1732,   886, -1732,   669,  4005,  4261, -1732,  2030,   855,
   -1732, -1732, -1732, 16543,   500,   228, -1732, -1732, 17911, -1732,
     862,    26, -1732, 19518, 17861,  3543,  3309, -1732,   273, -1732,
   -1732, -1732, 16849,  4261, -1732,   857, -1732, -1732, -1732, -1732,
    4261,  3804,   334,   630, -1732,  4261,   560, -1732,   701,   571,
     858, 17051,   686, 14709, 14287,  2516,  2516, -1732,  2516,   886,
    2516,   886, -1732, -1732,   571, -1732,   907, -1732, 17201, -1732,
   -1732, -1732, 17253,   747, -1732,   881,   361,  1962,   893,   460,
     942, -1732,  2346,   836,   560,  2346,  2447, -1732,   919,  1000,
   19590,   968,   976, 19518, 19662,   992,  6700, -1732, -1732, -1732,
   -1732, -1732, -1732, 19734, 19734, 15017,   967,  4090, -1732, -1732,
   -1732, -1732,   542, -1732,   582, -1732,  1140, -1732, 19518, 19518,
   -1732,   965,   432,   728,  1105,   625,  1106,   983,   994,  1015,
    1029,   -21, -1732,   760, -1732,  1014, -1732,  1084,  4529, 15479,
   -1732, -1732,   715,  1014, -1732, -1732,   781, -1732, -1732,  3790,
    1020,  1043,  1052,  1064,  1072,  1079, -1732, -1732,   370,  1098,
   -1732,   282,  1098, -1732, -1732, 16797, -1732,  1088,  1099, 15633,
   -1732, -1732,  4349,  3405,  1142, 14709,  1144,   546,   696, -1732,
   -1732, -1732, -1732, -1732,  4261,  4713, -1732, -1732, -1732, -1732,
   -1732, -1732,  6482,  4241,   967,  8016,  1100,  1115, -1732, -1732,
    1108, 18870,   717, -1732, -1732, -1732, 18942,  1139, -1732, -1732,
   -1732, -1732, -1732,  3180,   733,  1127,  1148,  1150,   754,  1154,
    1157,  1165,  2843, -1732, -1732,   571,  1146,   458,  1137, -1732,
   -1732,  1172, -1732, -1732,   500,   911, -1732, -1732, -1732,   500,
   -1732, -1732,  3309, -1732, 15479, 15479, -1732,   886,  4783, 18639,
   14551, -1732, -1732, -1732, -1732, -1732,   500,   911,    26, -1732,
   -1732,  2516,  1177,   911,   112, -1732,   500,   911, -1732, 13419,
   -1732,   886,   886, -1732, -1732,  1183,   497,  1191,   460,  1216,
   -1732, 18069, -1732,   804, -1732,  1262, 18535, -1732,  4783, 17412,
   14129, -1732, 16543, 19806, -1732, -1732, -1732, -1732, -1732,  3543,
     777,  3309, -1732, 14551,   862, 11972, -1732,  1223, -1732,  1230,
   -1732, -1732, -1732, -1732, -1732,  2346, -1732, -1732,  1305,  4679,
   17253, 11001, -1732, 17464, -1732,   886,   886, -1732, -1732,   747,
   -1732,   800,  1247,  1396, 19518,   817,  1172,  1248, -1732,   571,
     571, -1732,  1098, -1732, 17051, -1732, -1732, 18350,   886,   886,
   -1732,  4679,   571, -1732, 17718, -1732, -1732, 17201, -1732,   293,
    1267,   188,  1268,  1962,   812, 16849,   829, -1732, -1732, -1732,
   -1732, -1732, -1732,   834, -1732,  1279,  1255, -1732, 15325, -1732,
   17516, 17516, -1732, 15325, -1732, 19518, -1732, 12183, 12183, 15325,
   -1732, -1732, 16595, 17516, 17516,  1084,  1307,  1471,   675,  1509,
   -1732,   842,  1280,  1075,  1281, -1732, 18942, 19518, 19014,  1276,
   19518,  2030, 19518,  2030, -1732,  2198, -1732, -1732, 19086,  2161,
   19518, 19086,  2030, -1732, -1732, 19518, 19518, 19518, 19518, 19518,
   19518, 19518, 19518, 19518, 19518, 19518, 19518, 19518, 19518, 19518,
   19518, 19518, 19518, 19518, 19158,  1261,   652,  4174, 11001, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
    1285, 19518, -1732, -1732,   715,  1890, -1732, -1732,   571,   571,
   -1732, -1732, 15479, -1732,   391,  1098, -1732,   852,  1098, -1732,
   -1732, -1732,  1172, -1732, -1732,  1172, 19878, -1732, -1732, 11001,
    1282,  1283,  2817,  1421,  3035,   442,  1248, -1732,   571,   571,
    1248,   457, -1732,   571,   571, 19518,  4261,  1092,  1094,  1248,
     -15, 14077, 14077,  4261, -1732, -1732, 19518,  1108, -1732,  8016,
    1299, -1732,  1652, -1732, -1732, -1732, -1732, -1732,   882, -1732,
   14077,  2030,  4783,  2030,   887,  1297,  1298,  1300,   899,  1301,
    1302,  1304,   511,  1098, -1732, -1732,   543,  1098, -1732, -1732,
   -1732,  4783,   652, -1732,  1098, 19878, -1732,   500, 18069, -1732,
   -1732,   905,  1306,   913,  1312, -1732,  1308, -1732,   500, -1732,
   -1732,   500,   911,  1308, -1732,   500,  1313,  1316,  1318, -1732,
   -1732, 18350, -1732,  1314, -1732, -1732, -1732,  2030,  4261, 10160,
    1401,  1290, 18437, -1732,  1099, -1732, 14077,   902, -1732, -1732,
    1308, -1732, 16849, 15479,  1317, -1732,  1317, -1732, -1732, -1732,
   -1732, 17201, -1732, 11163, 15787, -1732, 18069,  1326,  1327,  1329,
   -1732,  8525,   571, -1732,   817, -1732, -1732, -1732, -1732,  1172,
   -1732, -1732, -1732,   886, -1732,  3865, -1732, -1732,   460,  2646,
    1333, -1732, 18870, -1732,  1962,  1267, -1732, -1732,  1334,  1344,
    2447, 19086, -1732,  1345,   256,  1352,  1357,  1358,  1356,  1365,
   19518,  1366,  1368,  1378, 11001, 19518, -1732, -1732,  1879, -1732,
   -1732, -1732, 19518, -1732,  1379,  1380, 18726,  1111, -1732, 19086,
    1384, -1732,  1385, -1732, -1732,  3892, -1732, -1732,   920, -1732,
   -1732, -1732, -1732,  3892, -1732, -1732,  1113,   539, -1732, -1732,
     965,   965,   965,   432,   432,   728,   728,  1105,  1105,  1105,
    1105,   625,   625,  1106,   983,   994,  1015,  1029, 19518,  1125,
   -1732,  1381,  3892, -1732, -1732,  8016, -1732, 18069,  1383,  1389,
    1392,  1890, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
    1172, -1732, -1732,  1172, 18069, 18069, -1732, -1732,  2817,   830,
    1393,  1395,  1400,  1407,  2510,  3035, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
    1405, -1732,  1248, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
   -1732,  1411,  1412, -1732,   458,  3892,  1129,   -27, -1732, -1732,
    1399, -1732, 18870, -1732, 19518,   571, 19230, 14077, -1732, -1732,
   -1732,  1376,   552,  1098, -1732,   568,  1098, -1732, -1732, -1732,
   -1732,  1172, -1732, -1732, -1732,  1172,   862,  1413,  1172, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732,  1417, -1732, -1732,  1308,
   -1732,   500, -1732, -1732, -1732, -1732, -1732,  9212,  1419,  1416,
   -1732,    97, -1732,   494,   324, 10839,  1423, 13906,  1429,  1430,
    2721,  2767,  3682, 19302,  1431, -1732, -1732,  1435,  1439, -1732,
   -1732,   500, 19518, 19518,  1576,  1434,   518, -1732,  1519,  1438,
    1420, -1732, -1732, -1732,  9988, -1732, -1732, -1732, -1732, -1732,
    2487, -1732, -1732, -1732,  1507, -1732, -1732, -1732,  2030, -1732,
   -1732, 12608, 16091,  1441, -1732,  4261, -1732,  1424,  1446,  1447,
   -1732,  1132, -1732, -1732, -1732, -1732,  4783, -1732, -1732,  1437,
    1442,   930, 16849,   560,   560, -1732, -1732,   967,  1099, 15633,
   -1732,  1014, -1732, 11325, -1732,   593,  1098, -1732,   886,  8687,
   -1732, -1732,  1962,   571,   571,   293,   188, -1732, -1732,  1267,
    1464,  1473, -1732, -1732,   935,   547, 11001,  2030, -1732,   547,
   16647,   547, -1732, 19518, 19518, 19518, -1732, -1732, -1732, -1732,
   19518, 19518,  1466,  8016, -1732, -1732,  1472,   575, -1732, -1732,
   -1732,  2080, -1732, -1732,  1158, -1732,    96, -1732, 19086,  1173,
   -1732, 18942, -1732, -1732, 19518,  1454,  1178,  1188,  1108, -1732,
     603,  1098, -1732, -1732, 18069, 18069, -1732, -1732,  1478,   606,
    1098, -1732,   623,  4189,   571,   571, -1732, -1732, 18069, 18069,
   -1732,  1476, -1732, 14551, 14551,  1482,  1480,  1481,  1487, -1732,
    1484, 19518, 19518,  1202,  1486, -1732, -1732, -1732, -1732, -1732,
   -1732, -1732,  1490, 19518, -1732, -1732, -1732,  1172, -1732, -1732,
   -1732,  1172, 18069, 18069,   458,   571,  1205,  1493,  1498, -1732,
   -1732,  1499, 12761, 12914, 13067, 16849, 17516, 17516,  1500, -1732,
    1474,  1483,  2533, 13369, -1732,   237,  4261, -1732, -1732,  4261,
   -1732, 18798,   -14,   275, -1732, -1732, -1732, -1732, 19518,  1503,
    1578, 10676, 10332, -1732,  1485, -1732,  1488, 19518,  1491,  8016,
    1492, 19518, 18942, 19518,   910, -1732,  1496,   173, -1732,   225,
    1505, -1732, -1732,  1512, -1732,  1502, -1732,  1504,  1515, 13906,
     565, 13685,   571,   331, -1732, -1732, -1732,  1510, -1732,  1524,
   -1732,  1525, -1732,  1522, -1732,  1527, -1732, -1732, -1732, -1732,
   11487,  1513,  1529,  1530, -1732,  1535, -1732, -1732, -1732,  1172,
   19518, 19518,  1099,  1533, -1732,  1267, -1732,  1531,   482, -1732,
    1543, -1732, -1732, 16849, -1732,  1541,  1540,   973, -1732,  1542,
   -1732, -1732, -1732, -1732, -1732,  8016,  1108, 18942, -1732,  1579,
    3892, -1732,  1579,  1579, -1732,  3892,  3238,  3877, -1732, -1732,
    1207, -1732, -1732, -1732,  1551,  1549, -1732, -1732, -1732,  1172,
   -1732, -1732,  1554,  1557,   571, -1732, -1732, -1732,  1172, -1732,
   -1732, -1732,  1558, -1732, -1732, -1732, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732,  1559, -1732, -1732, -1732,
   -1732,  1564,  1560,   571, -1732, 18069, 18069, -1732, -1732, -1732,
   -1732, 19518, -1732, -1732,  1572, -1732,  1500,  1500,  1500,   847,
    1538,   352, -1732,  4380,   365, 15479, -1732, -1732, -1732,  4020,
   19518,  4548,   378, -1732, -1732,    30,  1566,  1566,  4261, -1732,
   -1732, 18218, -1732, 19518,  1571,  1580, -1732, -1732, -1732, -1732,
     981,  1581, 13906,  1438,  1585, 19518,   281,  1574,   357, 13226,
   16849, 13906, 19518, 19518,   898,   614, -1732, 19518, -1732, -1732,
     435, -1732,  1108, -1732,   982,   987,   995, -1732, -1732, -1732,
   -1732,   500,   910,  1583, -1732, -1732, 19518, -1732,  1588,   652,
   10839, -1732, -1732, -1732, -1732, 19518,  1620, -1732,  9446, -1732,
     571, 14551, -1732, -1732, 16849, -1732, -1732, -1732, -1732, -1732,
    1586, -1732, 18069, -1732, -1732,  1587, -1732,  1593,  1600,  1595,
    1962, -1732, -1732, -1732, -1732, 19518, -1732, 16647, 19518,  1108,
    1606,  1211, -1732,  1214, -1732,  3892, -1732,  3892, -1732, -1732,
   -1732, -1732, 18069,  1615,  1616, -1732, -1732, 18069, 18069,  1618,
    1619,  1217, 14235, 14393, -1732,  1622, -1732, -1732, -1732, -1732,
    1623,  1626,  1221, -1732, -1732, -1732, -1732,   847,  2014,   471,
   -1732, -1732, -1732, -1732,   571,   571, -1732, -1732, -1732,   492,
   -1732,  1004,  4020,   690, -1732,  4548,   571, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732,   507, 13906,   462, 19374,  1708,
   13906,  1438, 14867,  1712,  1438,  1621, -1732, -1732, -1732, -1732,
   18647, 19518, 13906, 10504,  1624, -1732,  1641,   491, 13906, -1732,
   -1732,  1644, -1732, -1732,  1629,   652,   665,  1639,  1645,  1228,
    1709, -1732, -1732, -1732, -1732,  4261,  4783, -1732, -1732,  1646,
    1650, -1732, -1732, -1732,  1962,  1267,  1658, -1732, -1732, -1732,
    1659, -1732, -1732, -1732,  1231,  1234, -1732, -1732, -1732, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732,  1660, -1732, -1732,  1661,
    1663, -1732, -1732, -1732,  1664,  1665,  1667,  2014, -1732,   571,
   -1732, -1732, -1732, -1732, -1732,  1656,  4380, -1732, -1732,  4107,
     105, 11652, -1732, 13788, -1732,    19,  1026, 13906,  1746, 13906,
   19518,  1668, 19518,  1024,  1648,   269,  1750, -1732, 19518,  1651,
   11813, -1732, -1732, -1732, 17666, -1732,  1670,  1653,   242, 13906,
   -1732, 19518, 19086,   496, -1732, -1732, -1732,  1679, -1732, -1732,
    1267,  1683, -1732, -1732, -1732, -1732,  1682,  1685,  1687, 14551,
    1689, -1732, -1732,   664,  1098, -1732, -1732,   847, -1732, -1732,
      93, -1732,   204, -1732, -1732, -1732,  1686, 12290, -1732, -1732,
   13906, -1732,    77, -1732, 13906, -1732, -1732,  1438,  1692,  1695,
   19518, 19518, 19518, 13906, -1732, -1732,  1698, 12290, 17666, -1732,
    3114, 17464,  2030,  1701, -1732,  1757,  1702,   698,  1706, -1732,
    1790, -1732,  1032, 13906,  1715, 13906, 13906, -1732,  1718, -1732,
   -1732, -1732, -1732, -1732, -1732, -1732, -1732,  1172, -1732, 19518,
   -1732, 19518, -1732, -1732,  1315, 12449, -1732, -1732, 13906, -1732,
   -1732,  1703,  1710,   341, -1732,  1438, -1732, -1732,  1315, -1732,
    1699,  3338,  3099, -1732, -1732, -1732,   242,  1724, 19518,  1713,
     242,   242, 13906, -1732, -1732, 19518,  1773,  1776, -1732, 18069,
   -1732, -1732, 13788, -1732,  1315, -1732, -1732, 19518, 19446, 19518,
   -1732,  1699, 19518,  1738,  3099,  1742,   652,  1758, -1732,   710,
   -1732, -1732,  1033,  1709,   535, -1732, -1732,  9641,  1753, 13788,
    1438, -1732,  1438,  1438,  1763,  1762, -1732,   500,   652,  1765,
   -1732,  1745,   652, -1732, -1732, 13906,  1851,  1774, -1732, -1732,
   -1732,  9816, -1732,   500, -1732, -1732,  1241, 19518, -1732,  1042,
   -1732, 13906, -1732, -1732,   652,  2030,  1775,  1754, -1732, -1732,
   -1732,  1045, -1732, -1732,  1756,  2030, -1732, -1732
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
     460,   461,   462,   463,   464,   471,   472,   758,   474,   547,
     548,   551,   553,   549,   555,     0,     0,     0,   421,     0,
       0,    16,   518,   524,     9,    10,    11,    12,    13,    14,
      15,   722,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   774,   421,   723,   370,     0,   373,   648,   375,   384,
       0,   374,   404,   405,     0,     0,     0,     0,   501,   423,
     425,   431,   421,   433,   436,   486,   473,   409,   479,   484,
     410,   496,   411,   511,   515,   521,   500,   527,   539,   758,
     544,   545,   528,   594,   376,   377,     3,   724,   737,   426,
       0,     0,   758,   796,   758,     2,   813,   814,   815,   421,
       0,   972,   973,     0,     1,   421,     0,   421,   393,   394,
       0,   501,   415,   416,   417,   727,     0,   550,   552,   554,
     556,     0,   421,     0,   759,   760,   546,   475,   641,   642,
     640,   701,   696,   686,     0,     0,   725,     0,     0,   421,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   519,
     522,   421,   421,     0,   974,   501,   803,   821,   978,   971,
     969,   976,   369,     0,   159,   654,   158,     0,   378,     0,
       0,     0,     0,     0,     0,     0,   368,   873,   874,     0,
       0,   403,   756,   758,   752,   777,   758,   758,   754,     2,
     758,   753,   834,   758,   758,   831,     0,   494,   495,     0,
       0,   421,   421,   438,     2,   421,   385,   424,   434,   487,
       0,   516,     0,   740,     2,     0,   648,   386,   501,   480,
     497,   512,     0,   740,     2,     0,   437,   481,   488,   489,
     498,   503,   513,   517,     0,   531,     0,   716,     2,     2,
     738,   795,   797,   421,     0,     2,     2,   982,   501,   985,
     756,   756,     3,     0,   501,     0,     0,   396,   758,   754,
     753,     2,   421,     0,   720,     0,   682,   684,   683,   685,
       0,     0,   678,     0,   668,     0,   677,   688,     0,   758,
       2,   421,   993,   422,   421,   433,   412,   479,   413,   504,
     414,   511,   508,   529,   758,   530,     0,   629,   421,   630,
     947,   948,   421,   631,   633,   518,   524,     0,   595,   596,
       0,   761,     0,   699,   687,     0,   765,    21,     0,    20,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   421,     2,     0,    98,    99,
     100,   101,    82,    24,    83,    38,    81,   102,     0,     0,
     117,   119,   123,   126,   129,   134,   137,   139,   141,   143,
     145,   147,   150,     0,    26,     0,   525,     2,   102,   421,
     151,   693,   644,   515,   646,   692,     0,   643,   647,     0,
       0,     0,     0,     0,     0,     0,   775,   801,   758,   811,
     819,   823,   829,     2,   980,   421,   983,     2,    95,   421,
       3,   628,     0,   993,     0,   422,   479,   504,   511,     3,
       3,   610,   614,   624,   630,   631,     2,   804,   822,   970,
       2,     2,    23,     0,     2,   654,    24,     0,   652,   655,
     991,     0,     0,   661,   650,   649,     0,     0,   742,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   780,   837,   758,     0,   648,     2,   776,
     784,   900,   778,   779,     0,   740,     2,   833,   841,     0,
     835,   836,     0,   399,   421,   421,   485,   422,     0,   501,
     421,   975,   979,   977,   502,   720,     0,   740,   756,   379,
     387,   435,     0,   740,     2,   720,     0,   740,   697,   482,
     483,   499,   514,   520,   523,   518,   524,   542,   543,     0,
     698,   421,   638,     0,   196,   362,   421,     3,     0,   501,
     421,   739,   421,     0,   381,     2,   382,   717,   401,     0,
       0,     0,     2,   421,   756,   421,   720,     0,     2,     0,
     681,   680,   679,   674,   432,     0,   672,   689,   477,     0,
     421,   421,   949,   422,   418,   419,   420,   953,   944,   945,
     951,     2,     2,    96,     0,   909,   923,   993,   905,   758,
     758,   914,   921,   636,   421,   509,   632,   422,   505,   506,
     510,     0,   758,   959,   422,   964,   956,   421,   961,     0,
     991,   601,     0,     0,     0,   421,     0,   773,   772,   768,
     770,   771,   769,     0,   763,   766,     0,    22,   421,    89,
     421,   421,    84,   421,    91,     0,    32,     0,    33,   421,
      87,    88,   421,   421,   421,     2,    98,    99,     0,     0,
     177,     0,     0,   545,     0,   969,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,    56,    57,    61,     0,
       0,    61,     0,    85,    86,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   421,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     158,     0,   156,   157,     2,   885,   645,   882,   758,   758,
     890,   526,   421,   802,   758,   812,   820,   824,   830,     2,
     805,   807,   809,     2,   825,   827,     0,   981,   984,   421,
       0,     0,     2,    96,   909,   758,   993,   855,   758,   758,
     993,   758,   870,   758,   758,     3,   632,     0,     0,   993,
     993,   421,   421,     0,     2,   663,     0,   991,   660,   992,
       0,   656,     0,     2,   659,   662,   174,   173,     0,     2,
     421,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   758,   789,   793,   832,   758,   846,   851,   781,
     838,     0,     0,   407,   897,     0,   743,     0,   421,   744,
     400,     0,     0,     0,     0,   398,     2,   745,     0,   383,
     720,     0,   740,     2,   746,     0,     0,     0,     0,   557,
     617,   422,     3,     3,   621,   620,   816,     0,     0,   421,
     363,     0,   501,     3,    95,     3,   421,     0,     3,   721,
       2,   676,   421,   421,   670,   669,   670,   478,   476,   595,
     955,   421,   960,   422,   421,   946,   421,     0,     0,     0,
     924,     0,   758,   994,   910,   911,   637,   907,   908,   922,
     950,   954,   952,   507,   542,     0,   958,   963,   598,   992,
       0,   158,     0,   597,     0,   991,   702,   700,     0,     0,
     765,    61,   726,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,     0,   116,   115,     0,   112,
     111,    27,     0,    28,     0,     0,     0,     0,     3,    61,
       0,    46,     0,    47,    54,     0,    53,    65,     0,    62,
      63,    66,    49,     0,    48,    52,     0,     0,    45,   118,
     120,   121,   122,   124,   125,   127,   128,   132,   133,   130,
     131,   135,   136,   138,   140,   142,   144,   146,     0,     0,
     372,     0,     0,    29,     3,   654,   152,   421,     0,     0,
       0,   886,   887,   883,   884,   695,   694,     2,   806,   808,
     810,     2,   826,   828,   421,   421,   902,   901,     2,     0,
       0,     0,     0,     0,   758,   910,   858,   875,     2,   853,
     861,   634,   856,   857,   635,     2,   868,   878,   871,   872,
       0,     3,   993,   391,     2,   986,     2,   625,   626,   604,
       3,     3,     3,     3,   648,     0,   150,     0,     3,     3,
       0,   657,     0,   651,     0,   758,     0,   421,     3,   395,
     397,     0,   758,   790,   794,   758,   847,   852,     2,   782,
     785,   787,     2,   839,   842,   844,   756,     0,   898,     3,
     748,     3,   491,   490,   493,   492,     2,   721,   749,     2,
     747,     0,   721,   750,   557,   557,   557,   421,     0,     0,
     639,     0,   366,     0,     0,   421,     0,     2,     0,     0,
       0,     0,     0,   179,     0,   296,   297,     0,     0,   335,
     334,     0,   154,   154,   341,   518,   524,   193,     0,   180,
       0,   204,   181,   182,   421,   198,   183,   184,   185,   186,
       0,   187,   188,   302,     0,   189,   190,   191,     0,   192,
     200,   501,   421,     0,   202,     0,   360,     0,     0,     0,
       3,     0,   728,   721,   709,   710,     0,     3,   705,     3,
       3,     0,   421,   686,   686,   957,   962,     2,    95,   421,
       3,   516,     3,   422,     3,   758,   917,   920,   421,     3,
     906,   912,     0,   758,   758,     0,   601,   586,   602,   991,
       0,     2,   762,   764,     0,    90,   421,     0,    94,    92,
     421,     0,   106,     0,     0,     0,   110,   114,   113,   178,
       0,     0,     0,   654,   103,   171,     0,     0,    41,    42,
      79,     0,    79,    79,     0,    67,    69,    44,     0,     0,
      40,     0,    43,   149,     0,     0,     0,     0,   991,     3,
     758,   893,   896,   888,   421,   421,     3,     3,     0,   758,
     864,   867,   758,     0,   758,   758,   859,   876,   421,   421,
     987,     0,   627,   421,   421,     0,     0,     0,     0,   380,
       3,     0,     0,     0,     0,   653,   658,     3,   741,   176,
     175,     3,     0,     0,     2,   783,   786,   788,     2,   840,
     843,   845,   421,   421,   648,   758,     0,     0,     0,   721,
     751,     0,   421,   421,   421,   421,   421,   421,   540,   568,
       3,     3,   569,   501,   558,     0,     0,   798,     2,     0,
     364,    61,     0,     0,   287,   288,   201,   203,     0,     0,
       0,   421,   421,   283,     0,   281,     0,     0,     0,   654,
       0,     0,     0,     0,     0,   155,     0,     0,   342,     0,
       0,     3,   208,     0,   199,     0,   278,     0,     0,     2,
       0,   501,   758,     0,   361,   904,   903,     0,     2,     0,
     712,     2,   707,     0,   708,     0,   690,   671,   675,   673,
     421,     0,     0,     0,     3,     0,     2,   913,   915,   916,
       0,     0,    95,     0,     3,   991,   591,     0,   601,   599,
       0,   589,   703,   421,   767,     0,     0,     0,    34,     0,
     107,   109,   108,   105,   104,   654,   991,     0,    60,    76,
       0,    70,    77,    78,    55,     0,     0,     0,    64,    51,
       0,   148,   371,    30,     0,     0,     2,   889,   891,   892,
       3,     3,     0,     0,   758,     2,   860,   862,   863,     2,
     877,   879,     0,   854,   869,     3,     3,   988,     3,   612,
     611,   615,   990,     2,     2,   989,     0,     3,   755,   664,
     665,     0,     0,   758,   402,   421,   421,     3,     3,   408,
     757,     0,   848,   732,     0,   734,   540,   540,   540,   575,
     545,     0,   581,   569,     0,   421,   532,   567,   563,     0,
       0,     0,     0,   570,   572,   758,   583,   583,     0,   564,
     579,   421,   367,     0,     0,    62,   291,   292,   289,   290,
       0,     0,     2,   219,     0,     0,   221,   375,   220,   501,
     421,     2,     0,   179,   257,     0,   252,   179,   284,   282,
       0,   276,   991,   285,     0,     0,     0,   323,   324,   325,
     326,     0,   316,     0,   317,   293,     0,   294,     0,     0,
     421,   210,   197,   280,   279,     0,   314,   333,     0,   365,
     758,   421,   730,   691,   421,     2,     2,   965,   966,   967,
       0,   918,   421,     3,     3,     0,   926,     0,     0,     0,
       0,   600,   588,     3,    93,     0,    31,   421,     0,   991,
       0,     0,    80,     0,    68,     0,    74,     0,    72,    39,
     153,   894,   421,     0,     0,   799,   817,   421,   421,     0,
       0,     0,   421,   421,   667,     0,   388,   390,     3,     3,
       0,     0,     0,   736,   536,   538,   534,     0,   933,     0,
     576,   938,   578,   930,   758,   758,   562,   582,   566,     0,
     565,     0,     0,     0,   585,     0,   758,   559,   573,   584,
     574,   580,   619,   623,   622,     0,     2,     0,     0,   240,
       2,   222,   501,   248,   258,     0,   273,   274,   275,   272,
     261,     0,     2,   421,     0,   277,     0,     0,     2,   300,
     327,     0,   318,     2,     0,     0,     0,     0,   305,     0,
     301,   195,   194,   389,   706,     0,     0,   968,     3,     0,
       0,   925,   927,   590,     0,   991,     2,    37,    35,    36,
       0,    58,   172,    71,     0,     0,     3,   800,   818,     3,
       3,   865,   880,   392,     2,   609,     3,   608,   666,     0,
       0,   791,   849,   899,     0,     0,     0,   934,   935,   758,
     561,   931,   932,   560,   541,     0,     0,   209,   299,     0,
       0,     0,   233,     2,   211,     0,     0,     2,   242,     2,
     179,   266,     0,   262,     0,   259,   250,   253,   179,     0,
       0,   214,   298,     2,   421,   295,     0,     0,   343,     2,
     303,     0,    61,     0,   315,   711,   713,     0,   928,   929,
     991,     0,   704,    59,    75,    73,     0,     0,     0,   421,
       0,   792,   850,   758,   941,   943,   936,     0,   571,   228,
     223,   226,     0,   225,   232,   231,     0,   421,   235,   234,
       2,   244,     0,   241,     2,   249,   254,   263,   274,   272,
       0,   179,     0,     2,   256,   286,     0,   421,   421,     3,
     328,   422,   332,     0,   336,     0,     0,     0,   344,   345,
     217,   306,     0,     2,     0,     2,     2,   919,     0,   593,
     895,   866,   881,   613,     2,   937,   939,   940,   577,     0,
     230,     0,   229,   213,   236,   421,   356,   245,     2,   246,
     243,   268,   267,   264,   255,   260,   251,   216,   236,     3,
     321,     0,   933,   329,   330,   331,   343,     0,     0,     0,
     343,     0,     2,   304,   311,     0,   308,   310,   592,   421,
     224,   227,     2,     3,   237,   357,   247,     0,     0,     0,
       3,   321,     0,     0,   934,     0,     0,     0,   337,     0,
     346,   218,     0,   301,     0,     3,   205,     0,     0,     2,
     270,   271,   269,   265,     0,     0,   322,     0,   349,     0,
     347,     0,   349,   307,   309,     2,     0,     0,   207,   206,
     212,     0,   215,     0,   319,   350,     0,     0,   338,     0,
     312,     2,   942,   320,     0,     0,     0,     0,   313,   351,
     352,     0,   348,   339,     0,     0,   340,   353
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1732,  5611,  5612, -1732,    -1,   493,  1372,  -139, -1732,  1550,
   -1732,   317, -1732,  -682,   595,   689,  -847, -1039, -1732,   157,
    6467,  1914, -1732,   344, -1732,  1270,   101,   704,   707,   508,
     703,  1235,  1229,  1236,  1233,  1227, -1732,  -100,  -157,  8271,
     810, -1732,  -384, -1732, -1732,  -644,  4064, -1049,  2616, -1732,
     414, -1732,   802,   -19, -1732, -1732, -1732,   369,    50, -1732,
   -1731, -1403,   245,    36, -1732, -1732, -1732,   255,   166, -1732,
   -1732, -1732, -1732,    -3, -1687,   149, -1732, -1732,     0, -1732,
   -1732, -1732,    13,   393,   401,   106, -1732, -1732, -1732, -1732,
    -865, -1732,    44,    -6, -1732,   114, -1732,   -82, -1732, -1732,
   -1732,   814,  -623,  -951, -1227, -1732,    69, -1033,   185,  2861,
    -910,  -894, -1732,  -263, -1732,    55,  -150,   280,  -154,  -223,
    3529,  6712,  -642, -1732,     3,    12,   892,  2145, -1732,  1934,
   -1732,    65,  3714,  -284, -1732, -1732,    75, -1732, -1732,   486,
     110,  4363,  2742,   -59,  1736,  -272, -1732, -1732, -1732, -1732,
   -1732,  -402,   230,  4624, -1732,  -352,   102, -1732,   475,   213,
   -1732,   154,   668, -1732,   467,  -121, -1732, -1732, -1732,  5015,
    -622, -1158,  -537,  -478,  -331,  1107, -1732, -1176,  -161,   -57,
    1232,   839,  7413,  -174,  -469,  -231,  -165,  -422,  1209, -1732,
    1534,  -191,  1123,  1418, -1732, -1732, -1732, -1732,   235,  -168,
      47,  -863, -1732,    91, -1732, -1732,   578,   408, -1732, -1732,
   -1732,  2005,  -772,  -407,  -880,   -24, -1732, -1732, -1732, -1732,
   -1732, -1732,     9,  -795,  -131, -1714,  -159,  7453,   -65,  6074,
   -1732,  1087, -1732,  1398,  -203,  -205,  -204,  -193,     1,   -67,
     -66,   -45,   519,    -5,     4,    46,  -183,   -55,  -178,  -175,
    -166,  -714,  -695,  -693,  -678,  -684,   -38,  -674, -1732, -1732,
    -675,  1274,  1277,  1284,  1889,  7212,  -566,  -572,  -571,  -570,
    -686, -1732, -1572, -1620, -1608, -1582,  -592,  -115,  -212, -1732,
   -1732,   -20,   398,  -104, -1732,  7789,   900,  -471,  -400
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1138,   213,   382,   383,    80,    81,   384,   359,   385,
    1427,  1428,   386,   958,   959,   960,  1244,  1245,  1246,  1439,
     408,   388,   389,   390,   668,   669,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1057,   670,
    1366,   731,   207,   733,   404,   798,  1139,  1140,  1141,  1142,
    1143,  1144,  1145,  1967,  1146,  1147,  1371,  1544,  1841,  1842,
    1782,  1783,  1784,  1943,  1944,  1148,  1555,  1556,  1701,  1149,
    1150,  1151,  1152,  1153,  1154,  1379,  1719,  1886,  1814,  1155,
    1156,  1572,  1953,  1573,  1574,  1869,  1157,  1158,  1159,  1369,
    1877,  1878,  1879,  1996,  2011,  1904,  1905,   284,   285,   859,
     860,  1111,    83,    84,    85,    86,    87,    88,   441,    90,
      91,    92,    93,    94,   221,   558,   443,   412,   444,    97,
     294,    99,   100,   101,   324,   325,   104,   105,   166,   106,
     878,   326,   152,   109,   241,   110,   153,   250,   328,   329,
     330,   154,   405,   115,   116,   332,   117,   549,   848,   846,
     847,  1516,   333,   334,   120,   121,  1107,  1334,  1522,  1523,
    1659,  1660,  1335,  1511,  1678,  1524,   122,   632,  1609,   335,
     630,   913,  1050,   449,   450,   852,   853,   451,   452,   854,
     337,   553,  1163,   414,   415,   208,   469,   470,   471,   472,
     473,   313,  1183,   314,   876,   874,   583,   315,   353,   316,
     317,   416,   124,   172,   173,   125,  1177,  1178,  1179,  1180,
       2,  1096,  1097,   575,  1172,   126,   304,   305,   252,   262,
     532,   127,   211,   128,   222,  1059,   839,   499,   164,   129,
     643,   644,   645,   130,   224,   225,   226,   227,   299,   132,
     133,   134,   135,   136,   137,   138,   230,   300,   232,   233,
     234,   766,   767,   768,   769,   770,   235,   772,   773,   774,
     736,   737,   738,   739,   500,   139,   607,   608,   609,   610,
     611,   612,  1662,  1663,  1664,  1665,   597,   454,   340,   341,
     342,   417,   199,   141,   142,   143,   344,   790,   613
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   336,   131,    79,   102,   354,   183,   184,   181,   967,
    1181,   915,   322,   103,   485,   486,   190,   493,   516,   297,
     557,   403,   901,   673,   732,   529,   487,   940,   185,   887,
     888,   889,   947,  1813,   176,   289,   488,  1764,   358,   895,
     231,   489,   475,   787,   490,   832,   834,   339,  1419,  1765,
    1026,   615,  1849,   491,    79,    79,    95,    79,    57,   131,
    1002,   102,   198,   497,  1164,  1843,   107,  1020,   186,  1021,
     103,  1102,    79,   148,  1360,  1766,   111,   187,   402,  1030,
    1027,    79,   658,   196,  1022,  1037,  1768,   436,  1023,    79,
     485,   486,  1850,   493,    79,  -714,   228,    79,  1173,   253,
      57,    79,   487,   263,  1291,   513,  1844,  1479,  1480,   713,
     248,   112,   488,    95,   420,   421,  1249,   489,   836,   188,
     490,   292,   496,   107,  1546,   350,   625,   198,   843,   491,
     628,   564,   566,   111,   565,   453,   422,   278,  1292,    79,
     144,   498,    79,   279,    79,  1256,   131,   505,   102,    79,
    1908,   714,   183,   184,   483,    79,  1329,   103,   604,   910,
    1536,   634,    79,   209,   636,   494,   917,   592,   112,   870,
      57,   256,   527,   615,   185,   565,   423,   260,   658,    79,
      79,   196,   537,   498,   623,   424,    89,  1843,   626,   149,
     887,   888,   889,  1851,    79,  1836,   457,  1330,   895,  -358,
      95,   674,  1441,  -715,   466,  1900,   598,   896,  1290,    79,
     107,  1849,    57,  1331,   186,   544,  1446,  1318,    79,    79,
     111,   196,  1321,   187,  1899,   522,   155,   425,   183,   184,
     569,   118,   156,  -740,   118,    79,  1160,   504,  1849,  1214,
     509,   494,   868,    89,    79,  1053,   196,   912,  1447,   506,
     185,  1909,  1339,   498,    79,   112,   161,    79,  1292,   533,
    1611,  1845,   526,  1068,    79,   188,   823,  1237,  1576,  -358,
     531,  1340,   536,  1006,    79,    79,  1813,    79,   805,   806,
      19,    96,   522,   246,   150,   592,   560,   257,   118,   819,
     807,   162,  1209,  1389,    79,    79,  1228,   196,  1924,   863,
     808,  1764,    79,  1051,  1051,   809,   791,  1578,   810,    79,
      79,  1276,   118,  1765,    79,  -359,  1060,   811,  1547,  1547,
    1347,   615,  1051,  1020,   598,  1021,  1263,  1319,  1201,  1170,
      89,   758,   118,  1030,   204,   248,   175,   965,    96,  1766,
    1022,  1277,   177,    57,  1268,   615,    79,  1577,   107,  1164,
    1768,    79,   615,  1546,    79,   642,   538,   357,   111,  1901,
    1902,   911,   194,   587,   805,   806,  1031,   550,   819,   118,
    1034,  1329,  1329,  1329,  1579,   118,   807,   118,   882,  1047,
    1048,   830,  1836,  1342,  1343,  -359,   808,   835,  1051,   198,
     178,   809,  1528,   112,   810,  1875,   179,    62,    63,   140,
     209,   587,   140,   811,   190,   771,  1624,  1626,  1628,   118,
     191,  1529,  1330,  1330,  1330,   907,   420,   421,    79,   287,
     457,   118,   572,   322,   942,    96,   498,   194,  1331,  1331,
    1331,   753,   279,  1099,  1862,   498,   880,   820,   422,  1504,
      57,    79,    79,   530,  1210,    75,   887,   888,   889,  1538,
    1052,  1052,   842,    79,    79,   202,   140,   248,   339,  1201,
     900,    57,    79,   453,   466,   580,  1479,  1480,   210,  1052,
    -740,   434,   118,   906,   923,   118,   925,   926,   423,   927,
     118,   279,    79,   189,    63,   929,  1339,   424,   931,   932,
     933,    79,  1346,   457,   581,   582,   942,   146,  1344,   598,
     140,  1942,   420,   421,  1695,  1589,  1949,  1667,  1704,  1408,
     179,    79,    57,   118,   827,  1942,   820,    79,  -874,   749,
    1528,  1160,   518,   498,   422,   521,  1668,    57,    -3,   425,
    1301,   216,   118,  1676,   453,  1052,   838,  1779,  1780,  1670,
    1007,  1969,   841,   140,   498,   280,   845,  1547,   170,   170,
     157,   869,  1677,   158,   159,    79,   160,    79,    13,    14,
      15,    16,    17,    96,   236,   560,  1779,  1780,    79,  1883,
      79,   248,   457,  1258,    79,  -415,   131,   991,   102,   615,
    1415,    57,   521,   170,    79,  1378,  1471,   103,    79,  1051,
     942,  1028,   696,  1623,   884,   602,   278,   901,   426,   697,
     698,   197,  1884,   868,   204,   118,  1035,  1450,  1985,  1705,
     602,   615,   274,    57,   229,   205,    57,   254,  1041,  1781,
      79,   264,    57,  1128,  1061,   478,  1769,   107,   204,   531,
      95,   206,    79,   170,   274,   260,   170,   111,    57,   118,
     107,    57,  1282,  1341,   276,  1770,   179,  1676,  1800,   170,
     111,  1794,  1671,  1893,  -537,   911,   348,   279,   544,  1534,
    1078,   453,   942,    57,   498,   118,  1773,   179,   293,  1186,
    1547,  1070,   112,    57,  -642,   402,    57,  -419,   447,   278,
      79,  1777,    79,  1087,    79,   112,  1056,  1252,    79,   194,
    1086,    79,  1082,    57,  1248,   680,   498,  1232,   561,   197,
     681,  1304,   453,   170,  1233,   498,  -358,   652,    13,    14,
      15,    16,    17,  1397,   458,   757,    79,  1308,  1565,  -729,
     857,   498,   279,  1438,   453,   453,   771,  1185,  1545,  1557,
    1248,   311,   693,   694,    57,   682,  1052,   352,  1420,   197,
     683,  1856,  1406,   453,   118,   118,   602,  1208,   170,  1864,
     703,   704,  1456,   693,   355,  1465,   498,   356,   170,   498,
      89,    79,   884,    79,   197,  1702,    57,   357,   547,   170,
    1703,   552,  1469,   543,    63,    79,   602,   534,  1744,   427,
    1745,   584,    79,   693,   322,   585,   118,  1454,   466,   672,
     118,    79,   118,  1621,   705,   706,   170,   970,   971,   972,
      79,    79,    79,   170,   170,   118,   428,   595,   170,   453,
     618,  1436,  1914,  1894,  1547,    72,  1807,   498,   429,   339,
      79,  1808,   912,  1289,   595,   430,   935,  -420,   595,    13,
      14,    15,    16,    17,  1547,   601,   150,   936,   937,   602,
     170,   426,    96,   498,    72,   170,    77,   603,   170,  1929,
     431,  1101,   588,   274,  1930,    96,    79,    79,   466,   604,
     432,  1981,  1162,   248,   734,   118,  1982,  1204,   498,   792,
     793,   103,  1547,   794,   531,    77,    78,   461,   118,   456,
     118,   118,   278,   118,   426,   868,   498,    57,  1253,   118,
     699,   700,   118,   118,   118,   248,   460,  1296,    13,    14,
      15,    16,    17,   506,    79,   815,   474,   498,    79,   237,
     238,   715,   239,    79,    95,   716,   240,   476,   826,   642,
     479,   615,   480,   829,   107,   595,   572,  1314,   426,    72,
     498,   481,   741,   482,   111,   170,   742,  1562,   458,   157,
     837,  1174,   158,   159,  1608,   160,    72,   170,   170,   601,
     844,   495,  1478,   602,    79,   856,    57,  1545,   496,   857,
      77,    78,    79,   916,  1056,  1620,   601,   585,   514,   112,
     602,   515,   118,   140,  1510,  1410,    72,    77,   603,  1028,
     918,   426,   201,   602,   585,   919,  1275,   771,  1735,   920,
     525,    79,   535,   941,   466,   266,  1657,   942,   209,   267,
     498,  1011,   270,   554,   272,   498,   447,    77,    78,   635,
     453,   458,   590,  1619,   576,   354,   354,    79,  1567,  1568,
    1569,  1570,  1571,    79,    79,   243,     6,     7,     8,     9,
      10,    11,    12,  1065,   912,   900,   278,  1066,  -873,   969,
     498,  1696,  1697,  1698,    89,  1390,  1429,   201,   506,   146,
    -587,   572,   498,   942,    79,   498,  1092,  1175,   622,   447,
     942,  1955,   672,  1699,  1094,  1959,   322,   672,   942,   868,
     646,  1247,  1700,   672,   598,  1248,   595,   447,   170,   418,
    1557,  1396,   879,  1499,  -416,   742,  1424,  1336,  1548,   118,
    1248,  1706,   672,    13,    14,    15,    16,    17,  -417,   633,
     595,   339,   118,   118,  1654,  1655,  1656,    13,    14,    15,
      16,    17,  1820,   595,   904,   647,   466,   650,  1162,    79,
      79,    79,   676,  1481,  1616,   651,   170,   103,  1617,  1526,
    1882,   695,  1687,  1707,  1487,  1488,   942,   942,  1708,    96,
     266,   655,  1066,   466,   709,   523,  1709,  1162,  1740,    79,
     942,    57,  1176,   710,  1451,  1774,   103,    79,   712,   742,
      79,    79,   253,   263,    79,    57,   717,  1696,  1858,  1698,
      95,   743,  -406,   248,   951,    79,   953,  1852,   956,   942,
     107,   942,   964,  1933,  1983,   968,   711,  1248,   942,  1859,
     111,   402,   402,  2007,   744,  -406,  2014,  2004,  -180,    95,
    2015,    79,   523,   745,   447,   531,  1322,  1323,  1324,   107,
     993,   977,   978,   979,   980,   746,    79,   266,   267,   111,
     619,   600,   272,   747,  1906,   112,   453,   453,   701,   702,
     748,   256,   466,   707,   708,   944,   945,   260,   191,   676,
      79,  1090,   590,   676,  1906,   447,  1043,  1044,  1045,  1046,
     322,   433,  1098,    -3,   112,  1100,   -17,   140,   684,  1103,
     685,   686,   687,   789,  1821,  1235,  1066,  1250,  1251,   170,
     140,   788,    79,   775,  1222,  -418,   170,  1527,   812,  1226,
     942,  1254,  1945,  -151,  -151,   339,  1045,  1388,   799,   688,
    1234,   824,   689,   690,  1069,   822,  1071,   691,   692,   813,
      89,   814,  1336,  1336,  1336,   816,  1512,  1336,   817,   201,
    1526,  1756,  1444,  1445,   485,   486,   818,  1548,   493,  -116,
    -116,  -116,  -116,  -116,  -116,   286,   487,  1449,  1445,    89,
     858,    79,  1453,  1445,   840,    79,   488,   118,    79,   600,
    -535,   489,  1017,  1437,   490,   118,   246,   257,  -533,  1888,
    1110,   170,   170,   491,  1550,  1550,  1489,  1437,   466,  1017,
    1501,  1629,  1066,   103,   103,  1742,  1066,   595,  1743,  1445,
     618,  1753,  1754,   849,   118,  1763,   942,   871,   466,   873,
      79,   877,   533,  1811,  1812,  1824,  1445,  1332,  1825,  1445,
    1779,  1780,   118,   531,   148,    96,  2004,  2005,  1203,  1442,
    1443,   890,   170,   973,   974,  1679,  1679,   170,   975,   976,
     981,   982,   118,   892,   418,   418,   107,   107,  1398,  1399,
     447,   604,   909,   266,    96,   914,   111,   111,   921,   922,
    1481,   943,   946,   949,   466,   990,  1016,  1017,  1024,    79,
    1716,   322,   995,  1429,    79,    79,    79,  1063,  1072,  1073,
     118,  1074,  1075,  1076,  1672,  1077,  -718,  1093,  1527,   652,
      18,   112,   112,  1095,  1166,  -618,   494,   805,   806,  1165,
    1104,   182,   819,  1105,  1683,  1106,   339,  1195,  1196,   807,
    1197,  1207,  1481,  -115,  -115,  -115,  -115,  -115,  -115,   808,
    1182,  1211,  1174,   223,   809,  1212,  1215,   810,    47,    48,
      49,    50,    51,    52,    53,    54,   811,  1217,  1218,  1219,
     149,  1220,    79,   140,   902,  1320,  1221,  1223,    79,  1224,
      79,    13,    14,    15,    16,    17,   939,    79,  1345,  1225,
    1230,  1231,  1255,   693,  1260,   418,    89,    89,  1238,  1239,
    1261,   466,   140,  1262,  1269,  1364,  1270,  1295,   298,  1303,
     466,  1271,   118,   118,   118,   118,   118,   118,  1272,  1280,
     140,   248,  -606,  -605,  1315,  -719,   530,  1430,  1431,  1432,
    1337,  1338,  1348,  1526,  1433,  1434,  1806,   467,  1351,  1352,
    1361,   118,   118,  1550,  1362,   453,   453,   466,  1363,  1368,
    -641,  1370,   103,   942,  1372,  1816,  1378,  1382,  1384,   615,
    1385,  1386,  1332,  1332,  1332,   150,  1509,  1513,  1175,    79,
     170,  1392,  1421,   170,   170,   170,  1394,   484,   223,   256,
     820,  1422,  1840,  1435,    79,   260,    79,  1437,  1452,  1464,
    1477,    96,    96,  1482,   298,  1483,  1484,   170,  1485,  1445,
    1490,  1493,   418,   170,  1502,   107,  1503,  1505,  1517,  1515,
    1870,   552,  1341,   118,  1580,   111,  1541,  1518,   170,  1558,
    1582,  1590,  1559,  1174,  1585,  1561,  1563,  1597,  1481,  1876,
    1575,    79,  1592,  1593,    79,   595,  1583,  1595,  1584,   402,
     254,   264,  1596,  1598,  1599,   466,  1601,  1606,  1610,   466,
     112,  1612,  1614,   570,   298,   170,  1615,  1622,  1618,  1630,
    1631,   466,   447,  1176,   531,  1635,  1550,   466,  1636,   426,
    1425,  1646,  1666,  1644,  1870,   103,   485,   486,  1489,   493,
    1653,  1527,  1520,   140,    79,    79,  1248,  1720,   487,   210,
    1688,  1686,  1713,    79,   246,   257,  1690,  1715,   488,  1923,
    1727,  1731,  1940,   489,  1840,   118,   490,  1732,  1733,   140,
     140,   819,  1734,    61,  1741,   491,  1537,  1539,    64,    65,
      66,    67,    68,    69,    70,    89,  1747,  1748,   107,  1751,
    1752,  1957,   453,   418,  1761,    79,  1758,  1762,   111,  1175,
     118,  1787,   466,   855,  1872,  1789,   466,  1876,   466,  1799,
    1809,  1876,  1876,  1803,  1587,  1790,  1810,  1128,  1798,   402,
    1818,   402,    74,  1805,  1819,   784,  1822,  1823,   466,   498,
     118,  -607,  1831,   112,  1832,  1833,  1834,  1979,  1835,  1854,
    -518,   140,  1861,  1863,   118,  1865,  1873,  1874,   402,   170,
    1887,  1889,   170,  1890,  1903,   785,  1891,   467,  1892,  1995,
    2006,   765,  1911,  1995,  1754,  1912,  1917,   118,  1872,   466,
    1550,  1928,  1976,   466,   183,   184,   569,  1926,  1927,   103,
      96,  1931,   466,  1932,  1935,  2009,  1938,   494,  1947,    79,
    1550,    79,   170,  1952,  1176,  1948,   185,   530,  1956,   103,
    1963,   804,   466,  1964,   466,   466,   447,  1958,    89,  1977,
     223,    13,    14,    15,    16,    17,  1227,   402,  1978,   820,
     534,  1990,    13,    14,    15,    16,    17,   466,  1550,  1980,
     298,  1992,   107,  1993,  1997,    82,   298,   103,   147,  1998,
      79,    79,   111,   196,  2001,  2002,  2012,   679,  2013,  1718,
    2016,   466,   107,   118,  1738,  1091,  1535,  1448,   938,   984,
     987,   466,   111,  1367,   983,   986,  1374,   985,   140,  1717,
    1991,  1941,  1801,    79,  1950,   457,   298,   112,  1797,  1860,
      57,  1986,  1885,  1984,  1975,  1711,   466,   867,   466,   298,
     107,   629,    82,  1712,  1919,  1960,  1999,   112,   140,  1383,
     111,  1918,   167,    96,   466,  1710,   524,   180,  1669,  1838,
     466,  1898,   140,  1194,  1680,  1514,    82,  1380,  1062,  1184,
     466,  1613,  1724,   875,    79,     3,   795,  1213,   998,   220,
       0,   999,   245,     0,    79,   112,    82,     0,  1000,    72,
       0,   170,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,    89,     0,     0,   170,     0,     0,     0,   734,
       0,     0,     0,   498,   170,     0,     0,     0,     0,     0,
      77,    78,    89,   147,     0,     0,     0,     0,     0,    82,
     855,   147,     0,    61,   296,   302,   168,   169,    64,    65,
      66,    67,    68,    69,    70,     0,   321,   118,     0,     0,
       0,   170,   418,     0,    57,     0,     0,     0,     0,   902,
      89,     0,     0,   409,   180,   180,     0,   118,     0,     0,
       0,   140,     0,   170,  1259,   147,   439,     0,     0,   245,
     243,     6,     7,     8,     9,    10,    11,    12,   855,   508,
       0,  1266,  1267,     0,     0,     0,     0,    96,     0,     0,
       0,    61,     0,   220,   220,   118,    64,    65,    66,    67,
      68,    69,    70,    72,     0,     0,     0,    96,     0,     0,
     296,     0,     0,     0,     0,  1881,     0,     0,     0,    82,
    1019,   467,   765,  1657,   785,     0,     0,   498,     0,   265,
       0,     0,   245,     0,    77,    78,     0,     0,    18,   170,
       0,    61,     0,   170,     0,    96,    64,    65,    66,    67,
      68,    69,    70,  1240,     0,   170,     0,  1241,     0,  1242,
     298,   170,   302,     0,     0,     0,     0,     0,   302,   296,
     296,     0,     0,     0,     0,     0,   147,     0,   170,   298,
      51,    52,    53,    54,     0,     0,     0,   170,     0,   855,
      74,     0,     0,  1440,     0,   321,   605,   614,     0,     0,
       0,     0,     0,   249,     0,   140,   855,   855,     0,     0,
       0,     0,   321,     0,   269,     0,   321,     0,     0,     0,
       0,     0,    61,     0,     0,   140,     0,    64,    65,    66,
      67,    68,    69,    70,   962,     0,   170,     0,     0,     0,
     170,     0,   170,     0,     0,     0,     0,     0,     0,   409,
     595,     0,     0,     0,     0,     0,   249,     0,     0,    61,
       0,   740,   170,   140,    64,    65,    66,    67,    68,    69,
      70,   954,     0,     0,   963,     0,     0,   751,     0,     0,
     754,     0,     0,   409,     0,     0,   735,  1243,     0,     0,
       0,     0,     0,   180,     0,  1243,     0,     0,     0,     0,
     249,     0,     0,   170,     0,     0,     0,   170,     0,   147,
       0,   955,     0,   439,   595,     0,   170,   764,     0,   614,
       0,     0,     0,     0,  1243,  1925,     0,   467,     0,     0,
       0,  1460,  1461,     0,     0,     0,   170,   508,   170,   170,
       0,     0,     0,     0,     0,  1475,  1476,     0,     0,     0,
       0,  1994,     0,     0,     0,     0,     0,   220,     0,     0,
       0,   170,     0,   249,     0,     0,   220,  2003,     0,   306,
     307,   308,   309,     0,     0,   418,  1019,     0,     0,  1497,
    1498,     0,  1274,   765,     0,   170,   296,  1243,   409,   409,
       0,     0,   296,   249,   321,   170,     0,     0,     0,   249,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
     168,   169,    64,    65,    66,    67,    68,    69,    70,   637,
     170,     0,   170,     0,     0,     0,     0,     0,   249,     0,
       0,     0,   296,     0,     0,     0,     0,     0,   170,     0,
       0,     0,     0,   296,   170,   296,     0,   321,     0,    82,
       0,     0,     0,     0,   170,     0,   855,   855,  2010,   310,
       0,     0,     0,     0,   321,   439,     0,   614,  2017,     0,
     855,   855,     0,     0,     0,   605,     0,   311,     0,   605,
     243,     6,     7,     8,     9,    10,    11,    12,   321,     0,
       0,     0,     0,   638,     0,     0,   236,     0,   614,     0,
       0,   321,     0,     0,   855,   855,     0,     0,   639,   147,
       0,   640,   641,    64,    65,    66,    67,    68,    69,    70,
       0,     0,   409,  1375,   147,   147,     0,   409,     0,     0,
       0,     0,     0,   409,   298,     0,   147,   147,   147,     0,
      57,     0,     0,     0,     0,     0,     0,     0,    61,     0,
     249,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,     0,  1648,  1649,     0,   467,     0,     0,     0,     0,
       0,    61,     0,  1243,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   740,   740,     0,     0,     0,     0,     0,
       0,     0,   439,  1009,    61,     0,  1012,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   735,   735,
       0,     0,     0,     0,     0,  1205,   409,     0,     0,  1273,
      74,  1376,    72,     0,   249,     0,     0,     0,     0,     0,
       0,  1472,     0,   439,     0,     0,   764,     0,   764,     0,
       0,    57,  1519,    74,   249,     0,     0,     0,     0,  1520,
       0,     0,     0,    77,    78,   321,   321,   508,     0,     0,
       0,  1080,     0,     0,   249,  1084,     0,     0,     0,  1728,
       0,     0,    61,     0,   321,     0,   296,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,   855,   855,     0,
    1525,   467,     0,     0,     0,   296,     0,     0,   249,  1746,
      72,     0,     0,   114,  1749,  1750,   114,    61,     0,     0,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
      73,    74,   249,  1684,     0,     0,     0,     0,     0,   249,
       0,    77,    78,   409,     0,     0,     0,     0,     0,     0,
     321,     0,     0,     0,     0,     0,   147,   409,     0,     0,
       0,     0,     0,     0,     0,   321,     0,  1189,     0,     0,
     114,     0,   249,   269,     0,     0,     0,   467,   605,     0,
       0,     0,  1243,     0,     0,     0,     0,  1243,  1243,  1243,
       0,     0,    61,     0,   114,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   855,     0,     0,     0,     0,     0,
     251,     0,     0,     0,   114,     0,     0,     0,   439,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,     0,     0,   855,     0,     0,     0,    61,   855,
     855,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   114,     0,     0,     0,     0,     0,   114,     0,   114,
     740,     0,     0,   251,     0,  1353,     0,     0,     0,     0,
       0,     0,     0,   318,   114,   349,     0,     0,     0,     0,
       0,  1525,     0,    57,     0,   735,     0,  1673,    61,  1525,
       0,   413,     0,    64,    65,    66,    67,    68,    69,    70,
       0,     0,   764,   114,   413,     0,     0,   251,     0,   764,
       0,  1355,     0,   193,    61,     0,    72,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,  1306,     0,     0,  1310,     0,  1018,    74,     0,     0,
     602,     0,    72,     0,     0,     0,     0,    77,    78,     0,
       0,   321,     0,     0,   114,     0,     0,   114,     0,     0,
       0,     0,   219,    74,     0,     0,   249,  1243,     0,  1243,
     251,     0,     0,    77,    78,     0,     0,   249,   193,     0,
       0,     0,     0,     0,     0,     0,     0,   548,     0,     0,
       0,   147,     0,   193,     0,   114,     0,     0,   249,   409,
     251,     0,     0,     0,     0,     0,   251,     0,     0,     0,
     193,     0,     0,     0,   114,     0,  1965,    13,    14,    15,
      16,    17,     0,   442,     0,     0,     0,     0,   409,     0,
       0,     0,     0,   114,     0,   251,   114,     0,     0,     0,
    1775,     0,     0,  1525,     0,   245,    82,     0,     0,     0,
     114,     0,     0,     0,   114,     0,     0,     0,     0,     0,
     296,     0,     0,     0,     0,     0,   147,     0,     0,     0,
       0,     0,     0,   439,     0,    57,   193,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,   413,     0,     0,
       0,     0,     0,     0,   298,     0,     0,     0,     0,     0,
     439,     0,     0,     0,   147,     0,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,  1458,
       0,   413,     0,     0,     0,     0,     0,     0,  1467,     0,
       0,     0,     0,   193,    72,     0,     0,     0,     0,    57,
       0,   855,     0,     0,  1525,     0,     0,   114,     0,     0,
       0,   413,   193,     0,   762,    74,     0,   251,   602,     0,
       0,     0,     0,     0,     0,    77,   763,   321,   321,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,    72,     0,
       0,     0,     0,     0,     0,     0,   147,   147,   147,   147,
     147,   147,     0,    72,     0,     0,  1521,   302,  1921,    74,
      57,     0,   498,     0,     0,     0,   413,   413,     0,    77,
      78,   251,   114,  1921,    74,   409,   409,   498,   298,     0,
     193,     0,     0,     0,    77,    78,     0,     0,     0,     0,
       0,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   114,     0,   245,     0,     0,   114,     0,
     193,   251,   114,     0,   114,     0,   249,     0,     0,    72,
       0,     0,     0,     0,   439,   114,     0,   114,     0,   570,
     298,    13,    14,    15,    16,    17,     0,     0,     0,   219,
      74,   349,   114,   413,     0,   251,     0,   147,   249,    61,
      77,    78,     0,     0,    64,    65,    66,    67,    68,    69,
      70,  1240,   298,     0,     0,  1241,   114,  1242,     0,   251,
       0,     0,     0,   548,     0,     0,   251,     0,     0,   114,
       0,   908,     0,     0,     0,   193,   193,   114,     0,    57,
       0,   442,     0,     0,     0,     0,     0,     0,    74,     0,
     413,  1625,   114,   114,     0,   413,     0,     0,  1661,     0,
       0,   413,     0,     0,   114,   114,   114,     0,    57,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,  1658,     0,     0,     0,  1521,     0,   409,
       0,     0,     0,  1521,   193,  1521,     0,     0,    72,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,   442,     0,     0,     0,     0,     0,   295,    74,
     413,     0,     0,   302,   147,     0,     0,    72,     0,    77,
      78,     0,     0,     0,     0,   193,     0,     0,   249,     0,
       0,     0,     0,     0,   413,     0,     0,  1921,    74,     0,
       0,   498,     0,     0,   409,     0,   193,     0,    77,    78,
       0,   413,     0,     0,     0,   321,    61,     0,   147,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,   114,   114,     0,   249,     0,     0,     0,
      98,   147,     0,   151,    72,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,  1661,  1661,     0,     0,
       0,     0,     0,     0,   762,    74,   321,   321,   602,     0,
       0,     0,     0,     0,     0,    77,   763,     0,     0,     0,
     114,  1658,  1658,     0,     0,     0,     0,     0,   604,   442,
       0,     0,     0,     0,     0,     0,  1521,    98,     0,  1521,
       0,     0,     0,   251,     0,     0,     0,     0,     0,     0,
       0,   413,     0,   193,   251,     0,   302,     0,   114,     0,
       0,   195,     0,    57,   114,   413,     0,   409,     0,     0,
     442,     0,     0,   114,     0,  1191,   413,     0,   114,     0,
       0,   258,     0,     0,     0,     0,     0,     0,     0,     0,
     296,     0,   442,   442,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,  1661,     0,     0,     0,
       0,   442,     0,     0,     0,     0,     0,     0,   288,     0,
       0,     0,    72,     0,    98,     0,   413,     0,     0,     0,
       0,  1658,     0,     0,     0,     0,     0,     0,     0,     0,
    1521,   323,   295,    74,   249,     0,     0,     0,     0,     0,
       0,     0,     0,    77,    78,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,   108,     0,     0,   147,     0,
     288,   445,  1896,     0,     0,     0,  1661,   442,     0,     0,
       0,     0,     0,  1350,   193,     0,     0,     0,     0,   114,
       0,     0,     0,   321,     0,     0,     0,     0,     0,   492,
       0,  1658,     0,     0,     0,     0,   114,   114,     0,  1661,
       0,   147,     0,     0,     0,   512,     0,     0,     0,     0,
     517,   519,   108,     0,   195,     0,     0,     0,     0,     0,
       0,   147,   147,    61,  1922,   302,   168,   169,    64,    65,
      66,    67,    68,    69,    70,   193,   539,     0,     0,   541,
       0,   542,    13,    14,    15,    16,    17,     0,     0,   114,
    1661,  1661,   559,     0,     0,     0,   259,     0,     0,   147,
       0,     0,     0,    61,     0,   571,   345,   346,    64,    65,
      66,    67,    68,    69,    70,  1922,  1922,   249,     0,     0,
       0,     0,  1357,  1661,     0,     0,     0,     0,     0,   114,
     593,     0,     0,   617,     0,     0,     0,   413,     0,   108,
      57,     0,     0,     0,     0,     0,     0,   624,  1922,     0,
       0,   624,     0,     0,    75,     0,   327,     0,     0,   347,
       0,     0,     0,     0,     0,     0,   413,     0,     0,     0,
       0,    61,     0,     0,     0,   657,    64,    65,    66,    67,
      68,    69,    70,   251,   114,    61,   446,     0,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,     0,    72,
       0,     0,     0,     0,   114,     0,     0,     0,   442,     0,
       0,   413,     0,     0,     0,  1191,     0,     0,     0,    73,
      74,     0,     0,     0,     0,     0,     0,  1418,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,   413,     0,
       0,   578,   114,     0,   288,     0,    61,     0,   593,   217,
     218,    64,    65,    66,    67,    68,    69,    70,    61,     0,
       0,   540,     0,    64,    65,    66,    67,    68,    69,    70,
    1240,   657,     0,    61,  1241,  1586,  1242,   108,    64,    65,
      66,    67,    68,    69,    70,  1240,   114,   114,     0,  1241,
       0,  1242,     0,     0,     0,     0,   249,     0,     0,     0,
     114,   114,  1202,     0,     0,   114,   114,    74,     0,     0,
    1627,     0,     0,     0,     0,   594,     0,     0,   259,     0,
       0,     0,    74,   193,     0,     0,     0,     0,     0,   445,
     193,     0,   594,     0,   114,   114,   594,     0,     0,     0,
       0,     0,     0,     0,   114,   114,   114,   114,   114,   114,
       0,     0,     0,     0,     0,   251,     0,   193,     0,     0,
     851,     0,     0,     0,     0,   519,     0,     0,     0,   862,
      57,   559,     0,   413,   413,     0,     0,     0,     0,     0,
       0,     0,   323,     0,    98,     0,    61,     0,  1839,   545,
     546,    64,    65,    66,    67,    68,    69,    70,     0,   624,
     883,    61,     0,   251,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,   894,     0,     0,     0,     0,     0,
       0,     0,   413,   593,   442,   442,     0,     0,   903,    72,
       0,     0,     0,   594,     0,   360,   624,    75,  1689,   361,
       0,   362,     0,     0,     0,   114,     0,  1693,     0,  1519,
      74,     0,     0,     0,     0,     0,     0,     0,   363,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,  1722,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,   446,     0,    72,   114,   114,   678,
       0,     0,    75,   376,     0,     0,     0,   445,     0,     0,
       0,     0,     0,     0,     0,     0,   375,   413,     0,    75,
     376,   193,     0,     0,  1001,   327,   377,    77,    78,   378,
     379,   380,   381,   114,   259,    61,   108,     0,   543,    63,
      64,    65,    66,    67,    68,    69,    70,   446,   883,   108,
      61,   251,   114,  1025,     0,    64,    65,    66,    67,    68,
      69,    70,  1778,     0,   594,   446,  1788,     0,     0,     0,
     445,   445,     0,     0,     0,     0,     0,     0,  1796,     0,
       0,     0,   413,     0,  1802,     0,     0,   992,   594,   445,
       0,     0,     0,   114,     0,     0,   114,     0,  1273,    74,
       0,   594,    61,     0,   114,   189,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,   851,     0,   114,
       0,     0,    61,     0,   113,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   114,     0,   193,     0,     0,   114,
     114,     0,     0,     0,   114,   114,     0,     0,  1161,     0,
       0,    74,     0,     0,   784,   445,     0,     0,     0,  1848,
       0,   151,     0,  1853,     0,  1855,     0,     0,     0,     0,
     624,     0,     0,  1193,     0,   851,     0,     0,     0,     0,
    1199,   113,     0,     0,     0,  1880,     0,     0,     0,     0,
       0,     0,   446,     0,   251,     0,     0,     0,     0,   671,
       0,     0,     0,     0,     0,   413,     0,     0,     0,     0,
      61,     0,   193,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,   323,     0,   261,  1907,     0,     0,     0,
    1910,     0,     0,   446,     0,     0,     0,     0,     0,  1916,
       0,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   327,   327,     0,   456,  1934,
       0,  1936,  1937,   442,   442,     0,     0,     0,   113,    72,
       0,     0,     0,     0,   327,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1946,   331,   851,     0,     0,  1519,
      74,     0,     0,     0,     0,     0,  1520,     0,     0,     0,
      77,    78,   327,   851,   851,     0,   114,     0,  1961,     0,
       0,     0,     0,     0,     0,   448,     0,     0,  1966,     0,
      13,    14,    15,    16,    17,     0,     0,     0,     0,     0,
       0,   114,     0,   108,     0,     0,     0,     0,   831,   833,
     327,     0,     0,  1989,     0,  1966,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   594,   445,     0,   259,     0,
     327,  2000,     0,     0,     0,     0,     0,  1989,     0,   114,
     114,     0,     0,   251,     0,     0,     0,  2008,    57,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,  1333,     0,     0,     0,
       0,     0,     0,     0,  1161,     0,   113,   114,   446,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,   719,   720,   721,   722,   723,   724,   725,   726,   727,
     728,   729,     0,  1161,     0,   204,     0,    72,     0,     0,
       0,   114,   119,     0,   596,     0,     0,   261,     0,     0,
     442,  1381,     0,     0,     0,     0,     0,  1519,    74,     0,
       0,   596,   730,     0,     0,   596,   119,     0,    77,    78,
       0,   327,   671,     0,     0,     0,     0,   671,   593,     0,
       0,     0,     0,   671,     0,     0,   119,   517,   327,   327,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   671,     0,     0,   323,     0,     0,     0,     0,
       0,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   119,     0,     0,     0,     0,     0,   119,
       0,   119,     0,     0,     0,     0,     0,     0,   989,    72,
      61,   327,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,   851,   851,     0,     0,     0,     0,   219,
      74,     0,   596,   119,     0,     0,     0,   851,   851,     0,
      77,    78,   445,   445,    61,   119,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,   108,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   851,   851,     0,     0,     0,     0,     0,     0,     0,
       0,  1333,  1333,  1333,   151,     0,     0,     0,   108,     0,
       0,     0,   460,     0,     0,     0,   119,     0,     0,   119,
       0,     0,     0,     0,   119,     0,   259,     0,     0,     0,
    1549,  1549,     0,   448,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,   594,     0,     0,     0,   119,     0,     0,
       0,     0,    72,     0,   331,     0,     0,     0,     0,     0,
       0,     0,     0,   261,     0,   113,   119,     0,     0,   323,
     446,     0,   295,    74,     0,     0,   448,     0,   113,     0,
       0,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,   151,   596,   448,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   596,   327,   327,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     596,     0,   327,   327,     0,     0,     0,   327,   327,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   123,
       0,     0,     0,     0,   851,   851,   327,   327,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1675,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     851,     0,     0,     0,     0,   108,   108,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,  1692,
       0,   448,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1549,
       0,     0,     0,     0,   446,     0,     0,   123,     0,     0,
     323,     0,   448,   151,     0,     0,     0,     0,     0,     0,
       0,   851,     0,     0,     0,     0,     0,     0,   119,   119,
       0,     0,     0,     0,   331,   331,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     123,   851,   123,   331,     0,     0,   851,   851,     0,     0,
       0,   445,   445,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,   119,     0,   119,  1767,     0,     0,
       0,   331,     0,     0,   123,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   327,
     327,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,   331,
       0,     0,  1549,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   596,   327,     0,   261,     0,   331,
       0,     0,     0,     0,     0,     0,     0,   123,     0,   119,
     123,     0,     0,     0,   259,   123,     0,     0,     0,     0,
       0,     0,   119,     0,   119,   119,     0,   119,     0,     0,
       0,     0,     0,   119,     0,     0,   119,   119,   119,     0,
       0,     0,     0,     0,   108,     0,     0,   448,   123,     0,
       0,     0,     0,     0,     0,   327,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   327,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1871,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   327,     0,     0,     0,
       0,   327,   327,     0,     0,     0,   327,   327,   445,     0,
     331,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1549,   331,   331,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,  1549,  1871,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1540,     0,     0,  1543,  1554,   108,     0,     0,
       0,  1560,     0,     0,   123,  1564,     0,  1566,     0,     0,
     331,     0,     0,     0,  1549,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,  1954,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   851,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   594,     0,
       0,     0,     0,     0,     0,   261,     0,     0,     0,   123,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   327,     0,     0,     0,     0,     0,     0,
       0,     0,   596,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,     0,     0,  1652,     0,     0,     0,     0,
       0,   123,     0,     0,     0,   123,     0,   123,     0,   448,
       0,   108,   594,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,  1685,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1691,
       0,     1,     0,     0,   145,     0,  1694,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,   331,   331,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,   331,     0,  1543,     0,   331,   331,     0,     0,
     123,     0,     0,   327,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,   123,   123,     0,   123,     0,
       0,     0,     0,     0,   123,   331,   331,   123,   123,   123,
       0,     0,     0,     0,     0,     0,     0,   192,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   203,     0,
       0,     0,     0,     0,   214,   215,     0,     0,     0,     0,
       0,     0,     0,     0,   113,   113,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,   277,   119,
       0,     0,     0,     0,     0,     0,   283,     0,     0,     0,
       0,     0,  1786,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,   448,  1793,  1795,     0,  1554,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   520,  1857,     0,   331,   331,
       0,     0,     0,     0,     0,   283,     0,     0,     0,     0,
       0,     0,     0,     0,   123,   283,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,   123,   551,
     555,     0,     0,     0,   331,     0,   562,   563,     0,     0,
       0,     0,     0,     0,   567,     0,     0,     0,     0,     0,
       0,     0,   573,   261,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1913,     0,  1915,     0,     0,     0,
       0,   591,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,     0,   119,   119,   119,   119,
     119,   119,     0,     0,   331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   331,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   677,     0,     0,
       0,     0,     0,     0,     0,   331,     0,     0,     0,  1962,
     331,   331,     0,     0,     0,   331,   331,     0,     0,     0,
       0,  1970,  1972,  1973,     0,     0,     0,     0,   718,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   756,     0,     0,   119,   759,     0,
     760,     0,   761,     0,     0,     0,     0,     0,     0,     0,
       0,   777,   778,     0,     0,     0,   113,   781,     0,     0,
       0,   782,   783,     0,     0,   786,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     800,   801,   802,   803,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   825,
       0,     0,     0,     0,     0,     0,     0,   828,     0,     0,
       0,   165,   123,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,   283,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,   596,     0,   861,
       0,     0,     0,     0,   119,     0,   866,   123,     0,     0,
       0,     0,     0,   551,     0,     0,     0,     0,     0,   872,
       0,     0,   331,   165,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,   119,     0,   165,     0,   165,     0,
     113,     0,   886,   891,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,   596,     0,     0,     0,   123,     0,     0,   351,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   351,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,   934,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,   165,     0,     0,
     165,   165,   331,     0,   165,     0,     0,   165,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,   123,   123,
     123,   123,   123,     0,     0,   997,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1014,     0,     0,     0,  1015,     0,   123,   123,     0,   165,
       0,     0,   165,   886,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1040,     0,     0,
       0,     0,     0,   165,     0,  1055,     0,     0,     0,     0,
       0,     0,     0,     0,  1064,     0,     0,     0,   165,     0,
    1067,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1108,  1109,     0,     0,     0,     0,
       0,   119,     0,     0,     0,  1167,  1168,  1169,     0,     0,
    1171,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,   165,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,   119,
     123,     0,    45,    46,     0,  1216,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,   351,     0,     0,
    1236,     0,     0,     0,     0,   123,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   656,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1257,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,  1264,     0,
       0,     0,  1265,     0,     0,     0,     0,     0,     0,   886,
       0,     0,   123,     0,     0,     0,     0,     0,   -16,  1278,
       0,     0,     0,   351,     0,   387,  1279,     0,     0,     0,
       0,     0,     0,  1281,     0,  1283,     0,  1284,     0,     0,
       0,     0,  1285,  1286,  1287,  1288,     0,     0,     0,     0,
    1293,  1294,     0,     0,     0,     0,     0,     0,     0,     0,
    1302,     0,     0,   165,   165,     0,     0,     0,     0,  1312,
       0,     0,     0,  1313,     0,     0,   165,     0,     0,     0,
       0,  1316,     0,  1317,     0,     0,     0,   145,     0,     0,
       1,     0,    13,    14,    15,    16,    17,     0,   123,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,     0,  1373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1387,     0,     0,     0,     0,     0,     0,  1391,
       0,  1393,  1395,     0,     0,     0,     0,     0,  1400,     0,
    1401,   656,  1402,     0,  1403,     0,  1405,     0,     0,     0,
     247,  1413,   165,   165,     0,     0,     0,     0,   165,     0,
       0,   268,  1423,   271,     0,   273,     0,   649,     0,     0,
     387,   654,     0,     0,     0,     0,     0,     0,     0,   165,
     660,   661,   165,   165,     0,   165,     0,   165,   165,     0,
       0,     0,     0,     0,     0,   387,   387,     0,     0,     0,
       0,     0,   123,   247,     0,   271,   273,     0,     0,     0,
       0,  1455,     0,     0,     0,     0,   387,     0,  1462,  1463,
       0,     0,   123,     0,     0,     0,   165,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1486,     0,     0,     0,   387,   247,     0,  1491,
       0,     0,     0,  1492,     0,  1495,     0,     0,     0,  1496,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   214,     0,     0,     0,     0,     0,  1531,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
     247,     0,   271,   273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1581,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1591,
     247,     0,  1594,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1600,  1602,     0,     0,
       0,     0,     0,     0,  1605,     0,  1607,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,   620,     0,   273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1632,     0,     0,
       0,     0,  1633,  1634,     0,     0,  1637,     0,     0,     0,
    1638,     0,     0,     0,     0,     0,     0,  1639,  1640,     0,
    1641,     0,     0,     0,  1642,  1643,     0,     0,   165,  1645,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1650,
    1651,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,   165,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   620,
     273,     0,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,  1725,  1726,     0,     0,
       0,     0,     0,     0,     0,  1729,  1730,     0,     0,     0,
       0,     0,     0,     0,   247,  1736,     0,     0,     0,   247,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,   247,   247,     0,     0,     0,     0,     0,
    1759,  1760,     0,     0,     0,     0,     0,     0,     0,   165,
       0,   247,     0,     0,     0,     0,     0,   165,   165,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,   620,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1804,     0,     0,     0,     0,   247,
     620,     0,     0,     0,   165,     0,   247,     0,     0,     0,
    1817,     0,     0,   165,     0,     0,   165,  1594,   165,   165,
       0,     0,     0,     0,     0,     0,     0,     0,  1826,     0,
       0,  1827,  1828,     0,     0,  1829,     0,     0,  1830,   247,
     268,     0,     0,     0,   338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,  1847,     0,     0,     0,     0,   387,     0,     0,
       0,     0,   387,   435,   338,     0,     0,     0,     0,     0,
       0,  1867,     0,   387,  1868,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1920,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     163,     0,     0,   212,     0,  1939,     0,     0,     0,     0,
     501,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1951,     0,   338,   606,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   290,   627,  1968,   291,     0,     0,     0,
       0,     0,  1974,   247,     0,     0,     0,   165,     0,   312,
       0,     0,   275,     0,   247,     0,     0,  1987,     0,     0,
       0,     0,   387,     0,     0,   281,     0,   282,     0,     0,
       0,     0,     0,   165,     0,   247,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,   477,   501,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     501,   752,     0,   501,   755,     0,     0,     0,     0,     0,
       0,   338,     0,     0,     0,   606,   387,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,     0,   528,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   502,
     503,     0,     0,   507,     0,     0,   510,   511,     0,   171,
     387,   387,   387,     0,     0,     0,   501,   387,   387,     0,
     501,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,   574,     0,     0,     0,
       0,   387,     0,   577,   579,     0,     0,     0,   586,     0,
       0,     0,   338,     0,     0,     0,     0,   247,   165,   165,
       0,     0,     0,     0,     0,     0,   351,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,   387,   387,
     631,     0,     0,     0,     0,   312,     0,     0,   312,     0,
       0,     0,   589,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   501,     0,     0,   338,     0,   621,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   881,   338,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   606,     0,     0,     0,   606,     0,     0,
       0,     0,     0,     0,   899,     0,   338,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   212,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   779,   780,     0,
       0,   200,     0,   247,     0,     0,     0,     0,     0,     0,
       0,   750,     0,     0,     0,     0,     0,   255,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,   165,     0,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,     0,     0,     0,     0,     0,   200,     0,     0,     0,
     303,     0,     0,     0,     0,     0,   501,   501,   821,     0,
       0,   343,     0,     0,     0,     0,   501,  1010,     0,   501,
    1013,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   338,     0,     0,   606,     0,   606,   606,     0,     0,
       0,   455,     0,   606,   459,     0,     0,     0,     0,     0,
       0,     0,     0,   338,   338,   165,     0,     0,   312,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   338,     0,     0,     0,   501,     0,     0,     0,
     501,     0,     0,     0,   501,  1081,     0,     0,   501,  1085,
       0,     0,     0,     0,   200,     0,  1088,     0,     0,     0,
       0,     0,     0,     0,     0,   247,   631,   255,     0,     0,
       0,     0,   387,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   897,   898,   360,     0,     0,     0,   361,     0,
     362,     0,     0,     0,     0,   905,     0,     0,   338,   501,
       0,     0,     0,   459,     0,     0,     0,   363,     0,     0,
       0,   200,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   606,     0,     0,     0,
     599,     0,   616,     0,   364,   365,     0,   462,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,   338,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   675,   375,    74,     0,   463,   464,
       0,     0,     0,   465,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,     0,     0,     0,  1042,
       0,  1003,  1004,     0,     0,     0,  1054,  1008,   200,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   501,     0,     0,     0,     0,  1029,     0,
       0,  1032,  1033,     0,  1036,     0,  1038,  1039,   599,     0,
     606,   606,     0,     0,   776,     0,     0,   606,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,  1079,     0,     0,     0,  1083,
       0,  1112,     0,     0,     0,     0,     0,     0,   387,   338,
       0,     0,     0,     0,   501,  1307,     0,   501,  1311,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,   200,     0,     0,     0,     0,   455,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1206,     0,     0,     0,     0,   631,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1200,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   343,     0,     0,     0,   387,     0,   387,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     455,     0,   885,     0,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,     0,   387,     0,     0,     0,     0,
       0,   338,     0,   599,   247,     0,     0,   606,  1409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
       0,     0,     0,     0,   200,     0,     0,     0,   338,     0,
       0,     0,     0,     0,     0,     0,     0,   675,     0,   675,
     675,     0,   675,     0,     0,     0,     0,     0,   675,     0,
     411,   675,   675,   675,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   440,   387,     0,     0,     0,     0,     0,
       0,     0,   501,  1459,     0,     0,   468,  1200,   468,   247,
       0,   501,  1468,     0,   606,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   338,   338,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1298,     0,
       0,     0,     0,     0,     0,  1305,     0,     0,  1309,     0,
    1198,   200,     0,  1354,  1356,  1358,     0,    13,    14,    15,
      16,    17,     0,     0,     0,     0,     0,     0,   455,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1377,   568,     0,     0,     0,     0,     0,
     455,   455,     0,   360,     0,     0,     0,   361,  1112,   362,
       0,     0,     0,   247,     0,     0,     0,     0,     0,   455,
       0,     0,     0,     0,     0,    57,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   338,     0,     0,   631,     0,     0,     0,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,  1407,     0,
       0,     0,     0,     0,    72,   455,  1416,  1417,     0,     0,
       0,     0,   200,     0,     0,     0,   247,     0,     0,     0,
       0,     0,   776,     0,   375,     0,     0,    75,   376,     0,
       0,     0,     0,     0,   377,   438,    78,   378,   379,   380,
     381,     0,  1411,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1457,     0,     0,     0,     0,     0,     0,
       0,   501,  1466,   343,     0,  1470,     0,  1473,  1474,     0,
       0,     0,     0,     0,     0,   360,   468,   501,     0,   361,
       0,   362,   468,     0,     0,     0,     0,   797,     0,  1530,
       0,     0,  1532,     0,     0,     0,     0,    57,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1500,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,   338,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   865,  1588,   375,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,  1412,    78,   378,
     379,   380,   381,     0,   338,   338,   455,     0,     0,     0,
       0,     0,   440,     0,     0,     0,     0,     0,     0,   501,
     501,     0,     0,     0,     0,   893,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   675,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1470,     0,     0,
       0,     0,     0,     0,     0,     0,   928,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1681,     0,     0,     0,     0,  1647,   797,   948,     0,
     255,   950,     0,   952,     0,     0,     0,     0,     0,   961,
       0,   966,   961,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,     0,     0,   599,   501,
       0,     0,     0,     0,     0,     0,     0,   501,     0,   994,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   996,     0,     0,   343,     0,     0,     0,   675,
       0,     0,     0,  1005,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   631,     0,     0,     0,   440,     0,     0,
     994,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   338,     0,  1723,     0,   501,  1897,     0,     0,   501,
       0,     0,     0,     0,     0,     0,     0,  1058,     0,     0,
     468,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   455,   455,     0,     0,     0,     0,     0,     0,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1089,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   675,   675,   675,     0,   675,   675,  1771,  1772,     0,
       0,     0,   459,     0,     0,     0,     0,     0,     0,  1776,
       0,     0,     0,   501,   501,     0,     0,     0,  1815,     0,
       0,     0,     0,     0,   411,     0,     0,   631,     0,     0,
       0,     0,     0,     0,  1190,  1192,     0,     0,     0,     0,
       0,     0,   440,     0,     0,     0,   501,     0,     0,     0,
     255,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   468,     0,     0,     0,     0,     0,   343,
       0,     0,   961,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   994,     0,     0,     0,     0,
       0,     0,     0,  1229,     0,  1325,     0,     0,     0,     0,
     961,  1326,  1837,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,   468,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,  1327,     0,     0,  1895,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   468,     0,  1297,     0,  1300,   255,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1328,     0,     0,     0,    75,   924,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
     343,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1365,  1365,     0,   675,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   455,   455,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1721,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1349,  1404,     0,     0,     0,     0,     0,
    1414,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   255,     0,     0,     0,     0,     0,   440,     0,     0,
       0,     0,     0,     0,   360,     0,     0,     0,   361,     0,
     362,     0,     0,     0,   468,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1114,     0,   363,    -2,   961,
    1116,     0,   797,  1117,  1118,  1119,  1120,  1121,  1122,  1123,
    1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,  1131,  1132,
    1133,     0,  1134,     0,   364,   365,     0,   462,     0,   367,
    1135,  1136,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,  1137,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,  1494,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   675,     0,   375,     0,     0,    75,   376,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,   961,     0,     0,     0,     0,     0,   455,     0,
    -179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     468,     0,     0,   797,     0,     0,     0,     0,     0,     0,
       0,     0,  1988,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   675,  1349,     0,
     459,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   948,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1603,  1604,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,   361,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   468,     0,   797,     0,
    1114,     0,   363,    -2,     0,  1116,  -238,  -238,  1117,  1118,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    -301,  1129,  1130,  1131,  1132,  1133,     0,  1134,     0,   364,
     365,     0,   462,     0,   367,  1135,  1136,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,  1137,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   411,     0,     0,  -238,
     375,  1674,     0,    75,   376,     0,     0,     0,   279,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     0,  1988,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1349,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1714,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   360,     0,     0,     0,   361,     0,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1114,  1737,   363,    -2,  1739,
    1116,  -239,  -239,  1117,  1118,  1119,  1120,  1121,  1122,  1123,
    1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,  1131,  1132,
    1133,     0,  1134,     0,   364,   365,     0,   462,     0,   367,
    1135,  1136,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,  1137,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -239,   375,     0,     0,    75,   376,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,     0,     0,     0,     0,
    -179,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1113,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,  1114,    57,  1115,
      -2,     0,  1116,     0,     0,  1117,  1118,  1119,  1120,  1121,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,
    1131,  1132,  1133,   961,  1134,     0,   364,   365,    60,   462,
       0,   367,  1135,  1136,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,  1137,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -3,   375,     0,     0,
      75,   407,     0,     0,     0,   279,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,     0,  -179,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,  1113,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   360,     0,
      45,    46,   361,     0,   362,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,  1114,
      57,  1115,    -2,     0,  1116,     0,     0,  1117,  1118,  1119,
    1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  -301,
    1129,  1130,  1131,  1132,  1133,     0,  1134,     0,   364,   365,
      60,   462,     0,   367,  1135,  1136,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,  1137,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   407,     0,     0,     0,   279,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
       0,     0,     0,     0,  -179,     4,   243,     6,     7,     8,
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
       0,   375,     0,  1551,    75,   407,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,     0,     0,     0,  1552,  1553,     4,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   360,     0,    45,    46,   361,     0,   362,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,   365,    60,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   407,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,     0,     0,     0,  1552,  1553,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   360,     0,    45,    46,   361,     0,
     362,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,   365,    60,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,  1542,    75,   407,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,     4,   243,     6,     7,     8,     9,    10,    11,
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
       0,    75,   407,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   360,
       0,    45,    46,   361,     0,   362,   319,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,    75,   437,     0,     0,     0,     0,     0,
     377,   438,    78,   378,   379,   380,   381,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
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
       0,     0,   375,     0,     0,    75,  1187,     0,     0,     0,
       0,     0,   377,  1188,    78,   378,   379,   380,   381,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   360,     0,    45,    46,   361,     0,   362,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   360,     0,    45,    46,   361,
       0,   362,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     437,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,  1846,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,     0,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,  1866,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,
      -2,     0,     0,    -2,     0,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,    -2,    -2,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    59,     0,
       0,     0,    60,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -422,  -422,     0,
    -422,    45,    46,     0,  -422,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,    61,    45,    46,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,   244,     0,     0,     0,  -731,     0,
       0,    77,    78,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,     0,     0,     0,     0,  -354,  -354,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -354,     0,
       0,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,     4,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,     0,    57,
       0,     0,     0,     0,  -355,  -355,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -355,     0,     0,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -422,  -422,     0,  -422,    45,    46,
       0,  -422,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,   244,     0,     0,  1325,     0,     0,     0,    77,    78,
    1326,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,  1327,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1506,
       0,     0,     0,    75,   924,     0,     0,  1325,     0,     0,
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
       0,     0,  1507,     0,     0,     0,    75,   924,     0,     0,
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
       0,     0,     0,     0,     0,  1508,     0,     0,     0,    75,
     924,     0,     0,     0,     0,     0,     0,    77,    78,   242,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -422,  -422,     0,  -422,    45,    46,     0,  -422,
       0,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,    19,    57,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -421,  -421,     0,  -421,    45,    46,     0,  -421,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   242,     0,     0,     0,     0,     0,    75,   244,
       0,    13,    14,    15,    16,    17,    77,    78,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -422,  -422,     0,  -422,    45,
      46,     0,  -422,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,    19,    57,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -422,  -422,     0,  -422,    45,
      46,     0,  -422,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   301,     0,     0,     0,     0,     0,     0,    77,
      78,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -422,  -422,     0,  -422,    45,    46,     0,
    -422,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     244,     0,     0,     0,  -735,     0,     0,    77,    78,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -422,  -422,     0,  -422,    45,    46,     0,  -422,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,  1349,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,   360,    75,   244,     0,
     361,     0,   362,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1114,     0,   363,
       0,     0,  1116,  1779,  1780,  1117,  1118,  1119,  1120,  1121,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,
    1131,  1132,  1133,     0,  1134,     0,   364,   365,     0,   462,
       0,   367,  1135,  1136,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,  1137,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,  1349,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,     0,   279,     0,   377,    77,    78,
     378,   379,   380,   381,   360,     0,     0,     0,   361,     0,
     362,     0,  -179,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1114,     0,   363,     0,     0,
    1116,     0,     0,  1117,  1118,  1119,  1120,  1121,  1122,  1123,
    1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,  1131,  1132,
    1133,     0,  1134,     0,   364,   365,     0,   462,     0,   367,
    1135,  1136,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,  1137,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   376,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,     0,     0,     0,     0,
    -179,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,  1049,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -603,    75,
     320,     0,     0,    62,    63,     0,     0,    77,    78,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    75,     0,     0,     0,    45,    46,     0,     0,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,  1755,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   320,     0,
       0,    62,    63,     0,     0,    77,    78,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    75,
       0,     0,     0,    45,    46,     0,     0,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,  1757,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   320,     0,     0,     0,
       0,     0,     0,    77,    78,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   320,     0,     0,     0,     0,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   301,     0,     0,     0,     0,     0,     0,    77,
      78,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -422,  -422,     0,  -422,    45,    46,     0,
    -422,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     244,     0,     0,     0,     0,     0,     0,    77,    78,    13,
      14,    15,    16,    17,    18,   662,    19,   663,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   360,     0,    45,    46,   361,
       0,   362,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     664,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     665,     0,     0,     0,   279,     0,   377,    77,    78,   666,
     667,   380,   381,    13,    14,    15,    16,    17,    18,     0,
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
     375,     0,   406,    75,   407,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,   375,     0,     0,    75,   665,     0,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   360,     0,    45,
      46,   361,     0,   362,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   407,     0,     0,     0,     0,     0,   377,    77,
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
       0,     0,   375,     0,     0,    75,   437,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   360,     0,    45,    46,   361,
       0,   362,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,    76,     0,     0,     0,  -733,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -422,  -422,     0,  -422,    45,    46,     0,  -422,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,   301,     0,     0,
       0,     0,     0,     0,    77,    78,   556,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,    62,    63,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,    75,     0,    45,    46,    62,
      63,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,  1426,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   930,    75,   924,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     924,     0,     0,     0,     0,     0,     0,    77,    78,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     286,     0,     0,    62,    63,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,    62,    63,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   433,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   320,     0,     0,     0,     0,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   286,     0,     0,    62,    63,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   433,     0,     0,     0,
       0,     0,     0,    77,    78,   242,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -422,  -422,
       0,  -422,    45,    46,     0,  -422,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,    75,     0,    45,    46,    62,    63,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   301,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   924,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   924,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,    13,    14,    15,    16,    17,    77,    78,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -422,  -422,     0,
    -422,    45,    46,     0,  -422,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,    57,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -422,  -422,     0,
    -422,    45,    46,     0,  -422,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   301,    62,    63,     0,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     850,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -616,    75,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1682,
       0,     0,     0,     0,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
      75,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    62,    63,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -422,  -422,     0,  -422,    45,    46,     0,
    -422,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,    75,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -422,  -422,    75,  -422,    45,
      46,     0,  -422,     0,     0,   360,     0,     0,     0,   361,
       0,   362,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,   364,   365,     0,   366,     0,
     367,  1791,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,   360,     0,    72,     0,   361,     0,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,   375,   363,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,     0,     0,     0,
    1792,  -179,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,  1232,     0,    75,   376,
       0,     0,     0,  1233,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,   957,  1533,
      75,   376,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   376,     0,     0,     0,   465,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   375,   796,     0,    75,   376,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,   279,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,   957,     0,    75,   376,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,   988,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
    1299,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,  1359,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   375,     0,  1785,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,  1971,     0,    75,   376,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   648,
       0,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   653,     0,     0,    75,   376,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   659,     0,     0,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   376,
       0,     0,     0,     0,     0,   377,   864,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,     0,     0,     0,   377,   438,    78,
     378,   379,   380,   381
};

static const yytype_int16 yycheck[] =
{
       1,   162,     1,     4,     1,   173,    73,    73,    73,   691,
     873,   633,   162,     1,   219,   219,    75,   220,   241,   150,
     283,   178,   614,   375,   408,   256,   219,   669,    73,   601,
     601,   601,   676,  1720,    58,   139,   219,  1657,   177,   605,
      95,   219,   207,   465,   219,   514,   515,   162,  1206,  1657,
     764,   323,  1783,   219,    55,    56,     1,    58,    70,    58,
     735,    58,    82,   222,   859,  1779,     1,   762,    73,   762,
      58,   843,    73,     4,  1123,  1657,     1,    73,   178,   765,
     764,    82,   366,    82,   762,   771,  1658,   191,   762,    90,
     295,   295,    73,   296,    95,     0,    95,    98,   870,    98,
      70,   102,   295,   102,   131,   236,     1,  1283,  1284,   130,
      98,     1,   295,    58,   181,   181,   963,   295,   525,    73,
     295,   145,    96,    58,  1351,   163,   338,   147,   535,   295,
     342,   290,   291,    58,   149,   192,   181,   149,   165,   140,
       0,   153,   143,   157,   145,   992,   145,   229,   145,   150,
      73,   172,   219,   219,   219,   156,  1107,   145,   173,   630,
     174,   352,   163,    87,   355,   220,   635,   321,    58,   576,
      70,   102,   254,   445,   219,   149,   181,   102,   462,   180,
     181,   180,   264,   153,   338,   181,     1,  1901,   342,     4,
     762,   762,   762,   174,   195,  1767,   195,  1107,   764,    87,
     145,   375,  1241,     0,   205,     1,   321,   607,  1055,   210,
     145,  1942,    70,  1107,   219,   274,   120,  1097,   219,   220,
     145,   220,  1102,   219,   131,   245,   115,   181,   295,   295,
     295,     1,   149,   157,     4,   236,   859,   228,  1969,   921,
     231,   296,   573,    58,   245,   782,   245,   631,   152,   149,
     295,   174,   155,   153,   255,   145,   149,   258,   165,   258,
    1418,   156,   253,   800,   265,   219,   497,   949,    95,   157,
     258,   174,   263,   742,   275,   276,  1963,   278,   483,   483,
      19,     1,   302,    98,     4,   439,   283,   102,    58,   492,
     483,   149,   914,  1173,   295,   296,   938,   296,  1870,   562,
     483,  1921,   303,   781,   782,   483,   471,    82,   483,   310,
     311,  1025,    82,  1921,   315,    87,   787,   483,  1351,  1352,
    1115,   593,   800,  1018,   439,  1018,  1001,  1099,   894,   866,
     145,   435,   102,  1019,   146,   323,   149,   689,    58,  1921,
    1018,  1025,   149,    70,  1018,   617,   347,   174,   283,  1144,
    1922,   352,   624,  1580,   355,   356,   265,   115,   283,   155,
     156,   173,    82,   316,   569,   569,   766,   276,   571,   139,
     770,  1322,  1323,  1324,   149,   145,   569,   147,   590,   779,
     780,   512,  1954,    59,    60,   157,   569,   518,   866,   409,
     149,   569,   155,   283,   569,   153,   149,   104,   105,     1,
      87,   354,     4,   569,   463,   443,  1445,  1446,  1447,   179,
     154,   174,  1322,  1323,  1324,   627,   483,   483,   419,   139,
     419,   191,   149,   573,   155,   145,   153,   147,  1322,  1323,
    1324,   149,   157,   840,   165,   153,   590,   492,   483,  1319,
      70,   442,   443,   258,   915,   152,  1018,  1018,  1018,   174,
     781,   782,   534,   454,   455,   174,    58,   445,   573,  1025,
     614,    70,   463,   520,   465,   131,  1642,  1643,   155,   800,
     157,   191,   242,   627,   648,   245,   650,   651,   483,   653,
     250,   157,   483,   104,   105,   659,   155,   483,   662,   663,
     664,   492,  1115,   492,   160,   161,   155,     4,   174,   614,
     102,  1904,   569,   569,  1553,   174,   165,   155,  1557,  1195,
     149,   512,    70,   283,   505,  1918,   571,   518,   157,   149,
     155,  1144,   242,   153,   569,   245,   174,    70,   155,   483,
    1067,   174,   302,   155,   591,   866,   527,    75,    76,   174,
     149,  1944,   533,   145,   153,   131,   537,  1580,    55,    56,
      56,   575,   174,    59,    60,   556,    62,   558,    12,    13,
      14,    15,    16,   283,     3,   562,    75,    76,   569,    73,
     571,   559,   571,   995,   575,     3,   575,   716,   575,   851,
    1202,    70,   302,    90,   585,    89,  1272,   575,   589,  1067,
     155,   149,   160,  1440,   591,   153,   149,  1189,   151,   167,
     168,    82,   106,   934,   146,   375,   149,  1251,    73,   174,
     153,   883,   152,    70,    95,   157,    70,    98,   775,   157,
     621,   102,    70,    88,   789,   211,   155,   562,   146,   617,
     575,   173,   633,   140,   152,   560,   143,   562,    70,   409,
     575,    70,  1042,   149,   155,   174,   149,   155,   157,   156,
     575,  1700,  1515,  1829,   157,   173,   163,   157,   717,  1341,
     149,   718,   155,    70,   153,   435,   174,   149,   173,   881,
    1703,   802,   562,    70,   156,   775,    70,   131,   192,   149,
     681,   174,   683,   822,   685,   575,   786,   148,   689,   409,
     821,   692,   149,    70,   155,   153,   153,   150,   284,   180,
     158,   149,   759,   210,   157,   153,   157,   363,    12,    13,
      14,    15,    16,  1182,   195,   435,   717,   149,  1362,   157,
     155,   153,   157,   148,   781,   782,   764,   881,  1351,  1352,
     155,   171,   388,   389,    70,   153,  1067,   149,  1209,   220,
     158,  1790,   149,   800,   514,   515,   153,   912,   255,  1798,
     125,   126,   149,   409,   149,   149,   153,   149,   265,   153,
     575,   762,   759,   764,   245,   151,    70,   115,   275,   276,
     156,   278,   149,   104,   105,   776,   153,   258,  1625,   151,
    1627,   151,   783,   439,   934,   155,   556,  1258,   789,   375,
     560,   792,   562,  1437,   169,   170,   303,   696,   697,   698,
     801,   802,   803,   310,   311,   575,   151,   321,   315,   866,
     324,  1233,  1861,   149,  1847,   129,   151,   153,   151,   934,
     821,   156,  1206,  1054,   338,   151,   151,   131,   342,    12,
      13,    14,    15,    16,  1867,   149,   556,   162,   163,   153,
     347,   151,   562,   153,   129,   352,   160,   161,   355,   151,
     151,   842,   151,   152,   156,   575,   857,   858,   859,   173,
     151,   151,   859,   851,   149,   635,   156,   905,   153,   152,
     153,   859,  1905,   156,   862,   160,   161,   155,   648,   149,
     650,   651,   149,   653,   151,  1216,   153,    70,   988,   659,
     162,   163,   662,   663,   664,   883,   149,  1062,    12,    13,
      14,    15,    16,   149,   905,   151,    21,   153,   909,    46,
      47,   151,    49,   914,   859,   155,    53,   149,   504,   920,
     149,  1193,   155,   509,   859,   439,   149,  1086,   151,   129,
     153,   155,   151,   155,   859,   442,   155,  1359,   419,    56,
     526,   872,    59,    60,  1415,    62,   129,   454,   455,   149,
     536,   155,  1283,   153,   955,   151,    70,  1580,    96,   155,
     160,   161,   963,   151,  1064,  1436,   149,   155,   149,   859,
     153,   149,   742,   575,  1326,  1198,   129,   160,   161,   149,
     151,   151,    82,   153,   155,   151,  1024,  1025,  1610,   155,
     157,   992,   157,   151,   995,   103,   149,   155,    87,   107,
     153,   149,   110,   148,   112,   153,   520,   160,   161,   173,
    1067,   492,   154,  1435,   157,  1183,  1184,  1018,   108,   109,
     110,   111,   112,  1024,  1025,     4,     5,     6,     7,     8,
       9,    10,    11,   151,  1418,  1189,   149,   155,   157,   695,
     153,   143,   144,   145,   859,  1176,  1220,   147,   149,   556,
     157,   149,   153,   155,  1055,   153,   151,   872,   151,   573,
     155,  1926,   648,   165,   151,  1930,  1216,   653,   155,  1400,
     151,   151,   174,   659,  1189,   155,   590,   591,   585,   179,
    1703,   151,   589,  1314,     3,   155,   151,  1107,  1351,   859,
     155,  1562,   678,    12,    13,    14,    15,    16,     3,   157,
     614,  1216,   872,   873,  1506,  1507,  1508,    12,    13,    14,
      15,    16,  1734,   627,   621,   115,  1117,   149,  1115,  1120,
    1121,  1122,   155,  1284,   151,   149,   633,  1115,   155,  1332,
    1812,   166,   151,   151,  1291,  1292,   155,   155,   151,   859,
     248,   149,   155,  1144,   161,   245,   151,  1144,  1619,  1150,
     155,    70,   872,   159,  1254,   151,  1144,  1158,   129,   155,
    1161,  1162,  1161,  1162,  1165,    70,   152,   143,   144,   145,
    1115,   151,   151,  1161,   681,  1176,   683,   151,   685,   155,
    1115,   155,   689,   151,   151,   692,   171,   155,   155,   165,
    1115,  1291,  1292,   151,   151,   174,   151,   155,   174,  1144,
     155,  1202,   302,   151,   718,  1193,  1104,  1105,  1106,  1144,
     717,   703,   704,   705,   706,   151,  1217,   325,   326,  1144,
     328,   321,   330,   151,  1847,  1115,  1283,  1284,   123,   124,
     151,  1162,  1233,   127,   128,   160,   161,  1162,   154,   155,
    1241,   827,   154,   155,  1867,   759,   154,   155,   154,   155,
    1400,   153,   838,   154,  1144,   841,   156,   859,   118,   845,
     120,   121,   122,   155,  1735,   154,   155,   154,   155,   776,
     872,   156,  1273,   131,   930,   131,   783,  1332,   151,   935,
     155,   156,  1905,   154,   155,  1400,   154,   155,   149,   149,
     946,   154,   152,   153,   801,   149,   803,   157,   158,   151,
    1115,   151,  1322,  1323,  1324,   151,  1326,  1327,   151,   409,
    1513,  1642,   154,   155,  1519,  1519,   151,  1580,  1521,    12,
      13,    14,    15,    16,    17,   153,  1519,   154,   155,  1144,
      68,  1332,   154,   155,   157,  1336,  1519,  1107,  1339,   439,
     157,  1519,   154,   155,  1519,  1115,  1161,  1162,   157,  1820,
     857,   858,   859,  1519,  1351,  1352,   154,   155,  1359,   154,
     155,   154,   155,  1351,  1352,   154,   155,   881,   154,   155,
     884,   154,   155,   157,  1144,   154,   155,   154,  1379,   149,
    1381,    76,  1381,   155,   156,   154,   155,  1107,   154,   155,
      75,    76,  1162,  1381,  1325,  1115,   155,   156,   905,  1242,
    1243,   154,   909,   699,   700,  1526,  1527,   914,   701,   702,
     707,   708,  1182,    17,   514,   515,  1351,  1352,  1183,  1184,
     934,   173,   155,   531,  1144,   157,  1351,  1352,   149,   174,
    1591,   151,   151,   157,  1435,   174,   154,   154,    17,  1440,
    1579,  1591,   157,  1617,  1445,  1446,  1447,   148,   151,   151,
    1220,   151,   151,   151,  1519,   151,   148,   151,  1513,  1115,
      17,  1351,  1352,   151,   174,   151,  1521,  1672,  1672,    68,
     157,    73,  1675,   157,  1531,   157,  1591,   151,   151,  1672,
     151,   148,  1643,    12,    13,    14,    15,    16,    17,  1672,
     173,   157,  1423,    95,  1672,   151,   151,  1672,    55,    56,
      57,    58,    59,    60,    61,    62,  1672,   155,   151,   151,
    1325,   155,  1513,  1115,   614,  1101,   151,   151,  1519,   151,
    1521,    12,    13,    14,    15,    16,    17,  1528,  1114,   151,
     151,   151,   151,  1189,   151,   635,  1351,  1352,   154,   154,
     151,  1542,  1144,   151,   151,  1131,   151,   148,   150,   173,
    1551,   151,  1322,  1323,  1324,  1325,  1326,  1327,   151,   154,
    1162,  1549,   151,   151,   151,   148,  1381,  1223,  1224,  1225,
     151,   155,   149,  1776,  1230,  1231,  1715,   205,   149,   149,
     149,  1351,  1352,  1580,   149,  1642,  1643,  1588,   149,    13,
     156,    72,  1580,   155,   174,  1726,    89,   156,   174,  1871,
     154,   154,  1322,  1323,  1324,  1325,  1326,  1327,  1423,  1610,
    1117,   174,   148,  1120,  1121,  1122,   174,   219,   220,  1550,
    1675,   148,  1779,   157,  1625,  1550,  1627,   155,   174,   151,
     154,  1351,  1352,   151,   236,   155,   155,  1144,   151,   155,
     154,   151,   742,  1150,   151,  1580,   148,   148,   174,   149,
    1804,  1158,   149,  1423,   149,  1580,    78,   174,  1165,   174,
     148,   151,   174,  1594,   149,   174,   174,   154,  1829,  1808,
     174,  1672,   148,   148,  1675,  1189,   174,   155,   174,  1779,
    1161,  1162,   155,   154,   154,  1686,   151,   154,   157,  1690,
    1580,   148,   151,   295,   296,  1202,   156,   118,   156,   148,
     151,  1702,  1216,  1423,  1692,   151,  1703,  1708,   151,   151,
    1217,   151,   174,   154,  1868,  1703,  1921,  1921,   154,  1922,
     148,  1776,   156,  1325,  1725,  1726,   155,   107,  1921,   155,
     149,   151,   149,  1734,  1549,  1550,   151,   149,  1921,  1870,
     154,   154,  1899,  1921,  1901,  1515,  1921,   154,   148,  1351,
    1352,  1954,   157,   101,   148,  1921,  1342,  1343,   106,   107,
     108,   109,   110,   111,   112,  1580,   151,   151,  1703,   151,
     151,  1928,  1829,   873,   151,  1776,   154,   151,  1703,  1594,
    1550,    73,  1783,   551,  1804,    73,  1787,  1926,  1789,   148,
     151,  1930,  1931,   149,  1380,   174,   151,    88,   174,  1899,
     154,  1901,   150,   174,   154,   153,   148,   148,  1809,   153,
    1580,   151,   151,  1703,   151,   151,   151,  1956,   151,    73,
     152,  1423,   174,    73,  1594,   174,   156,   174,  1928,  1336,
     151,   148,  1339,   151,   148,   463,   151,   465,   151,  1978,
    1997,   443,   150,  1982,   155,   150,   148,  1617,  1868,  1850,
    1847,   149,  1952,  1854,  1921,  1921,  1921,   156,   101,  1847,
    1580,   155,  1863,    73,   149,  2004,   148,  1922,   165,  1870,
    1867,  1872,  1379,   174,  1594,   165,  1921,  1692,   154,  1867,
     107,   483,  1883,   107,  1885,  1886,  1400,   174,  1703,   151,
     492,    12,    13,    14,    15,    16,    17,  1997,   156,  1954,
    1381,   148,    12,    13,    14,    15,    16,  1908,  1905,   151,
     512,   148,  1847,   151,   149,     1,   518,  1905,     4,   174,
    1921,  1922,  1847,  1922,    73,   151,   151,   377,   174,  1585,
     174,  1932,  1867,  1703,  1617,   828,  1341,  1248,   668,   710,
     713,  1942,  1867,  1133,   709,   712,  1144,   711,  1550,  1580,
    1969,  1901,  1707,  1954,  1918,  1954,   558,  1847,  1703,  1793,
      70,  1964,  1813,  1963,  1951,  1572,  1967,   569,  1969,   571,
    1905,     9,    58,  1572,  1868,  1931,  1982,  1867,  1580,  1165,
    1905,  1867,    48,  1703,  1985,  1571,   250,    73,  1513,  1776,
    1991,  1837,  1594,   886,  1527,  1327,    82,  1158,   789,   876,
    2001,  1423,  1594,   585,  2005,     0,   472,   920,   734,    95,
      -1,   734,    98,    -1,  2015,  1905,   102,    -1,   734,   129,
      -1,  1528,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,  1847,    -1,    -1,  1542,    -1,    -1,    -1,   149,
      -1,    -1,    -1,   153,  1551,    -1,    -1,    -1,    -1,    -1,
     160,   161,  1867,   139,    -1,    -1,    -1,    -1,    -1,   145,
     828,   147,    -1,   101,   150,   151,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   162,  1847,    -1,    -1,
      -1,  1588,  1182,    -1,    70,    -1,    -1,    -1,    -1,  1189,
    1905,    -1,    -1,   179,   180,   181,    -1,  1867,    -1,    -1,
      -1,  1703,    -1,  1610,   997,   191,   192,    -1,    -1,   195,
       4,     5,     6,     7,     8,     9,    10,    11,   886,   230,
      -1,  1014,  1015,    -1,    -1,    -1,    -1,  1847,    -1,    -1,
      -1,   101,    -1,   219,   220,  1905,   106,   107,   108,   109,
     110,   111,   112,   129,    -1,    -1,    -1,  1867,    -1,    -1,
     236,    -1,    -1,    -1,    -1,  1811,    -1,    -1,    -1,   245,
     762,   789,   764,   149,   792,    -1,    -1,   153,    -1,    63,
      -1,    -1,   258,    -1,   160,   161,    -1,    -1,    17,  1686,
      -1,   101,    -1,  1690,    -1,  1905,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,  1702,    -1,   117,    -1,   119,
     802,  1708,   288,    -1,    -1,    -1,    -1,    -1,   294,   295,
     296,    -1,    -1,    -1,    -1,    -1,   302,    -1,  1725,   821,
      59,    60,    61,    62,    -1,    -1,    -1,  1734,    -1,   997,
     150,    -1,    -1,   153,    -1,   321,   322,   323,    -1,    -1,
      -1,    -1,    -1,    98,    -1,  1847,  1014,  1015,    -1,    -1,
      -1,    -1,   338,    -1,   109,    -1,   342,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,  1867,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,  1783,    -1,    -1,    -1,
    1787,    -1,  1789,    -1,    -1,    -1,    -1,    -1,    -1,   375,
    1804,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   101,
      -1,   412,  1809,  1905,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,   153,    -1,    -1,   428,    -1,    -1,
     431,    -1,    -1,   409,    -1,    -1,   412,   955,    -1,    -1,
      -1,    -1,    -1,   419,    -1,   963,    -1,    -1,    -1,    -1,
     195,    -1,    -1,  1850,    -1,    -1,    -1,  1854,    -1,   435,
      -1,   153,    -1,   439,  1868,    -1,  1863,   443,    -1,   445,
      -1,    -1,    -1,    -1,   992,  1872,    -1,   995,    -1,    -1,
      -1,  1264,  1265,    -1,    -1,    -1,  1883,   488,  1885,  1886,
      -1,    -1,    -1,    -1,    -1,  1278,  1279,    -1,    -1,    -1,
      -1,  1977,    -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,
      -1,  1908,    -1,   258,    -1,    -1,   492,  1993,    -1,    63,
      64,    65,    66,    -1,    -1,  1515,  1018,    -1,    -1,  1312,
    1313,    -1,  1024,  1025,    -1,  1932,   512,  1055,   514,   515,
      -1,    -1,   518,   288,   520,  1942,    -1,    -1,    -1,   294,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    12,
    1967,    -1,  1969,    -1,    -1,    -1,    -1,    -1,   323,    -1,
      -1,    -1,   558,    -1,    -1,    -1,    -1,    -1,  1985,    -1,
      -1,    -1,    -1,   569,  1991,   571,    -1,   573,    -1,   575,
      -1,    -1,    -1,    -1,  2001,    -1,  1264,  1265,  2005,   153,
      -1,    -1,    -1,    -1,   590,   591,    -1,   593,  2015,    -1,
    1278,  1279,    -1,    -1,    -1,   601,    -1,   171,    -1,   605,
       4,     5,     6,     7,     8,     9,    10,    11,   614,    -1,
      -1,    -1,    -1,    86,    -1,    -1,     3,    -1,   624,    -1,
      -1,   627,    -1,    -1,  1312,  1313,    -1,    -1,   101,   635,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,   648,    76,   650,   651,    -1,   653,    -1,    -1,
      -1,    -1,    -1,   659,  1176,    -1,   662,   663,   664,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
     445,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,  1495,  1496,    -1,  1233,    -1,    -1,    -1,    -1,
      -1,   101,    -1,  1241,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   734,   735,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   718,   744,   101,    -1,   747,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   734,   735,
      -1,    -1,    -1,    -1,    -1,     9,   742,    -1,    -1,   149,
     150,   174,   129,    -1,   519,    -1,    -1,    -1,    -1,    -1,
      -1,  1273,    -1,   759,    -1,    -1,   762,    -1,   764,    -1,
      -1,    70,   149,   150,   539,    -1,    -1,    -1,    -1,   156,
      -1,    -1,    -1,   160,   161,   781,   782,   808,    -1,    -1,
      -1,   812,    -1,    -1,   559,   816,    -1,    -1,    -1,  1602,
      -1,    -1,   101,    -1,   800,    -1,   802,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,  1495,  1496,    -1,
    1332,  1359,    -1,    -1,    -1,   821,    -1,    -1,   593,  1632,
     129,    -1,    -1,     1,  1637,  1638,     4,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     149,   150,   617,  1531,    -1,    -1,    -1,    -1,    -1,   624,
      -1,   160,   161,   859,    -1,    -1,    -1,    -1,    -1,    -1,
     866,    -1,    -1,    -1,    -1,    -1,   872,   873,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   881,    -1,   883,    -1,    -1,
      58,    -1,   657,   658,    -1,    -1,    -1,  1435,   894,    -1,
      -1,    -1,  1440,    -1,    -1,    -1,    -1,  1445,  1446,  1447,
      -1,    -1,   101,    -1,    82,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1602,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,    -1,   102,    -1,    -1,    -1,   934,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    -1,    -1,  1632,    -1,    -1,    -1,   101,  1637,
    1638,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   139,    -1,    -1,    -1,    -1,    -1,   145,    -1,   147,
    1001,    -1,    -1,   151,    -1,   174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,    -1,    -1,    -1,    -1,
      -1,  1513,    -1,    70,    -1,  1001,    -1,  1519,   101,  1521,
      -1,   179,    -1,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,  1018,   191,   192,    -1,    -1,   195,    -1,  1025,
      -1,   174,    -1,    82,   101,    -1,   129,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,  1072,    -1,    -1,  1075,    -1,   149,   150,    -1,    -1,
     153,    -1,   129,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,  1067,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,
      -1,    -1,   149,   150,    -1,    -1,   851,  1625,    -1,  1627,
     258,    -1,    -1,   160,   161,    -1,    -1,   862,   147,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   275,    -1,    -1,
      -1,  1107,    -1,   162,    -1,   283,    -1,    -1,   883,  1115,
     288,    -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,
     179,    -1,    -1,    -1,   302,    -1,  1939,    12,    13,    14,
      15,    16,    -1,   192,    -1,    -1,    -1,    -1,  1144,    -1,
      -1,    -1,    -1,   321,    -1,   323,   324,    -1,    -1,    -1,
    1672,    -1,    -1,  1675,    -1,  1161,  1162,    -1,    -1,    -1,
     338,    -1,    -1,    -1,   342,    -1,    -1,    -1,    -1,    -1,
    1176,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,
      -1,    -1,    -1,  1189,    -1,    70,   245,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,   375,    -1,    -1,
      -1,    -1,    -1,    -1,  1726,    -1,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,    -1,  1220,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,  1260,
      -1,   409,    -1,    -1,    -1,    -1,    -1,    -1,  1269,    -1,
      -1,    -1,    -1,   302,   129,    -1,    -1,    -1,    -1,    70,
      -1,  1939,    -1,    -1,  1776,    -1,    -1,   435,    -1,    -1,
      -1,   439,   321,    -1,   149,   150,    -1,   445,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,  1283,  1284,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1322,  1323,  1324,  1325,
    1326,  1327,    -1,   129,    -1,    -1,  1332,  1333,   149,   150,
      70,    -1,   153,    -1,    -1,    -1,   514,   515,    -1,   160,
     161,   519,   520,   149,   150,  1351,  1352,   153,  1870,    -1,
     409,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   551,    -1,  1381,    -1,    -1,   556,    -1,
     439,   559,   560,    -1,   562,    -1,  1161,    -1,    -1,   129,
      -1,    -1,    -1,    -1,  1400,   573,    -1,   575,    -1,  1921,
    1922,    12,    13,    14,    15,    16,    -1,    -1,    -1,   149,
     150,   589,   590,   591,    -1,   593,    -1,  1423,  1193,   101,
     160,   161,    -1,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,  1954,    -1,    -1,   117,   614,   119,    -1,   617,
      -1,    -1,    -1,   621,    -1,    -1,   624,    -1,    -1,   627,
      -1,   629,    -1,    -1,    -1,   514,   515,   635,    -1,    70,
      -1,   520,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
     648,   153,   650,   651,    -1,   653,    -1,    -1,  1509,    -1,
      -1,   659,    -1,    -1,   662,   663,   664,    -1,    70,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,  1509,    -1,    -1,    -1,  1513,    -1,  1515,
      -1,    -1,    -1,  1519,   573,  1521,    -1,    -1,   129,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   591,    -1,    -1,    -1,    -1,    -1,   149,   150,
     718,    -1,    -1,  1549,  1550,    -1,    -1,   129,    -1,   160,
     161,    -1,    -1,    -1,    -1,   614,    -1,    -1,  1333,    -1,
      -1,    -1,    -1,    -1,   742,    -1,    -1,   149,   150,    -1,
      -1,   153,    -1,    -1,  1580,    -1,   635,    -1,   160,   161,
      -1,   759,    -1,    -1,    -1,  1591,   101,    -1,  1594,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,   781,   782,    -1,  1381,    -1,    -1,    -1,
       1,  1617,    -1,     4,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   800,    -1,    -1,    -1,  1657,  1658,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,  1642,  1643,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
     828,  1657,  1658,    -1,    -1,    -1,    -1,    -1,   173,   718,
      -1,    -1,    -1,    -1,    -1,    -1,  1672,    58,    -1,  1675,
      -1,    -1,    -1,   851,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   859,    -1,   742,   862,    -1,  1692,    -1,   866,    -1,
      -1,    82,    -1,    70,   872,   873,    -1,  1703,    -1,    -1,
     759,    -1,    -1,   881,    -1,   883,   884,    -1,   886,    -1,
      -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1726,    -1,   781,   782,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1767,    -1,    -1,    -1,
      -1,   800,    -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,
      -1,    -1,   129,    -1,   145,    -1,   934,    -1,    -1,    -1,
      -1,  1767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1776,   162,   149,   150,  1549,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,  1804,    -1,
     191,   192,  1833,    -1,    -1,    -1,  1837,   866,    -1,    -1,
      -1,    -1,    -1,  1117,   873,    -1,    -1,    -1,    -1,   997,
      -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,    -1,   220,
      -1,  1837,    -1,    -1,    -1,    -1,  1014,  1015,    -1,  1870,
      -1,  1847,    -1,    -1,    -1,   236,    -1,    -1,    -1,    -1,
     241,   242,    58,    -1,   245,    -1,    -1,    -1,    -1,    -1,
      -1,  1867,  1868,   101,  1870,  1871,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   934,   267,    -1,    -1,   270,
      -1,   272,    12,    13,    14,    15,    16,    -1,    -1,  1067,
    1921,  1922,   283,    -1,    -1,    -1,   102,    -1,    -1,  1905,
      -1,    -1,    -1,   101,    -1,   296,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1921,  1922,  1692,    -1,    -1,
      -1,    -1,   160,  1954,    -1,    -1,    -1,    -1,    -1,  1107,
     321,    -1,    -1,   324,    -1,    -1,    -1,  1115,    -1,   145,
      70,    -1,    -1,    -1,    -1,    -1,    -1,   338,  1954,    -1,
      -1,   342,    -1,    -1,   152,    -1,   162,    -1,    -1,   157,
      -1,    -1,    -1,    -1,    -1,    -1,  1144,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   366,   106,   107,   108,   109,
     110,   111,   112,  1161,  1162,   101,   192,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   129,
      -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,  1067,    -1,
      -1,  1189,    -1,    -1,    -1,  1193,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,  1216,    -1,
      -1,   157,  1220,    -1,   435,    -1,   101,    -1,   439,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   101,    -1,
      -1,   267,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   462,    -1,   101,   117,  1379,   119,   283,   106,   107,
     108,   109,   110,   111,   112,   113,  1264,  1265,    -1,   117,
      -1,   119,    -1,    -1,    -1,    -1,  1871,    -1,    -1,    -1,
    1278,  1279,   157,    -1,    -1,  1283,  1284,   150,    -1,    -1,
     153,    -1,    -1,    -1,    -1,   321,    -1,    -1,   324,    -1,
      -1,    -1,   150,  1182,    -1,    -1,    -1,    -1,    -1,   520,
    1189,    -1,   338,    -1,  1312,  1313,   342,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1322,  1323,  1324,  1325,  1326,  1327,
      -1,    -1,    -1,    -1,    -1,  1333,    -1,  1216,    -1,    -1,
     551,    -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,   560,
      70,   562,    -1,  1351,  1352,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   573,    -1,   575,    -1,   101,    -1,     1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   590,
     591,   101,    -1,  1381,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   605,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1400,   614,  1283,  1284,    -1,    -1,   619,   129,
      -1,    -1,    -1,   439,    -1,    48,   627,   152,  1542,    52,
      -1,    54,    -1,    -1,    -1,  1423,    -1,  1551,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,  1588,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,   520,    -1,   129,  1495,  1496,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,   718,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,  1515,    -1,   152,
     153,  1400,    -1,    -1,   735,   551,   159,   160,   161,   162,
     163,   164,   165,  1531,   560,   101,   562,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   573,   759,   575,
     101,  1549,  1550,   764,    -1,   106,   107,   108,   109,   110,
     111,   112,  1686,    -1,   590,   591,  1690,    -1,    -1,    -1,
     781,   782,    -1,    -1,    -1,    -1,    -1,    -1,  1702,    -1,
      -1,    -1,  1580,    -1,  1708,    -1,    -1,   153,   614,   800,
      -1,    -1,    -1,  1591,    -1,    -1,  1594,    -1,   149,   150,
      -1,   627,   101,    -1,  1602,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   828,    -1,  1617,
      -1,    -1,   101,    -1,     1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1632,    -1,  1515,    -1,    -1,  1637,
    1638,    -1,    -1,    -1,  1642,  1643,    -1,    -1,   859,    -1,
      -1,   150,    -1,    -1,   153,   866,    -1,    -1,    -1,  1783,
      -1,   872,    -1,  1787,    -1,  1789,    -1,    -1,    -1,    -1,
     881,    -1,    -1,   884,    -1,   886,    -1,    -1,    -1,    -1,
     891,    58,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,   718,    -1,  1692,    -1,    -1,    -1,    -1,   375,
      -1,    -1,    -1,    -1,    -1,  1703,    -1,    -1,    -1,    -1,
     101,    -1,  1591,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   934,    -1,   102,  1850,    -1,    -1,    -1,
    1854,    -1,    -1,   759,    -1,    -1,    -1,    -1,    -1,  1863,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   781,   782,    -1,   149,  1883,
      -1,  1885,  1886,  1642,  1643,    -1,    -1,    -1,   145,   129,
      -1,    -1,    -1,    -1,   800,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1908,   162,   997,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,
     160,   161,   828,  1014,  1015,    -1,  1804,    -1,  1932,    -1,
      -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,  1942,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,  1829,    -1,   859,    -1,    -1,    -1,    -1,   514,   515,
     866,    -1,    -1,  1967,    -1,  1969,    -1,    -1,    -1,  1847,
      -1,    -1,    -1,    -1,    -1,   881,  1067,    -1,   884,    -1,
     886,  1985,    -1,    -1,    -1,    -1,    -1,  1991,    -1,  1867,
    1868,    -1,    -1,  1871,    -1,    -1,    -1,  2001,    70,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1115,    -1,   283,  1905,   934,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,    -1,  1144,    -1,   146,    -1,   129,    -1,    -1,
      -1,  1939,    58,    -1,   321,    -1,    -1,   324,    -1,    -1,
    1829,  1162,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,   338,   173,    -1,    -1,   342,    82,    -1,   160,   161,
      -1,   997,   648,    -1,    -1,    -1,    -1,   653,  1189,    -1,
      -1,    -1,    -1,   659,    -1,    -1,   102,  1198,  1014,  1015,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   678,    -1,    -1,  1216,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   139,    -1,    -1,    -1,    -1,    -1,   145,
      -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,   714,   129,
     101,  1067,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,  1264,  1265,    -1,    -1,    -1,    -1,   149,
     150,    -1,   439,   179,    -1,    -1,    -1,  1278,  1279,    -1,
     160,   161,  1283,  1284,   101,   191,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,  1115,
      -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1312,  1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1322,  1323,  1324,  1325,    -1,    -1,    -1,  1144,    -1,
      -1,    -1,   149,    -1,    -1,    -1,   242,    -1,    -1,   245,
      -1,    -1,    -1,    -1,   250,    -1,  1162,    -1,    -1,    -1,
    1351,  1352,    -1,   520,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1189,    -1,    -1,    -1,   283,    -1,    -1,
      -1,    -1,   129,    -1,   551,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   560,    -1,   562,   302,    -1,    -1,  1400,
    1216,    -1,   149,   150,    -1,    -1,   573,    -1,   575,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1423,   590,   591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   614,  1264,  1265,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     627,    -1,  1278,  1279,    -1,    -1,    -1,  1283,  1284,   375,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,  1495,  1496,  1312,  1313,    -1,    -1,
      -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,
    1531,    -1,    -1,    -1,    -1,  1351,  1352,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,  1550,
      -1,   718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1580,
      -1,    -1,    -1,    -1,  1400,    -1,    -1,   102,    -1,    -1,
    1591,    -1,   759,  1594,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1602,    -1,    -1,    -1,    -1,    -1,    -1,   514,   515,
      -1,    -1,    -1,    -1,   781,   782,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,
     145,  1632,   147,   800,    -1,    -1,  1637,  1638,    -1,    -1,
      -1,  1642,  1643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     556,    -1,    -1,    -1,   560,    -1,   562,  1658,    -1,    -1,
      -1,   828,    -1,    -1,   179,    -1,    -1,    -1,    -1,   575,
      -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,  1495,
    1496,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,   866,
      -1,    -1,  1703,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   881,  1531,    -1,   884,    -1,   886,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   242,    -1,   635,
     245,    -1,    -1,    -1,  1550,   250,    -1,    -1,    -1,    -1,
      -1,    -1,   648,    -1,   650,   651,    -1,   653,    -1,    -1,
      -1,    -1,    -1,   659,    -1,    -1,   662,   663,   664,    -1,
      -1,    -1,    -1,    -1,  1580,    -1,    -1,   934,   283,    -1,
      -1,    -1,    -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1602,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1804,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1632,    -1,    -1,    -1,
      -1,  1637,  1638,    -1,    -1,    -1,  1642,  1643,  1829,    -1,
     997,    -1,    -1,    -1,    -1,    -1,   742,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1847,  1014,  1015,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     375,    -1,    -1,    -1,    -1,    -1,  1867,  1868,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1348,    -1,    -1,  1351,  1352,  1703,    -1,    -1,
      -1,  1357,    -1,    -1,   409,  1361,    -1,  1363,    -1,    -1,
    1067,    -1,    -1,    -1,  1905,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     435,  1922,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1939,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1115,    -1,
      -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   872,   873,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1804,    -1,
      -1,    -1,    -1,    -1,    -1,  1162,    -1,    -1,    -1,   514,
     515,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1847,    -1,    -1,    -1,  1501,    -1,    -1,    -1,    -1,
      -1,   556,    -1,    -1,    -1,   560,    -1,   562,    -1,  1216,
      -1,  1867,  1868,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     575,    -1,    -1,    -1,    -1,    -1,    -1,  1533,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1545,
      -1,     0,    -1,    -1,     3,    -1,  1552,    -1,    -1,  1905,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1264,  1265,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1278,  1279,    -1,  1580,    -1,  1283,  1284,    -1,    -1,
     635,    -1,    -1,  1939,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   648,    -1,   650,   651,    -1,   653,    -1,
      -1,    -1,    -1,    -1,   659,  1312,  1313,   662,   663,   664,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,
      -1,    -1,    -1,    -1,    92,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1351,  1352,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1107,    -1,    -1,    -1,    -1,    -1,    -1,   126,  1115,
      -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,    -1,
      -1,    -1,  1688,    -1,    -1,    -1,    -1,   742,    -1,    -1,
      -1,    -1,    -1,  1400,  1700,  1701,    -1,  1703,  1144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     229,    -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   244,  1792,    -1,  1495,  1496,
      -1,    -1,    -1,    -1,    -1,   254,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   859,   264,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   872,   873,   278,
     279,    -1,    -1,    -1,  1531,    -1,   285,   286,    -1,    -1,
      -1,    -1,    -1,    -1,   292,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   301,  1550,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1860,    -1,  1862,    -1,    -1,    -1,
      -1,   320,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1580,    -1,    -1,  1322,  1323,  1324,  1325,
    1326,  1327,    -1,    -1,  1591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1602,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1351,  1352,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   376,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1632,    -1,    -1,    -1,  1935,
    1637,  1638,    -1,    -1,    -1,  1642,  1643,    -1,    -1,    -1,
      -1,  1947,  1948,  1949,    -1,    -1,    -1,    -1,   407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   433,    -1,    -1,  1423,   437,    -1,
     438,    -1,   440,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   449,   450,    -1,    -1,    -1,  1703,   456,    -1,    -1,
      -1,   460,   461,    -1,    -1,   464,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     479,   480,   481,   482,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   498,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   506,    -1,    -1,
      -1,    47,  1107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1515,
      -1,    -1,    -1,    -1,    -1,   534,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1804,    -1,   557,
      -1,    -1,    -1,    -1,  1550,    -1,   565,  1162,    -1,    -1,
      -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,    -1,   578,
      -1,    -1,  1829,   119,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,    -1,    -1,  1580,    -1,   132,    -1,   134,    -1,
    1847,    -1,   601,   602,    -1,    -1,    -1,    -1,  1594,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1867,  1868,    -1,    -1,    -1,  1220,    -1,    -1,   164,    -1,
      -1,  1617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   665,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   219,    -1,    -1,    -1,   223,    -1,    -1,
     226,   227,  1939,    -1,   230,    -1,    -1,   233,   234,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1703,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1322,  1323,  1324,
    1325,  1326,  1327,    -1,    -1,   734,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     749,    -1,    -1,    -1,   753,    -1,  1351,  1352,    -1,   295,
      -1,    -1,   298,   762,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   775,    -1,    -1,
      -1,    -1,    -1,   319,    -1,   784,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,   334,    -1,
     799,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1423,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,    -1,    -1,
      -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   852,   853,    -1,    -1,    -1,    -1,
      -1,  1847,    -1,    -1,    -1,   863,   864,   865,    -1,    -1,
     868,   870,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1867,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,   428,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,  1905,
    1515,    -1,    50,    51,    -1,   924,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,   483,    -1,    -1,
     948,    -1,    -1,    -1,    -1,  1550,    -1,    -1,    -1,   495,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1580,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   994,    -1,    -1,  1594,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1007,    -1,
      -1,    -1,  1011,    -1,    -1,    -1,    -1,    -1,    -1,  1018,
      -1,    -1,  1617,    -1,    -1,    -1,    -1,    -1,   156,  1028,
      -1,    -1,    -1,   569,    -1,   178,  1035,    -1,    -1,    -1,
      -1,    -1,    -1,  1041,    -1,  1044,    -1,  1046,    -1,    -1,
      -1,    -1,  1050,  1051,  1052,  1053,    -1,    -1,    -1,    -1,
    1058,  1059,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1068,    -1,    -1,   609,   610,    -1,    -1,    -1,    -1,  1078,
      -1,    -1,    -1,  1082,    -1,    -1,   622,    -1,    -1,    -1,
      -1,  1089,    -1,  1091,    -1,    -1,    -1,  1096,    -1,    -1,
    1099,    -1,    12,    13,    14,    15,    16,    -1,  1703,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,  1144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1170,    -1,    -1,    -1,    -1,    -1,    -1,  1177,
      -1,  1179,  1180,    -1,    -1,    -1,    -1,    -1,  1187,    -1,
    1188,   101,  1190,    -1,  1192,    -1,  1194,    -1,    -1,    -1,
      98,  1199,   738,   739,    -1,    -1,    -1,    -1,   744,    -1,
      -1,   109,  1211,   111,    -1,   113,    -1,   360,    -1,    -1,
     363,   364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   765,
     373,   374,   768,   769,    -1,   771,    -1,   773,   774,    -1,
      -1,    -1,    -1,    -1,    -1,   388,   389,    -1,    -1,    -1,
      -1,    -1,  1847,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,  1259,    -1,    -1,    -1,    -1,   409,    -1,  1266,  1267,
      -1,    -1,  1867,    -1,    -1,    -1,   812,    -1,    -1,    -1,
     816,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1290,    -1,    -1,    -1,   439,   195,    -1,  1297,
      -1,    -1,    -1,  1301,    -1,  1304,    -1,    -1,    -1,  1308,
    1905,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1331,    -1,    -1,    -1,    -1,    -1,  1338,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   892,    -1,    -1,    -1,
     258,    -1,   260,   261,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1371,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1388,
     288,    -1,  1391,    -1,    -1,    -1,   294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1404,  1406,    -1,    -1,
      -1,    -1,    -1,    -1,  1412,    -1,  1414,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,
      -1,   329,    -1,   331,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1456,    -1,    -1,
      -1,    -1,  1460,  1461,    -1,    -1,  1465,    -1,    -1,    -1,
    1469,    -1,    -1,    -1,    -1,    -1,    -1,  1475,  1476,    -1,
    1478,    -1,    -1,    -1,  1483,  1484,    -1,    -1,  1024,  1487,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1497,
    1498,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1065,
      -1,    -1,    -1,    -1,    -1,    -1,  1072,    -1,    -1,  1075,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   445,    -1,   447,
     448,    -1,   695,   696,   697,   698,   699,   700,   701,   702,
     703,   704,   705,   706,   707,   708,   709,   710,   711,   712,
     713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   492,    -1,  1595,  1596,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1603,  1604,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   512,  1613,    -1,    -1,    -1,   517,
      -1,   519,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   539,    -1,   541,   542,    -1,    -1,    -1,    -1,    -1,
    1648,  1649,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1195,
      -1,   559,    -1,    -1,    -1,    -1,    -1,  1203,  1204,    -1,
      -1,    -1,    -1,   571,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   593,    -1,   595,   596,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1713,    -1,    -1,    -1,    -1,   617,
     618,    -1,    -1,    -1,  1260,    -1,   624,    -1,    -1,    -1,
    1728,    -1,    -1,  1269,    -1,    -1,  1272,  1736,  1274,  1275,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1746,    -1,
      -1,  1749,  1750,    -1,    -1,  1754,    -1,    -1,  1756,   657,
     658,    -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1315,
      -1,    -1,  1781,    -1,    -1,    -1,    -1,   930,    -1,    -1,
      -1,    -1,   935,   191,   192,    -1,    -1,    -1,    -1,    -1,
      -1,  1800,    -1,   946,  1803,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   223,    -1,    -1,    -1,    -1,
      -1,    -1,   230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   988,  1382,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1869,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      47,    -1,    -1,    90,    -1,  1894,    -1,    -1,    -1,    -1,
     298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1919,    -1,   321,   322,    -1,    -1,    -1,  1464,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   140,   342,  1943,   143,    -1,    -1,    -1,
      -1,    -1,  1950,   851,    -1,    -1,    -1,  1493,    -1,   156,
      -1,    -1,   119,    -1,   862,    -1,    -1,  1965,    -1,    -1,
      -1,    -1,  1115,    -1,    -1,   132,    -1,   134,    -1,    -1,
      -1,    -1,    -1,  1519,    -1,   883,    -1,    -1,    -1,  1525,
      -1,    -1,    -1,    -1,    -1,    -1,   894,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   903,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   210,   412,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     428,   429,    -1,   431,   432,    -1,    -1,    -1,    -1,    -1,
      -1,   439,    -1,    -1,    -1,   443,  1189,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1590,    -1,    -1,    -1,   255,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   265,   226,
     227,    -1,    -1,   230,    -1,    -1,   233,   234,    -1,   276,
    1223,  1224,  1225,    -1,    -1,    -1,   484,  1230,  1231,    -1,
     488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1001,    -1,    -1,   303,    -1,    -1,    -1,
      -1,  1254,    -1,   310,   311,    -1,    -1,    -1,   315,    -1,
      -1,    -1,   520,    -1,    -1,    -1,    -1,  1025,  1664,  1665,
      -1,    -1,    -1,    -1,    -1,    -1,  1672,    -1,    -1,    -1,
    1676,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1291,  1292,
     347,    -1,    -1,    -1,    -1,   352,    -1,    -1,   355,    -1,
      -1,    -1,   319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   570,    -1,    -1,   573,    -1,   334,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   590,   591,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   601,    -1,    -1,    -1,   605,    -1,    -1,
      -1,    -1,    -1,    -1,   612,    -1,   614,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1769,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   454,   455,    -1,
      -1,    82,    -1,  1161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   428,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1193,    -1,  1833,    -1,    -1,
      -1,  1199,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     718,    -1,    -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,
     151,    -1,    -1,    -1,    -1,    -1,   734,   735,   495,    -1,
      -1,   162,    -1,    -1,    -1,    -1,   744,   745,    -1,   747,
     748,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,   759,    -1,    -1,   762,    -1,   764,   765,    -1,    -1,
      -1,   192,    -1,   771,   195,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   781,   782,  1921,    -1,    -1,   585,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   800,    -1,    -1,    -1,   804,    -1,    -1,    -1,
     808,    -1,    -1,    -1,   812,   813,    -1,    -1,   816,   817,
      -1,    -1,    -1,    -1,   245,    -1,   824,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1333,   633,   258,    -1,    -1,
      -1,    -1,  1585,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   609,   610,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,   622,    -1,    -1,   866,   867,
      -1,    -1,    -1,   294,    -1,    -1,    -1,    71,    -1,    -1,
      -1,   302,    -1,  1381,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   894,    -1,    -1,    -1,
     321,    -1,   323,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,   934,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   375,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   776,
      -1,   738,   739,    -1,    -1,    -1,   783,   744,   409,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1001,    -1,    -1,    -1,    -1,   765,    -1,
      -1,   768,   769,    -1,   771,    -1,   773,   774,   439,    -1,
    1018,  1019,    -1,    -1,   445,    -1,    -1,  1025,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1779,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1549,    -1,    -1,    -1,   812,    -1,    -1,    -1,   816,
      -1,   858,    -1,    -1,    -1,    -1,    -1,    -1,  1811,  1067,
      -1,    -1,    -1,    -1,  1072,  1073,    -1,  1075,  1076,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   514,   515,    -1,    -1,    -1,    -1,   520,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   909,    -1,    -1,    -1,    -1,   914,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   892,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   573,    -1,    -1,    -1,  1899,    -1,  1901,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     591,    -1,   593,    -1,    -1,    -1,    -1,  1675,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1928,    -1,    -1,    -1,    -1,
      -1,  1189,    -1,   614,  1692,    -1,    -1,  1195,  1196,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1952,
      -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,  1216,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   648,    -1,   650,
     651,    -1,   653,    -1,    -1,    -1,    -1,    -1,   659,    -1,
     179,   662,   663,   664,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   192,  1997,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1260,  1261,    -1,    -1,   205,  1024,   207,  1767,
      -1,  1269,  1270,    -1,  1272,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1283,  1284,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1065,    -1,
      -1,    -1,    -1,    -1,    -1,  1072,    -1,    -1,  1075,    -1,
       5,   742,    -1,  1120,  1121,  1122,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,   759,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1150,   293,    -1,    -1,    -1,    -1,    -1,
     781,   782,    -1,    48,    -1,    -1,    -1,    52,  1165,    54,
      -1,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,   800,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1400,    -1,    -1,  1202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,  1195,    -1,
      -1,    -1,    -1,    -1,   129,   866,  1203,  1204,    -1,    -1,
      -1,    -1,   873,    -1,    -1,    -1,  1954,    -1,    -1,    -1,
      -1,    -1,   883,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1260,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1509,  1269,   934,    -1,  1272,    -1,  1274,  1275,    -1,
      -1,    -1,    -1,    -1,    -1,    48,   465,  1525,    -1,    52,
      -1,    54,   471,    -1,    -1,    -1,    -1,   476,    -1,  1336,
      -1,    -1,  1339,    -1,    -1,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1315,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,  1591,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   563,  1382,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,  1642,  1643,  1067,    -1,    -1,    -1,
      -1,    -1,   591,    -1,    -1,    -1,    -1,    -1,    -1,  1657,
    1658,    -1,    -1,    -1,    -1,   604,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1673,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1464,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   655,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1528,    -1,    -1,    -1,    -1,  1493,   676,   677,    -1,
    1161,   680,    -1,   682,    -1,    -1,    -1,    -1,    -1,   688,
      -1,   690,   691,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,  1189,  1767,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1775,    -1,   718,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   731,    -1,    -1,  1216,    -1,    -1,    -1,  1220,
      -1,    -1,    -1,   742,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1610,    -1,    -1,    -1,   756,    -1,    -1,
     759,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1829,    -1,  1590,    -1,  1833,  1834,    -1,    -1,  1837,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,    -1,    -1,
     789,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1870,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   825,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1322,  1323,  1324,    -1,  1326,  1327,  1664,  1665,    -1,
      -1,    -1,  1333,    -1,    -1,    -1,    -1,    -1,    -1,  1676,
      -1,    -1,    -1,  1921,  1922,    -1,    -1,    -1,  1725,    -1,
      -1,    -1,    -1,    -1,   873,    -1,    -1,  1734,    -1,    -1,
      -1,    -1,    -1,    -1,   883,   884,    -1,    -1,    -1,    -1,
      -1,    -1,   891,    -1,    -1,    -1,  1954,    -1,    -1,    -1,
    1381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   912,    -1,    -1,    -1,    -1,    -1,  1400,
      -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   934,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   942,    -1,     3,    -1,    -1,    -1,    -1,
     949,     9,  1769,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,   995,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    -1,  1833,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1515,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1062,    -1,  1064,    -1,  1066,  1549,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
    1591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1132,  1133,    -1,  1617,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1642,  1643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    17,  1193,    -1,    -1,    -1,    -1,    -1,
    1199,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1692,    -1,    -1,    -1,    -1,    -1,  1216,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,  1233,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    72,  1248,
      74,    -1,  1251,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,  1303,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1804,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,  1341,    -1,    -1,    -1,    -1,    -1,  1829,    -1,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1359,    -1,    -1,  1362,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1868,    17,    -1,
    1871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1400,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1410,  1411,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1435,    -1,  1437,    -1,
      69,    -1,    71,    72,    -1,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1515,    -1,    -1,   148,
     149,  1520,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,     1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1576,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,  1615,    71,    72,  1618,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   148,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,    -1,    50,    51,
      52,    -1,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    71,
      72,    -1,    74,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,  1812,    96,    -1,    98,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   174,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,    -1,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    71,    72,    -1,    74,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    -1,    96,    -1,    98,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,     3,     4,     5,     6,     7,
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
      -1,   149,    -1,   151,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   173,   174,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    67,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,   100,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   173,   174,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    67,    -1,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,   100,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,   151,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,     3,     4,     5,     6,     7,     8,     9,    10,
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
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
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
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
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
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
     165,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    55,    56,    57,
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
      -1,    -1,   160,   161,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    55,    56,
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
      -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,
      -1,    -1,   100,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,   116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    70,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,   101,    50,    51,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    67,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,     3,    -1,    -1,    -1,   160,   161,
       9,    -1,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,   152,   153,    -1,    -1,     3,    -1,    -1,
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
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    19,    70,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    12,    13,    14,    15,    16,   160,   161,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    19,    70,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    48,   152,   153,    -1,
      52,    -1,    54,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,
      -1,    -1,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    -1,    96,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    -1,    -1,
      74,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,
     153,    -1,    -1,   104,   105,    -1,    -1,   160,   161,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,   152,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    12,
      13,    14,    15,    16,    17,    70,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,   104,
     105,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,   104,   105,    -1,    -1,   160,   161,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,   152,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    48,    -1,    50,    51,    52,
      -1,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
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
     149,    -1,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
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
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    12,    13,    14,    15,
      16,   160,   161,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,     6,
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
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    12,
      13,    14,    15,    16,    17,    70,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,   152,    -1,    50,    51,   104,
     105,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    76,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,   152,   153,    -1,
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
     161,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    12,    13,    14,    15,    16,    17,    70,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,   104,   105,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,   104,   105,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    12,    13,    14,    15,    16,
      17,    70,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,   104,   105,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    17,    70,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    12,    13,
      14,    15,    16,    17,    70,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,   152,    -1,    50,    51,   104,   105,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
     104,   105,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      12,    13,    14,    15,    16,    17,    70,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
     104,   105,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,   104,   105,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,    12,    13,    14,    15,    16,   160,   161,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,    70,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   104,   105,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   152,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
     152,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,   104,   105,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,   152,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,   152,    49,    50,
      51,    -1,    53,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    48,    -1,   129,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,    -1,    -1,   149,    71,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     173,   174,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,   151,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    48,   121,   122,    -1,    52,    -1,    54,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    48,   121,   122,    -1,    52,    -1,
      54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,   156,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    48,   121,   122,    -1,    52,    -1,    54,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    48,   121,   122,    -1,    52,    -1,
      54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    48,   121,   122,    -1,    52,    -1,    54,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    48,   121,   122,    -1,    52,    -1,
      54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165
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
     462,   173,   217,   346,   157,   345,   151,   359,   151,   151,
     155,   149,   174,   358,   153,   358,   358,   358,   214,   358,
     151,   358,   358,   358,   176,   151,   162,   163,   200,    17,
     297,   151,   155,   151,   160,   161,   151,   220,   214,   157,
     214,   180,   214,   180,   113,   153,   180,   150,   188,   189,
     190,   214,   113,   153,   180,   330,   214,   188,   180,   198,
     201,   201,   201,   202,   202,   203,   203,   204,   204,   204,
     204,   205,   205,   206,   207,   208,   209,   210,   156,   221,
     174,   182,   153,   180,   214,   157,   214,   176,   436,   437,
     438,   295,   435,   402,   402,   214,   359,   149,   402,   439,
     440,   149,   439,   440,   176,   176,   154,   154,   149,   408,
     427,   428,   429,   432,    17,   295,   426,   430,   149,   402,
     445,   463,   402,   402,   463,   149,   402,   445,   402,   402,
     177,   213,   357,   154,   155,   154,   155,   463,   463,   131,
     347,   348,   349,   347,   357,   176,   212,   213,   214,   400,
     462,   361,   363,   148,   176,   151,   155,   176,   347,   180,
     399,   180,   151,   151,   151,   151,   151,   151,   149,   402,
     439,   440,   149,   402,   439,   440,   399,   182,   440,   214,
     225,   350,   151,   151,   151,   151,   386,   387,   225,   388,
     225,   397,   387,   225,   157,   157,   157,   331,   177,   177,
     180,   276,   357,    17,    69,    71,    74,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    90,
      91,    92,    93,    94,    96,   104,   105,   116,   176,   221,
     222,   223,   224,   225,   226,   227,   229,   230,   240,   244,
     245,   246,   247,   248,   249,   254,   255,   261,   262,   263,
     277,   295,   299,   357,   398,    68,   174,   177,   177,   177,
     347,   177,   389,   387,   281,   283,   292,   381,   382,   383,
     384,   376,   173,   367,   367,   293,   453,   153,   160,   196,
     214,   317,   214,   295,   350,   151,   151,   151,     5,   295,
     402,   441,   157,   180,   431,     9,   357,   148,   361,   345,
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
     462,   148,   148,   176,   151,   180,    76,   185,   186,   358,
     198,   198,   198,   198,   198,   157,   362,   155,   148,   194,
     153,   192,   194,   194,   154,   155,   120,   152,   190,   154,
     220,   212,   174,   154,   462,   177,   149,   402,   439,   440,
     350,   350,   177,   177,   151,   149,   402,   439,   440,   149,
     402,   445,   408,   402,   402,   350,   350,   154,   349,   352,
     352,   353,   151,   155,   155,   151,   177,   213,   213,   154,
     154,   177,   177,   151,   214,   176,   176,   350,   350,   360,
     402,   155,   151,   148,   389,   148,   148,   148,   148,   292,
     330,   338,   456,   292,   337,   149,   326,   174,   174,   149,
     156,   196,   333,   334,   340,   408,   409,   422,   155,   174,
     357,   176,   357,   151,   188,   189,   174,   225,   174,   225,
     221,    78,   151,   221,   232,   277,   279,   282,   288,   295,
     299,   151,   173,   174,   221,   241,   242,   277,   174,   174,
     221,   174,   362,   174,   221,   220,   221,   108,   109,   110,
     111,   112,   256,   258,   259,   174,    95,   174,    82,   149,
     149,   177,   148,   174,   174,   149,   223,   225,   402,   174,
     151,   176,   148,   148,   176,   155,   155,   154,   154,   154,
     177,   151,   176,   214,   214,   177,   154,   177,   462,   343,
     157,   346,   148,   381,   151,   156,   151,   155,   156,   362,
     462,   220,   118,   191,   192,   153,   192,   153,   192,   154,
     148,   151,   176,   177,   177,   151,   151,   176,   176,   177,
     177,   177,   176,   176,   154,   177,   151,   402,   350,   350,
     177,   177,   221,   148,   326,   326,   326,   149,   196,   335,
     336,   439,   447,   448,   449,   450,   174,   155,   174,   333,
     174,   376,   403,   408,   214,   295,   155,   174,   339,   340,
     339,   357,   131,   354,   355,   221,   151,   151,   149,   223,
     151,   221,   295,   223,   221,   222,   143,   144,   145,   165,
     174,   243,   151,   156,   222,   174,   462,   151,   151,   151,
     225,   258,   259,   149,   214,   149,   182,   232,   198,   251,
     107,     1,   223,   402,   382,   176,   176,   154,   350,   177,
     177,   154,   154,   148,   157,   345,   177,   214,   186,   214,
     462,   148,   154,   154,   191,   191,   350,   151,   151,   350,
     350,   151,   151,   154,   155,   131,   349,   131,   154,   177,
     177,   151,   151,   154,   448,   449,   450,   295,   447,   155,
     174,   402,   402,   174,   151,   408,   402,   174,   223,    75,
      76,   157,   235,   236,   237,   151,   221,    73,   223,    73,
     174,   104,   173,   221,   222,   221,   223,   242,   174,   148,
     157,   237,   223,   149,   176,   174,   182,   151,   156,   151,
     151,   155,   156,   249,   253,   357,   399,   177,   154,   154,
     345,   462,   148,   148,   154,   154,   177,   177,   177,   176,
     177,   151,   151,   151,   151,   151,   447,   402,   334,     1,
     213,   233,   234,   400,     1,   156,     1,   176,   223,   235,
      73,   174,   151,   223,    73,   223,   222,   221,   144,   165,
     243,   174,   165,    73,   222,   174,     1,   176,   176,   260,
     293,   295,   456,   156,   174,   153,   182,   265,   266,   267,
     223,   198,   188,    73,   106,   250,   252,   151,   462,   148,
     151,   151,   151,   352,   149,   402,   439,   440,   336,   131,
       1,   155,   156,   148,   270,   271,   277,   223,    73,   174,
     223,   150,   150,   221,   222,   221,   223,   148,   270,   260,
     177,   149,   196,   399,   447,   180,   156,   101,   149,   151,
     156,   155,    73,   151,   223,   149,   223,   223,   148,   176,
     213,   233,   236,   238,   239,   277,   223,   165,   165,   165,
     238,   177,   174,   257,   295,   265,   154,   213,   174,   265,
     267,   223,   221,   107,   107,   350,   223,   228,   177,   236,
     221,   150,   221,   221,   177,   257,   212,   151,   156,   182,
     151,   151,   156,   151,   253,    73,   248,   177,     1,   223,
     148,   228,   148,   151,   225,   182,   268,   149,   174,   268,
     223,    73,   151,   225,   155,   156,   213,   151,   223,   182,
     180,   269,   151,   174,   151,   155,   174,   180
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
     345,   346,   346,   347,   347,   347,   347,   347,   347,   347,
     348,   348,   348,   348,   349,   349,   350,   350,   350,   350,
     351,   351,   351,   351,   352,   352,   352,   352,   352,   353,
     353,   353,   353,   353,   354,   354,   355,   355,   356,   356,
     357,   357,   357,   358,   358,   358,   359,   359,   360,   360,
     360,   360,   361,   361,   362,   362,   362,   362,   362,   363,
     363,   364,   364,   365,   365,   365,   365,   365,   366,   366,
     367,   367,   369,   368,   370,   368,   368,   368,   371,   371,
     371,   371,   372,   372,   372,   372,   373,   373,   374,   374,
     375,   375,   376,   376,   376,   376,   377,   377,   377,   378,
     378,   379,   379,   380,   380,   381,   381,   382,   382,   383,
     383,   383,   384,   384,   385,   385,   386,   386,   387,   387,
     388,   389,   390,   390,   390,   390,   390,   391,   390,   392,
     390,   393,   390,   394,   390,   395,   390,   396,   396,   396,
     397,   397,   398,   398,   398,   398,   398,   398,   398,   398,
     398,   398,   399,   399,   399,   400,   401,   401,   402,   402,
     403,   403,   404,   405,   405,   406,   406,   406,   407,   407,
     407,   407,   407,   407,   408,   408,   409,   409,   409,   409,
     410,   410,   410,   410,   411,   411,   411,   411,   411,   411,
     411,   412,   412,   412,   412,   413,   413,   413,   414,   414,
     414,   414,   414,   415,   415,   415,   415,   416,   416,   416,
     416,   416,   416,   417,   417,   417,   418,   418,   418,   418,
     418,   419,   419,   419,   419,   420,   420,   420,   420,   420,
     420,   421,   421,   422,   422,   422,   422,   423,   423,   423,
     423,   424,   424,   424,   424,   424,   424,   424,   425,   425,
     425,   425,   425,   426,   426,   426,   426,   426,   427,   427,
     427,   428,   428,   428,   428,   429,   429,   429,   430,   430,
     430,   430,   430,   431,   431,   432,   432,   432,   433,   433,
     434,   434,   435,   435,   435,   436,   436,   436,   436,   436,
     437,   437,   437,   437,   438,   438,   438,   439,   439,   439,
     439,   440,   440,   440,   440,   441,   441,   441,   441,   442,
     442,   442,   442,   442,   443,   443,   443,   443,   444,   444,
     444,   445,   445,   445,   446,   446,   446,   446,   446,   446,
     447,   447,   447,   448,   448,   448,   448,   448,   449,   449,
     449,   449,   450,   450,   451,   451,   451,   452,   452,   453,
     453,   453,   453,   453,   453,   454,   454,   454,   454,   454,
     454,   454,   454,   454,   454,   455,   455,   455,   455,   456,
     456,   456,   457,   457,   458,   458,   458,   458,   458,   458,
     459,   459,   459,   459,   459,   459,   460,   460,   460,   461,
     461,   462,   462,   463,   463
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
       5,     0,     2,     0,     1,     1,     1,     5,     5,     5,
       1,     5,     5,     9,     1,     5,     0,     1,     1,     5,
       1,     1,     5,     5,     1,     3,     3,     4,     1,     1,
       1,     1,     2,     1,     3,     3,     2,     3,     1,     3,
       1,     1,     1,     1,     1,     2,     1,     1,     0,     2,
       2,     4,     1,     4,     0,     1,     2,     3,     4,     2,
       2,     1,     2,     2,     5,     5,     7,     6,     1,     3,
       0,     2,     0,     5,     0,     5,     3,     1,     0,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       5,     6,     1,     1,     3,     3,     2,     3,     3,     2,
       4,     1,     4,     7,    10,     1,     4,     2,     2,     1,
       1,     5,     2,     5,     0,     1,     3,     4,     0,     1,
       0,     0,     1,     1,     1,     2,     5,     0,     6,     0,
       8,     0,     7,     0,     7,     0,     8,     1,     2,     3,
       0,     5,     3,     4,     4,     4,     4,     5,     5,     5,
       5,     6,     1,     1,     1,     3,     0,     5,     0,     1,
       1,     2,     6,     1,     3,     0,     1,     4,     1,     1,
       1,     1,     1,     1,     1,     3,     2,     1,     2,     2,
       2,     3,     4,     5,     2,     4,     5,     4,     5,     3,
       4,     8,     9,     3,     4,     2,     1,     2,     6,     8,
       9,     3,     4,     2,     3,     4,     5,     4,     5,     4,
       5,     3,     4,     1,     1,     1,     4,     8,     9,     3,
       4,     2,     3,     3,     4,     4,     5,     4,     5,     3,
       4,     1,     3,     2,     1,     2,     2,     2,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     6,     8,
       9,     3,     4,     2,     4,     1,     2,     2,     2,     3,
       4,     2,     4,     4,     3,     6,     8,     3,     2,     4,
       1,     2,     2,     1,     1,     2,     3,     4,     2,     4,
       6,     8,     1,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     5,     8,     3,     2,     3,     7,
       1,     5,     5,     6,     6,     1,     3,     2,     2,     1,
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
#line 546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7037 "Parser/parser.cc"
    break;

  case 3:
#line 550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7043 "Parser/parser.cc"
    break;

  case 4:
#line 557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7049 "Parser/parser.cc"
    break;

  case 5:
#line 558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7055 "Parser/parser.cc"
    break;

  case 6:
#line 559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7061 "Parser/parser.cc"
    break;

  case 7:
#line 560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7067 "Parser/parser.cc"
    break;

  case 8:
#line 561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7073 "Parser/parser.cc"
    break;

  case 19:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7079 "Parser/parser.cc"
    break;

  case 20:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7085 "Parser/parser.cc"
    break;

  case 21:
#line 590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7091 "Parser/parser.cc"
    break;

  case 22:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7101 "Parser/parser.cc"
    break;

  case 23:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7107 "Parser/parser.cc"
    break;

  case 24:
#line 605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7113 "Parser/parser.cc"
    break;

  case 25:
#line 609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7119 "Parser/parser.cc"
    break;

  case 27:
#line 612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7125 "Parser/parser.cc"
    break;

  case 28:
#line 614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7131 "Parser/parser.cc"
    break;

  case 29:
#line 616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7137 "Parser/parser.cc"
    break;

  case 30:
#line 618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7143 "Parser/parser.cc"
    break;

  case 31:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7153 "Parser/parser.cc"
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
#line 7165 "Parser/parser.cc"
    break;

  case 33:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7175 "Parser/parser.cc"
    break;

  case 35:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7186 "Parser/parser.cc"
    break;

  case 36:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7195 "Parser/parser.cc"
    break;

  case 37:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7201 "Parser/parser.cc"
    break;

  case 39:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7207 "Parser/parser.cc"
    break;

  case 40:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7213 "Parser/parser.cc"
    break;

  case 41:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7219 "Parser/parser.cc"
    break;

  case 42:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7225 "Parser/parser.cc"
    break;

  case 43:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7235 "Parser/parser.cc"
    break;

  case 44:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7241 "Parser/parser.cc"
    break;

  case 45:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7247 "Parser/parser.cc"
    break;

  case 46:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7253 "Parser/parser.cc"
    break;

  case 47:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7259 "Parser/parser.cc"
    break;

  case 48:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7265 "Parser/parser.cc"
    break;

  case 49:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7271 "Parser/parser.cc"
    break;

  case 50:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7277 "Parser/parser.cc"
    break;

  case 51:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7283 "Parser/parser.cc"
    break;

  case 52:
#line 706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7289 "Parser/parser.cc"
    break;

  case 53:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7295 "Parser/parser.cc"
    break;

  case 54:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7301 "Parser/parser.cc"
    break;

  case 55:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7307 "Parser/parser.cc"
    break;

  case 56:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7313 "Parser/parser.cc"
    break;

  case 57:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7319 "Parser/parser.cc"
    break;

  case 58:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7325 "Parser/parser.cc"
    break;

  case 59:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7331 "Parser/parser.cc"
    break;

  case 60:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7341 "Parser/parser.cc"
    break;

  case 61:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7347 "Parser/parser.cc"
    break;

  case 64:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7353 "Parser/parser.cc"
    break;

  case 65:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7359 "Parser/parser.cc"
    break;

  case 68:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7365 "Parser/parser.cc"
    break;

  case 70:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7371 "Parser/parser.cc"
    break;

  case 71:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7377 "Parser/parser.cc"
    break;

  case 72:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7383 "Parser/parser.cc"
    break;

  case 73:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7389 "Parser/parser.cc"
    break;

  case 74:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7395 "Parser/parser.cc"
    break;

  case 75:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7401 "Parser/parser.cc"
    break;

  case 76:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7407 "Parser/parser.cc"
    break;

  case 77:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7413 "Parser/parser.cc"
    break;

  case 78:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7421 "Parser/parser.cc"
    break;

  case 79:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7427 "Parser/parser.cc"
    break;

  case 80:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7436 "Parser/parser.cc"
    break;

  case 83:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7442 "Parser/parser.cc"
    break;

  case 84:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7448 "Parser/parser.cc"
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
#line 7468 "Parser/parser.cc"
    break;

  case 86:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7474 "Parser/parser.cc"
    break;

  case 87:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7480 "Parser/parser.cc"
    break;

  case 88:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7486 "Parser/parser.cc"
    break;

  case 89:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7492 "Parser/parser.cc"
    break;

  case 90:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7498 "Parser/parser.cc"
    break;

  case 91:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7504 "Parser/parser.cc"
    break;

  case 92:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7510 "Parser/parser.cc"
    break;

  case 93:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7516 "Parser/parser.cc"
    break;

  case 94:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7525 "Parser/parser.cc"
    break;

  case 95:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7531 "Parser/parser.cc"
    break;

  case 96:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7537 "Parser/parser.cc"
    break;

  case 97:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7543 "Parser/parser.cc"
    break;

  case 98:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7549 "Parser/parser.cc"
    break;

  case 99:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7555 "Parser/parser.cc"
    break;

  case 100:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7561 "Parser/parser.cc"
    break;

  case 101:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7567 "Parser/parser.cc"
    break;

  case 103:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7573 "Parser/parser.cc"
    break;

  case 104:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7579 "Parser/parser.cc"
    break;

  case 105:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7585 "Parser/parser.cc"
    break;

  case 106:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7591 "Parser/parser.cc"
    break;

  case 107:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7597 "Parser/parser.cc"
    break;

  case 108:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7603 "Parser/parser.cc"
    break;

  case 109:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7609 "Parser/parser.cc"
    break;

  case 110:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7615 "Parser/parser.cc"
    break;

  case 118:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7621 "Parser/parser.cc"
    break;

  case 120:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7627 "Parser/parser.cc"
    break;

  case 121:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7633 "Parser/parser.cc"
    break;

  case 122:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7639 "Parser/parser.cc"
    break;

  case 124:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7645 "Parser/parser.cc"
    break;

  case 125:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7651 "Parser/parser.cc"
    break;

  case 127:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7657 "Parser/parser.cc"
    break;

  case 128:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7663 "Parser/parser.cc"
    break;

  case 130:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7669 "Parser/parser.cc"
    break;

  case 131:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7675 "Parser/parser.cc"
    break;

  case 132:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7681 "Parser/parser.cc"
    break;

  case 133:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7687 "Parser/parser.cc"
    break;

  case 135:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7693 "Parser/parser.cc"
    break;

  case 136:
#line 938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7699 "Parser/parser.cc"
    break;

  case 138:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7705 "Parser/parser.cc"
    break;

  case 140:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7711 "Parser/parser.cc"
    break;

  case 142:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7717 "Parser/parser.cc"
    break;

  case 144:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7723 "Parser/parser.cc"
    break;

  case 146:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7729 "Parser/parser.cc"
    break;

  case 148:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7735 "Parser/parser.cc"
    break;

  case 149:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7741 "Parser/parser.cc"
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
#line 7753 "Parser/parser.cc"
    break;

  case 153:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7759 "Parser/parser.cc"
    break;

  case 154:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7765 "Parser/parser.cc"
    break;

  case 158:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7771 "Parser/parser.cc"
    break;

  case 159:
#line 1012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7777 "Parser/parser.cc"
    break;

  case 160:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7783 "Parser/parser.cc"
    break;

  case 161:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7789 "Parser/parser.cc"
    break;

  case 162:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7795 "Parser/parser.cc"
    break;

  case 163:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7801 "Parser/parser.cc"
    break;

  case 164:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7807 "Parser/parser.cc"
    break;

  case 165:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7813 "Parser/parser.cc"
    break;

  case 166:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7819 "Parser/parser.cc"
    break;

  case 167:
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7825 "Parser/parser.cc"
    break;

  case 168:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7831 "Parser/parser.cc"
    break;

  case 169:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7837 "Parser/parser.cc"
    break;

  case 170:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7843 "Parser/parser.cc"
    break;

  case 171:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7849 "Parser/parser.cc"
    break;

  case 172:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7855 "Parser/parser.cc"
    break;

  case 174:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7861 "Parser/parser.cc"
    break;

  case 175:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7867 "Parser/parser.cc"
    break;

  case 176:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7873 "Parser/parser.cc"
    break;

  case 178:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7879 "Parser/parser.cc"
    break;

  case 179:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7885 "Parser/parser.cc"
    break;

  case 191:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7891 "Parser/parser.cc"
    break;

  case 193:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7897 "Parser/parser.cc"
    break;

  case 194:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7903 "Parser/parser.cc"
    break;

  case 195:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 7914 "Parser/parser.cc"
    break;

  case 196:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7920 "Parser/parser.cc"
    break;

  case 197:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7926 "Parser/parser.cc"
    break;

  case 199:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7932 "Parser/parser.cc"
    break;

  case 200:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7938 "Parser/parser.cc"
    break;

  case 201:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 202:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 203:
#line 1121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 206:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7962 "Parser/parser.cc"
    break;

  case 207:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 7968 "Parser/parser.cc"
    break;

  case 208:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 209:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 210:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7986 "Parser/parser.cc"
    break;

  case 211:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7992 "Parser/parser.cc"
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
#line 8006 "Parser/parser.cc"
    break;

  case 213:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8012 "Parser/parser.cc"
    break;

  case 214:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8018 "Parser/parser.cc"
    break;

  case 215:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8027 "Parser/parser.cc"
    break;

  case 216:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8033 "Parser/parser.cc"
    break;

  case 217:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8039 "Parser/parser.cc"
    break;

  case 218:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8045 "Parser/parser.cc"
    break;

  case 219:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8051 "Parser/parser.cc"
    break;

  case 220:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8057 "Parser/parser.cc"
    break;

  case 221:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8063 "Parser/parser.cc"
    break;

  case 222:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8069 "Parser/parser.cc"
    break;

  case 223:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8075 "Parser/parser.cc"
    break;

  case 224:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8081 "Parser/parser.cc"
    break;

  case 226:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8087 "Parser/parser.cc"
    break;

  case 227:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8093 "Parser/parser.cc"
    break;

  case 228:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8099 "Parser/parser.cc"
    break;

  case 229:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8105 "Parser/parser.cc"
    break;

  case 230:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8111 "Parser/parser.cc"
    break;

  case 231:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8117 "Parser/parser.cc"
    break;

  case 232:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8123 "Parser/parser.cc"
    break;

  case 234:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8129 "Parser/parser.cc"
    break;

  case 235:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8135 "Parser/parser.cc"
    break;

  case 236:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8141 "Parser/parser.cc"
    break;

  case 238:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8147 "Parser/parser.cc"
    break;

  case 239:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8153 "Parser/parser.cc"
    break;

  case 240:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8159 "Parser/parser.cc"
    break;

  case 241:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8168 "Parser/parser.cc"
    break;

  case 242:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8174 "Parser/parser.cc"
    break;

  case 243:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8180 "Parser/parser.cc"
    break;

  case 244:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8186 "Parser/parser.cc"
    break;

  case 245:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8195 "Parser/parser.cc"
    break;

  case 246:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8201 "Parser/parser.cc"
    break;

  case 247:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 248:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8213 "Parser/parser.cc"
    break;

  case 249:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8222 "Parser/parser.cc"
    break;

  case 250:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8228 "Parser/parser.cc"
    break;

  case 251:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8234 "Parser/parser.cc"
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
#line 8253 "Parser/parser.cc"
    break;

  case 254:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8259 "Parser/parser.cc"
    break;

  case 255:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8265 "Parser/parser.cc"
    break;

  case 256:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8271 "Parser/parser.cc"
    break;

  case 257:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8278 "Parser/parser.cc"
    break;

  case 258:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8285 "Parser/parser.cc"
    break;

  case 259:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8291 "Parser/parser.cc"
    break;

  case 260:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8297 "Parser/parser.cc"
    break;

  case 261:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8303 "Parser/parser.cc"
    break;

  case 262:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8310 "Parser/parser.cc"
    break;

  case 263:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8317 "Parser/parser.cc"
    break;

  case 264:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8323 "Parser/parser.cc"
    break;

  case 265:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8329 "Parser/parser.cc"
    break;

  case 266:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8338 "Parser/parser.cc"
    break;

  case 267:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8344 "Parser/parser.cc"
    break;

  case 268:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8350 "Parser/parser.cc"
    break;

  case 269:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8356 "Parser/parser.cc"
    break;

  case 270:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8362 "Parser/parser.cc"
    break;

  case 271:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8368 "Parser/parser.cc"
    break;

  case 272:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8374 "Parser/parser.cc"
    break;

  case 273:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8380 "Parser/parser.cc"
    break;

  case 274:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8386 "Parser/parser.cc"
    break;

  case 275:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8392 "Parser/parser.cc"
    break;

  case 276:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8398 "Parser/parser.cc"
    break;

  case 277:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8404 "Parser/parser.cc"
    break;

  case 278:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8410 "Parser/parser.cc"
    break;

  case 279:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8416 "Parser/parser.cc"
    break;

  case 280:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8422 "Parser/parser.cc"
    break;

  case 281:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8428 "Parser/parser.cc"
    break;

  case 282:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8434 "Parser/parser.cc"
    break;

  case 283:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8440 "Parser/parser.cc"
    break;

  case 284:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8446 "Parser/parser.cc"
    break;

  case 285:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8452 "Parser/parser.cc"
    break;

  case 286:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8458 "Parser/parser.cc"
    break;

  case 287:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8464 "Parser/parser.cc"
    break;

  case 288:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8470 "Parser/parser.cc"
    break;

  case 289:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8476 "Parser/parser.cc"
    break;

  case 290:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8482 "Parser/parser.cc"
    break;

  case 291:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8488 "Parser/parser.cc"
    break;

  case 292:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8494 "Parser/parser.cc"
    break;

  case 293:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8500 "Parser/parser.cc"
    break;

  case 294:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8506 "Parser/parser.cc"
    break;

  case 295:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8512 "Parser/parser.cc"
    break;

  case 298:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8518 "Parser/parser.cc"
    break;

  case 299:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8524 "Parser/parser.cc"
    break;

  case 300:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8530 "Parser/parser.cc"
    break;

  case 301:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8536 "Parser/parser.cc"
    break;

  case 303:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8542 "Parser/parser.cc"
    break;

  case 304:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8548 "Parser/parser.cc"
    break;

  case 306:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8554 "Parser/parser.cc"
    break;

  case 307:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8560 "Parser/parser.cc"
    break;

  case 308:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8566 "Parser/parser.cc"
    break;

  case 309:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8572 "Parser/parser.cc"
    break;

  case 310:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8578 "Parser/parser.cc"
    break;

  case 311:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8584 "Parser/parser.cc"
    break;

  case 312:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8590 "Parser/parser.cc"
    break;

  case 313:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8596 "Parser/parser.cc"
    break;

  case 314:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8602 "Parser/parser.cc"
    break;

  case 315:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8608 "Parser/parser.cc"
    break;

  case 316:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8614 "Parser/parser.cc"
    break;

  case 317:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8620 "Parser/parser.cc"
    break;

  case 318:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8626 "Parser/parser.cc"
    break;

  case 319:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8632 "Parser/parser.cc"
    break;

  case 320:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8638 "Parser/parser.cc"
    break;

  case 321:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8644 "Parser/parser.cc"
    break;

  case 322:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8650 "Parser/parser.cc"
    break;

  case 323:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8656 "Parser/parser.cc"
    break;

  case 324:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8662 "Parser/parser.cc"
    break;

  case 325:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8668 "Parser/parser.cc"
    break;

  case 326:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8674 "Parser/parser.cc"
    break;

  case 327:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8680 "Parser/parser.cc"
    break;

  case 329:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8686 "Parser/parser.cc"
    break;

  case 330:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8692 "Parser/parser.cc"
    break;

  case 331:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8698 "Parser/parser.cc"
    break;

  case 336:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8704 "Parser/parser.cc"
    break;

  case 337:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8710 "Parser/parser.cc"
    break;

  case 338:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8716 "Parser/parser.cc"
    break;

  case 339:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8722 "Parser/parser.cc"
    break;

  case 340:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8728 "Parser/parser.cc"
    break;

  case 341:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8734 "Parser/parser.cc"
    break;

  case 342:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8740 "Parser/parser.cc"
    break;

  case 343:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8746 "Parser/parser.cc"
    break;

  case 346:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8752 "Parser/parser.cc"
    break;

  case 347:
#line 1568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8758 "Parser/parser.cc"
    break;

  case 348:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8764 "Parser/parser.cc"
    break;

  case 349:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8770 "Parser/parser.cc"
    break;

  case 350:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8776 "Parser/parser.cc"
    break;

  case 351:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8782 "Parser/parser.cc"
    break;

  case 352:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8791 "Parser/parser.cc"
    break;

  case 353:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8800 "Parser/parser.cc"
    break;

  case 354:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8806 "Parser/parser.cc"
    break;

  case 357:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8812 "Parser/parser.cc"
    break;

  case 358:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8818 "Parser/parser.cc"
    break;

  case 360:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8824 "Parser/parser.cc"
    break;

  case 361:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8830 "Parser/parser.cc"
    break;

  case 368:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 8841 "Parser/parser.cc"
    break;

  case 371:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8847 "Parser/parser.cc"
    break;

  case 372:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8853 "Parser/parser.cc"
    break;

  case 376:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8859 "Parser/parser.cc"
    break;

  case 378:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8865 "Parser/parser.cc"
    break;

  case 379:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8871 "Parser/parser.cc"
    break;

  case 380:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8877 "Parser/parser.cc"
    break;

  case 381:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8883 "Parser/parser.cc"
    break;

  case 382:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8889 "Parser/parser.cc"
    break;

  case 383:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8895 "Parser/parser.cc"
    break;

  case 385:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8901 "Parser/parser.cc"
    break;

  case 386:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8907 "Parser/parser.cc"
    break;

  case 387:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8913 "Parser/parser.cc"
    break;

  case 388:
#line 1706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8924 "Parser/parser.cc"
    break;

  case 389:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8930 "Parser/parser.cc"
    break;

  case 390:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8936 "Parser/parser.cc"
    break;

  case 391:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8942 "Parser/parser.cc"
    break;

  case 392:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8948 "Parser/parser.cc"
    break;

  case 393:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8957 "Parser/parser.cc"
    break;

  case 394:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8966 "Parser/parser.cc"
    break;

  case 395:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8975 "Parser/parser.cc"
    break;

  case 396:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8984 "Parser/parser.cc"
    break;

  case 397:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8993 "Parser/parser.cc"
    break;

  case 398:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9002 "Parser/parser.cc"
    break;

  case 399:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9011 "Parser/parser.cc"
    break;

  case 400:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9020 "Parser/parser.cc"
    break;

  case 401:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9028 "Parser/parser.cc"
    break;

  case 402:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9036 "Parser/parser.cc"
    break;

  case 403:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 407:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 408:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 416:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9065 "Parser/parser.cc"
    break;

  case 421:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9071 "Parser/parser.cc"
    break;

  case 424:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9077 "Parser/parser.cc"
    break;

  case 427:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9083 "Parser/parser.cc"
    break;

  case 428:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9089 "Parser/parser.cc"
    break;

  case 429:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9095 "Parser/parser.cc"
    break;

  case 430:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9101 "Parser/parser.cc"
    break;

  case 432:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 434:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 435:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 437:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 438:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9131 "Parser/parser.cc"
    break;

  case 439:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9137 "Parser/parser.cc"
    break;

  case 440:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9143 "Parser/parser.cc"
    break;

  case 441:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9149 "Parser/parser.cc"
    break;

  case 442:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9155 "Parser/parser.cc"
    break;

  case 443:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9161 "Parser/parser.cc"
    break;

  case 444:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9167 "Parser/parser.cc"
    break;

  case 445:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9173 "Parser/parser.cc"
    break;

  case 446:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9179 "Parser/parser.cc"
    break;

  case 447:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9185 "Parser/parser.cc"
    break;

  case 448:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9191 "Parser/parser.cc"
    break;

  case 449:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9197 "Parser/parser.cc"
    break;

  case 450:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9203 "Parser/parser.cc"
    break;

  case 451:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9209 "Parser/parser.cc"
    break;

  case 452:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9215 "Parser/parser.cc"
    break;

  case 453:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9221 "Parser/parser.cc"
    break;

  case 454:
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9227 "Parser/parser.cc"
    break;

  case 455:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9233 "Parser/parser.cc"
    break;

  case 456:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9239 "Parser/parser.cc"
    break;

  case 457:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9245 "Parser/parser.cc"
    break;

  case 458:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9251 "Parser/parser.cc"
    break;

  case 459:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9257 "Parser/parser.cc"
    break;

  case 460:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9263 "Parser/parser.cc"
    break;

  case 461:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9269 "Parser/parser.cc"
    break;

  case 462:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9275 "Parser/parser.cc"
    break;

  case 463:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9281 "Parser/parser.cc"
    break;

  case 464:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9287 "Parser/parser.cc"
    break;

  case 465:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9293 "Parser/parser.cc"
    break;

  case 466:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9299 "Parser/parser.cc"
    break;

  case 467:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9305 "Parser/parser.cc"
    break;

  case 468:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9311 "Parser/parser.cc"
    break;

  case 469:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9317 "Parser/parser.cc"
    break;

  case 470:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9323 "Parser/parser.cc"
    break;

  case 471:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9329 "Parser/parser.cc"
    break;

  case 472:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9335 "Parser/parser.cc"
    break;

  case 474:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9341 "Parser/parser.cc"
    break;

  case 476:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9347 "Parser/parser.cc"
    break;

  case 477:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9353 "Parser/parser.cc"
    break;

  case 478:
#line 2022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9359 "Parser/parser.cc"
    break;

  case 480:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9365 "Parser/parser.cc"
    break;

  case 481:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9371 "Parser/parser.cc"
    break;

  case 482:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9377 "Parser/parser.cc"
    break;

  case 483:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 485:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 487:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9395 "Parser/parser.cc"
    break;

  case 488:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 489:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9407 "Parser/parser.cc"
    break;

  case 490:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9413 "Parser/parser.cc"
    break;

  case 491:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9419 "Parser/parser.cc"
    break;

  case 492:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9425 "Parser/parser.cc"
    break;

  case 493:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9431 "Parser/parser.cc"
    break;

  case 494:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9437 "Parser/parser.cc"
    break;

  case 495:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9443 "Parser/parser.cc"
    break;

  case 496:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9454 "Parser/parser.cc"
    break;

  case 497:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9460 "Parser/parser.cc"
    break;

  case 498:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 499:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 500:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9483 "Parser/parser.cc"
    break;

  case 501:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9489 "Parser/parser.cc"
    break;

  case 502:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9495 "Parser/parser.cc"
    break;

  case 503:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9504 "Parser/parser.cc"
    break;

  case 505:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9510 "Parser/parser.cc"
    break;

  case 506:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 507:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9522 "Parser/parser.cc"
    break;

  case 509:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9528 "Parser/parser.cc"
    break;

  case 510:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9534 "Parser/parser.cc"
    break;

  case 512:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9540 "Parser/parser.cc"
    break;

  case 513:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9546 "Parser/parser.cc"
    break;

  case 514:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9552 "Parser/parser.cc"
    break;

  case 516:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9558 "Parser/parser.cc"
    break;

  case 517:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9564 "Parser/parser.cc"
    break;

  case 518:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9570 "Parser/parser.cc"
    break;

  case 519:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9576 "Parser/parser.cc"
    break;

  case 520:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9582 "Parser/parser.cc"
    break;

  case 522:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9588 "Parser/parser.cc"
    break;

  case 523:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9594 "Parser/parser.cc"
    break;

  case 524:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9600 "Parser/parser.cc"
    break;

  case 525:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9606 "Parser/parser.cc"
    break;

  case 526:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9612 "Parser/parser.cc"
    break;

  case 527:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9623 "Parser/parser.cc"
    break;

  case 531:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9629 "Parser/parser.cc"
    break;

  case 532:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9635 "Parser/parser.cc"
    break;

  case 533:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9644 "Parser/parser.cc"
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
#line 9661 "Parser/parser.cc"
    break;

  case 535:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9670 "Parser/parser.cc"
    break;

  case 536:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9680 "Parser/parser.cc"
    break;

  case 537:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9689 "Parser/parser.cc"
    break;

  case 538:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9699 "Parser/parser.cc"
    break;

  case 540:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9705 "Parser/parser.cc"
    break;

  case 541:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9711 "Parser/parser.cc"
    break;

  case 542:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9721 "Parser/parser.cc"
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
#line 9736 "Parser/parser.cc"
    break;

  case 546:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9742 "Parser/parser.cc"
    break;

  case 547:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9748 "Parser/parser.cc"
    break;

  case 548:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9754 "Parser/parser.cc"
    break;

  case 549:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9760 "Parser/parser.cc"
    break;

  case 550:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9766 "Parser/parser.cc"
    break;

  case 551:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9772 "Parser/parser.cc"
    break;

  case 552:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9778 "Parser/parser.cc"
    break;

  case 553:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9784 "Parser/parser.cc"
    break;

  case 554:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9790 "Parser/parser.cc"
    break;

  case 555:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9796 "Parser/parser.cc"
    break;

  case 556:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9802 "Parser/parser.cc"
    break;

  case 557:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9808 "Parser/parser.cc"
    break;

  case 558:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9814 "Parser/parser.cc"
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
#line 9827 "Parser/parser.cc"
    break;

  case 560:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9833 "Parser/parser.cc"
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
#line 9846 "Parser/parser.cc"
    break;

  case 562:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9852 "Parser/parser.cc"
    break;

  case 565:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9858 "Parser/parser.cc"
    break;

  case 566:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9864 "Parser/parser.cc"
    break;

  case 569:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9870 "Parser/parser.cc"
    break;

  case 571:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9876 "Parser/parser.cc"
    break;

  case 572:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9882 "Parser/parser.cc"
    break;

  case 573:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9888 "Parser/parser.cc"
    break;

  case 574:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9894 "Parser/parser.cc"
    break;

  case 575:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9900 "Parser/parser.cc"
    break;

  case 577:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9906 "Parser/parser.cc"
    break;

  case 579:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9912 "Parser/parser.cc"
    break;

  case 580:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9918 "Parser/parser.cc"
    break;

  case 582:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9924 "Parser/parser.cc"
    break;

  case 583:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9930 "Parser/parser.cc"
    break;

  case 585:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9936 "Parser/parser.cc"
    break;

  case 586:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9942 "Parser/parser.cc"
    break;

  case 587:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9948 "Parser/parser.cc"
    break;

  case 588:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9954 "Parser/parser.cc"
    break;

  case 589:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9960 "Parser/parser.cc"
    break;

  case 590:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 9971 "Parser/parser.cc"
    break;

  case 591:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9980 "Parser/parser.cc"
    break;

  case 592:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 9988 "Parser/parser.cc"
    break;

  case 593:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 9998 "Parser/parser.cc"
    break;

  case 595:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10004 "Parser/parser.cc"
    break;

  case 596:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 597:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10016 "Parser/parser.cc"
    break;

  case 598:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10022 "Parser/parser.cc"
    break;

  case 599:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 600:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10034 "Parser/parser.cc"
    break;

  case 601:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10040 "Parser/parser.cc"
    break;

  case 602:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10046 "Parser/parser.cc"
    break;

  case 603:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10052 "Parser/parser.cc"
    break;

  case 604:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10058 "Parser/parser.cc"
    break;

  case 607:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10064 "Parser/parser.cc"
    break;

  case 608:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10070 "Parser/parser.cc"
    break;

  case 609:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10076 "Parser/parser.cc"
    break;

  case 611:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10082 "Parser/parser.cc"
    break;

  case 612:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10088 "Parser/parser.cc"
    break;

  case 613:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 615:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10100 "Parser/parser.cc"
    break;

  case 616:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10106 "Parser/parser.cc"
    break;

  case 617:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10112 "Parser/parser.cc"
    break;

  case 619:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10118 "Parser/parser.cc"
    break;

  case 622:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 623:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 625:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10136 "Parser/parser.cc"
    break;

  case 626:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10142 "Parser/parser.cc"
    break;

  case 627:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10148 "Parser/parser.cc"
    break;

  case 632:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10154 "Parser/parser.cc"
    break;

  case 634:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10160 "Parser/parser.cc"
    break;

  case 635:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10166 "Parser/parser.cc"
    break;

  case 636:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10172 "Parser/parser.cc"
    break;

  case 637:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10178 "Parser/parser.cc"
    break;

  case 638:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10184 "Parser/parser.cc"
    break;

  case 639:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10190 "Parser/parser.cc"
    break;

  case 645:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10196 "Parser/parser.cc"
    break;

  case 648:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10202 "Parser/parser.cc"
    break;

  case 649:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10208 "Parser/parser.cc"
    break;

  case 650:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10214 "Parser/parser.cc"
    break;

  case 651:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10220 "Parser/parser.cc"
    break;

  case 652:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10226 "Parser/parser.cc"
    break;

  case 653:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10232 "Parser/parser.cc"
    break;

  case 654:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10238 "Parser/parser.cc"
    break;

  case 656:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10244 "Parser/parser.cc"
    break;

  case 657:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10250 "Parser/parser.cc"
    break;

  case 658:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10256 "Parser/parser.cc"
    break;

  case 660:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10262 "Parser/parser.cc"
    break;

  case 662:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10268 "Parser/parser.cc"
    break;

  case 663:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10274 "Parser/parser.cc"
    break;

  case 664:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10280 "Parser/parser.cc"
    break;

  case 665:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10286 "Parser/parser.cc"
    break;

  case 666:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10292 "Parser/parser.cc"
    break;

  case 667:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10298 "Parser/parser.cc"
    break;

  case 669:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 670:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10310 "Parser/parser.cc"
    break;

  case 671:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10316 "Parser/parser.cc"
    break;

  case 672:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10327 "Parser/parser.cc"
    break;

  case 673:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 674:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10339 "Parser/parser.cc"
    break;

  case 675:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10345 "Parser/parser.cc"
    break;

  case 676:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10354 "Parser/parser.cc"
    break;

  case 677:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 678:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10366 "Parser/parser.cc"
    break;

  case 679:
#line 2688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10372 "Parser/parser.cc"
    break;

  case 680:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10378 "Parser/parser.cc"
    break;

  case 681:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10384 "Parser/parser.cc"
    break;

  case 682:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10390 "Parser/parser.cc"
    break;

  case 683:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10396 "Parser/parser.cc"
    break;

  case 684:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10402 "Parser/parser.cc"
    break;

  case 685:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10408 "Parser/parser.cc"
    break;

  case 686:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10414 "Parser/parser.cc"
    break;

  case 689:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 690:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 691:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10432 "Parser/parser.cc"
    break;

  case 692:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 694:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10444 "Parser/parser.cc"
    break;

  case 695:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10450 "Parser/parser.cc"
    break;

  case 696:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10456 "Parser/parser.cc"
    break;

  case 697:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 698:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 699:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 700:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 701:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10489 "Parser/parser.cc"
    break;

  case 702:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10498 "Parser/parser.cc"
    break;

  case 703:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10504 "Parser/parser.cc"
    break;

  case 704:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 706:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 711:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 712:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 713:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 715:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10540 "Parser/parser.cc"
    break;

  case 716:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10546 "Parser/parser.cc"
    break;

  case 717:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10552 "Parser/parser.cc"
    break;

  case 718:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10558 "Parser/parser.cc"
    break;

  case 720:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10564 "Parser/parser.cc"
    break;

  case 721:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10570 "Parser/parser.cc"
    break;

  case 722:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 725:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10585 "Parser/parser.cc"
    break;

  case 726:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10591 "Parser/parser.cc"
    break;

  case 727:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10600 "Parser/parser.cc"
    break;

  case 728:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10610 "Parser/parser.cc"
    break;

  case 729:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10619 "Parser/parser.cc"
    break;

  case 730:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10629 "Parser/parser.cc"
    break;

  case 731:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10638 "Parser/parser.cc"
    break;

  case 732:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10648 "Parser/parser.cc"
    break;

  case 733:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10657 "Parser/parser.cc"
    break;

  case 734:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10667 "Parser/parser.cc"
    break;

  case 735:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10676 "Parser/parser.cc"
    break;

  case 736:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10686 "Parser/parser.cc"
    break;

  case 738:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 739:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 740:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10704 "Parser/parser.cc"
    break;

  case 741:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 10716 "Parser/parser.cc"
    break;

  case 742:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10727 "Parser/parser.cc"
    break;

  case 743:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10736 "Parser/parser.cc"
    break;

  case 744:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10745 "Parser/parser.cc"
    break;

  case 745:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 746:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 747:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10763 "Parser/parser.cc"
    break;

  case 748:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10772 "Parser/parser.cc"
    break;

  case 749:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10778 "Parser/parser.cc"
    break;

  case 750:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10784 "Parser/parser.cc"
    break;

  case 751:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 755:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 756:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10802 "Parser/parser.cc"
    break;

  case 757:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10812 "Parser/parser.cc"
    break;

  case 758:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10818 "Parser/parser.cc"
    break;

  case 761:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 762:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10830 "Parser/parser.cc"
    break;

  case 764:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 765:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10842 "Parser/parser.cc"
    break;

  case 766:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 767:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 772:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10860 "Parser/parser.cc"
    break;

  case 773:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10866 "Parser/parser.cc"
    break;

  case 774:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 775:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10878 "Parser/parser.cc"
    break;

  case 776:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 778:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 779:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 780:
#line 3083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 781:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 782:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 783:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 784:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 785:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 786:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 787:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 788:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 789:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10956 "Parser/parser.cc"
    break;

  case 790:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 791:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 792:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 793:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10980 "Parser/parser.cc"
    break;

  case 794:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 795:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 797:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10998 "Parser/parser.cc"
    break;

  case 798:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11004 "Parser/parser.cc"
    break;

  case 799:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 800:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11016 "Parser/parser.cc"
    break;

  case 801:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11022 "Parser/parser.cc"
    break;

  case 802:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 803:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11034 "Parser/parser.cc"
    break;

  case 804:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 805:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 806:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11052 "Parser/parser.cc"
    break;

  case 807:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 808:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 809:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 810:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11076 "Parser/parser.cc"
    break;

  case 811:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11082 "Parser/parser.cc"
    break;

  case 812:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 816:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 817:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 818:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 819:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11112 "Parser/parser.cc"
    break;

  case 820:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11118 "Parser/parser.cc"
    break;

  case 821:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 822:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 823:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11136 "Parser/parser.cc"
    break;

  case 824:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11142 "Parser/parser.cc"
    break;

  case 825:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 826:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11154 "Parser/parser.cc"
    break;

  case 827:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 828:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 829:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11172 "Parser/parser.cc"
    break;

  case 830:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11178 "Parser/parser.cc"
    break;

  case 831:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11187 "Parser/parser.cc"
    break;

  case 832:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11193 "Parser/parser.cc"
    break;

  case 833:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11199 "Parser/parser.cc"
    break;

  case 835:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11205 "Parser/parser.cc"
    break;

  case 836:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11211 "Parser/parser.cc"
    break;

  case 837:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11217 "Parser/parser.cc"
    break;

  case 838:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11223 "Parser/parser.cc"
    break;

  case 839:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11229 "Parser/parser.cc"
    break;

  case 840:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11235 "Parser/parser.cc"
    break;

  case 841:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11241 "Parser/parser.cc"
    break;

  case 842:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11247 "Parser/parser.cc"
    break;

  case 843:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11253 "Parser/parser.cc"
    break;

  case 844:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11259 "Parser/parser.cc"
    break;

  case 845:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11265 "Parser/parser.cc"
    break;

  case 846:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11271 "Parser/parser.cc"
    break;

  case 847:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11277 "Parser/parser.cc"
    break;

  case 848:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11283 "Parser/parser.cc"
    break;

  case 849:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11289 "Parser/parser.cc"
    break;

  case 850:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11295 "Parser/parser.cc"
    break;

  case 851:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11301 "Parser/parser.cc"
    break;

  case 852:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 853:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 854:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11319 "Parser/parser.cc"
    break;

  case 856:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 857:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 858:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11337 "Parser/parser.cc"
    break;

  case 859:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11343 "Parser/parser.cc"
    break;

  case 860:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11349 "Parser/parser.cc"
    break;

  case 861:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11355 "Parser/parser.cc"
    break;

  case 862:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11361 "Parser/parser.cc"
    break;

  case 863:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11367 "Parser/parser.cc"
    break;

  case 864:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11373 "Parser/parser.cc"
    break;

  case 865:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11379 "Parser/parser.cc"
    break;

  case 866:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 867:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11391 "Parser/parser.cc"
    break;

  case 868:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 869:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11403 "Parser/parser.cc"
    break;

  case 871:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 872:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 873:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11421 "Parser/parser.cc"
    break;

  case 874:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 875:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 876:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 877:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 878:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11451 "Parser/parser.cc"
    break;

  case 879:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11457 "Parser/parser.cc"
    break;

  case 880:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11463 "Parser/parser.cc"
    break;

  case 881:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11469 "Parser/parser.cc"
    break;

  case 883:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11475 "Parser/parser.cc"
    break;

  case 884:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11481 "Parser/parser.cc"
    break;

  case 885:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 886:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11493 "Parser/parser.cc"
    break;

  case 887:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11499 "Parser/parser.cc"
    break;

  case 888:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11505 "Parser/parser.cc"
    break;

  case 889:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11511 "Parser/parser.cc"
    break;

  case 891:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11517 "Parser/parser.cc"
    break;

  case 892:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11523 "Parser/parser.cc"
    break;

  case 893:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11529 "Parser/parser.cc"
    break;

  case 894:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11535 "Parser/parser.cc"
    break;

  case 895:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11541 "Parser/parser.cc"
    break;

  case 896:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11547 "Parser/parser.cc"
    break;

  case 897:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11553 "Parser/parser.cc"
    break;

  case 898:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11559 "Parser/parser.cc"
    break;

  case 899:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11565 "Parser/parser.cc"
    break;

  case 901:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11571 "Parser/parser.cc"
    break;

  case 902:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11577 "Parser/parser.cc"
    break;

  case 903:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11583 "Parser/parser.cc"
    break;

  case 904:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11589 "Parser/parser.cc"
    break;

  case 906:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11595 "Parser/parser.cc"
    break;

  case 907:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11601 "Parser/parser.cc"
    break;

  case 908:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11607 "Parser/parser.cc"
    break;

  case 909:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11613 "Parser/parser.cc"
    break;

  case 910:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11619 "Parser/parser.cc"
    break;

  case 911:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11625 "Parser/parser.cc"
    break;

  case 912:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11631 "Parser/parser.cc"
    break;

  case 913:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11637 "Parser/parser.cc"
    break;

  case 915:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11643 "Parser/parser.cc"
    break;

  case 916:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11649 "Parser/parser.cc"
    break;

  case 917:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11655 "Parser/parser.cc"
    break;

  case 918:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11661 "Parser/parser.cc"
    break;

  case 919:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11667 "Parser/parser.cc"
    break;

  case 920:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11673 "Parser/parser.cc"
    break;

  case 922:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 924:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11685 "Parser/parser.cc"
    break;

  case 925:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 926:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11697 "Parser/parser.cc"
    break;

  case 927:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11703 "Parser/parser.cc"
    break;

  case 928:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11709 "Parser/parser.cc"
    break;

  case 929:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11715 "Parser/parser.cc"
    break;

  case 931:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 932:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11727 "Parser/parser.cc"
    break;

  case 933:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 934:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11739 "Parser/parser.cc"
    break;

  case 935:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11745 "Parser/parser.cc"
    break;

  case 936:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11751 "Parser/parser.cc"
    break;

  case 937:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 939:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11763 "Parser/parser.cc"
    break;

  case 940:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11769 "Parser/parser.cc"
    break;

  case 941:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11775 "Parser/parser.cc"
    break;

  case 942:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 943:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11787 "Parser/parser.cc"
    break;

  case 946:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 949:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 950:
#line 3634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11805 "Parser/parser.cc"
    break;

  case 951:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11811 "Parser/parser.cc"
    break;

  case 952:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11817 "Parser/parser.cc"
    break;

  case 953:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 954:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11829 "Parser/parser.cc"
    break;

  case 955:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 956:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11841 "Parser/parser.cc"
    break;

  case 957:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11847 "Parser/parser.cc"
    break;

  case 958:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11853 "Parser/parser.cc"
    break;

  case 959:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11859 "Parser/parser.cc"
    break;

  case 960:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11865 "Parser/parser.cc"
    break;

  case 961:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 962:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 963:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11883 "Parser/parser.cc"
    break;

  case 964:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11889 "Parser/parser.cc"
    break;

  case 965:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11895 "Parser/parser.cc"
    break;

  case 966:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11901 "Parser/parser.cc"
    break;

  case 967:
#line 3680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11907 "Parser/parser.cc"
    break;

  case 968:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11913 "Parser/parser.cc"
    break;

  case 970:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11919 "Parser/parser.cc"
    break;

  case 974:
#line 3720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 975:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11931 "Parser/parser.cc"
    break;

  case 976:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11937 "Parser/parser.cc"
    break;

  case 977:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11943 "Parser/parser.cc"
    break;

  case 978:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11949 "Parser/parser.cc"
    break;

  case 979:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11955 "Parser/parser.cc"
    break;

  case 980:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11961 "Parser/parser.cc"
    break;

  case 981:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 982:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11973 "Parser/parser.cc"
    break;

  case 983:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11979 "Parser/parser.cc"
    break;

  case 984:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11985 "Parser/parser.cc"
    break;

  case 985:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11991 "Parser/parser.cc"
    break;

  case 986:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11997 "Parser/parser.cc"
    break;

  case 987:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12003 "Parser/parser.cc"
    break;

  case 988:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12009 "Parser/parser.cc"
    break;

  case 989:
#line 3763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12015 "Parser/parser.cc"
    break;

  case 990:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12021 "Parser/parser.cc"
    break;

  case 993:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12027 "Parser/parser.cc"
    break;

  case 994:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12033 "Parser/parser.cc"
    break;


#line 12037 "Parser/parser.cc"

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
