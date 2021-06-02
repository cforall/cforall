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
#line 42 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
	//    i.e., u"a" U"b" L"c" is disallowed.

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

DeclarationNode * distAttr( DeclarationNode * specifier, DeclarationNode * declList ) {
	// distribute declaration_specifier across all declared variables, e.g., static, const, __attribute__.
	DeclarationNode * cur = declList, * cl = (new DeclarationNode)->addType( specifier );
	for ( cur = dynamic_cast<DeclarationNode *>( cur->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur );
	} // for
	declList->addType( cl );
	return declList;
} // distAttr

void distExt( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
	for ( DeclarationNode *iter = declaration; iter != nullptr; iter = (DeclarationNode *)iter->get_next() ) {
		iter->set_extension( true );
	} // for
} // distExt

void distInl( DeclarationNode * declaration ) {
	// distribute EXTENSION across all declarations
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
			typeSpec->type->print( ss );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		fieldList = DeclarationNode::newName( nullptr );
	} // if
	return distAttr( typeSpec, fieldList );				// mark all fields in list
} // fieldDecl

ForCtrl * forCtrl( ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ConstantExpr * constant = dynamic_cast<ConstantExpr *>(type->expr.get());
	if ( constant && (constant->get_constant()->get_value() == "0" || constant->get_constant()->get_value() == "1") ) {
		type = new ExpressionNode( new CastExpr( maybeMoveBuild<Expression>(type), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) ) );
	} // if
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

#line 258 "Parser/parser.cc"

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
    ZERO_T = 298,
    ONE_T = 299,
    SIZEOF = 300,
    TYPEOF = 301,
    VALIST = 302,
    AUTO_TYPE = 303,
    OFFSETOF = 304,
    BASETYPEOF = 305,
    TYPEID = 306,
    ENUM = 307,
    STRUCT = 308,
    UNION = 309,
    EXCEPTION = 310,
    GENERATOR = 311,
    COROUTINE = 312,
    MONITOR = 313,
    THREAD = 314,
    OTYPE = 315,
    FTYPE = 316,
    DTYPE = 317,
    TTYPE = 318,
    TRAIT = 319,
    LABEL = 320,
    SUSPEND = 321,
    ATTRIBUTE = 322,
    EXTENSION = 323,
    IF = 324,
    ELSE = 325,
    SWITCH = 326,
    CASE = 327,
    DEFAULT = 328,
    DO = 329,
    WHILE = 330,
    FOR = 331,
    BREAK = 332,
    CONTINUE = 333,
    GOTO = 334,
    RETURN = 335,
    CHOOSE = 336,
    FALLTHRU = 337,
    FALLTHROUGH = 338,
    WITH = 339,
    WHEN = 340,
    WAITFOR = 341,
    DISABLE = 342,
    ENABLE = 343,
    TRY = 344,
    THROW = 345,
    THROWRESUME = 346,
    AT = 347,
    ASM = 348,
    ALIGNAS = 349,
    ALIGNOF = 350,
    GENERIC = 351,
    STATICASSERT = 352,
    IDENTIFIER = 353,
    QUOTED_IDENTIFIER = 354,
    TYPEDEFname = 355,
    TYPEGENname = 356,
    TIMEOUT = 357,
    WOR = 358,
    CATCH = 359,
    RECOVER = 360,
    CATCHRESUME = 361,
    FIXUP = 362,
    FINALLY = 363,
    INTEGERconstant = 364,
    CHARACTERconstant = 365,
    STRINGliteral = 366,
    DIRECTIVE = 367,
    FLOATING_DECIMALconstant = 368,
    FLOATING_FRACTIONconstant = 369,
    FLOATINGconstant = 370,
    ARROW = 371,
    ICR = 372,
    DECR = 373,
    LS = 374,
    RS = 375,
    LE = 376,
    GE = 377,
    EQ = 378,
    NE = 379,
    ANDAND = 380,
    OROR = 381,
    ELLIPSIS = 382,
    EXPassign = 383,
    MULTassign = 384,
    DIVassign = 385,
    MODassign = 386,
    PLUSassign = 387,
    MINUSassign = 388,
    LSassign = 389,
    RSassign = 390,
    ANDassign = 391,
    ERassign = 392,
    ORassign = 393,
    ErangeUpEq = 394,
    ErangeDown = 395,
    ErangeDownEq = 396,
    ATassign = 397,
    THEN = 398
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
#define ZERO_T 298
#define ONE_T 299
#define SIZEOF 300
#define TYPEOF 301
#define VALIST 302
#define AUTO_TYPE 303
#define OFFSETOF 304
#define BASETYPEOF 305
#define TYPEID 306
#define ENUM 307
#define STRUCT 308
#define UNION 309
#define EXCEPTION 310
#define GENERATOR 311
#define COROUTINE 312
#define MONITOR 313
#define THREAD 314
#define OTYPE 315
#define FTYPE 316
#define DTYPE 317
#define TTYPE 318
#define TRAIT 319
#define LABEL 320
#define SUSPEND 321
#define ATTRIBUTE 322
#define EXTENSION 323
#define IF 324
#define ELSE 325
#define SWITCH 326
#define CASE 327
#define DEFAULT 328
#define DO 329
#define WHILE 330
#define FOR 331
#define BREAK 332
#define CONTINUE 333
#define GOTO 334
#define RETURN 335
#define CHOOSE 336
#define FALLTHRU 337
#define FALLTHROUGH 338
#define WITH 339
#define WHEN 340
#define WAITFOR 341
#define DISABLE 342
#define ENABLE 343
#define TRY 344
#define THROW 345
#define THROWRESUME 346
#define AT 347
#define ASM 348
#define ALIGNAS 349
#define ALIGNOF 350
#define GENERIC 351
#define STATICASSERT 352
#define IDENTIFIER 353
#define QUOTED_IDENTIFIER 354
#define TYPEDEFname 355
#define TYPEGENname 356
#define TIMEOUT 357
#define WOR 358
#define CATCH 359
#define RECOVER 360
#define CATCHRESUME 361
#define FIXUP 362
#define FINALLY 363
#define INTEGERconstant 364
#define CHARACTERconstant 365
#define STRINGliteral 366
#define DIRECTIVE 367
#define FLOATING_DECIMALconstant 368
#define FLOATING_FRACTIONconstant 369
#define FLOATINGconstant 370
#define ARROW 371
#define ICR 372
#define DECR 373
#define LS 374
#define RS 375
#define LE 376
#define GE 377
#define EQ 378
#define NE 379
#define ANDAND 380
#define OROR 381
#define ELLIPSIS 382
#define EXPassign 383
#define MULTassign 384
#define DIVassign 385
#define MODassign 386
#define PLUSassign 387
#define MINUSassign 388
#define LSassign 389
#define RSassign 390
#define ANDassign 391
#define ERassign 392
#define ORassign 393
#define ErangeUpEq 394
#define ErangeDown 395
#define ErangeDownEq 396
#define ATassign 397
#define THEN 398

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

	Token tok;
	ParseNode * pn;
	ExpressionNode * en;
	DeclarationNode * decl;
	AggregateDecl::Aggregate aggKey;
	TypeDecl::Kind tclass;
	StatementNode * sn;
	WaitForStmt * wfs;
	Expression * constant;
	IfCtrl * ifctl;
	ForCtrl * fctl;
	enum OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	CatchStmt::Kind catch_kind;
	GenericExpr * genexpr;

#line 618 "Parser/parser.cc"

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
#define YYFINAL  140
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   18950

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  171
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  286
/* YYNRULES -- Number of rules.  */
#define YYNRULES  967
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1973

#define YYUNDEFTOK  2
#define YYMAXUTOK   398


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
       2,     2,     2,   160,     2,     2,     2,   164,   157,     2,
     145,   147,   156,   158,   151,   159,   148,   163,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   152,   170,
     165,   169,   166,   168,   146,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   149,   162,   150,   155,     2,   154,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   153,   167,   144,   161,     2,     2,     2,
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
     135,   136,   137,   138,   139,   140,   141,   142,   143
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   532,   532,   536,   543,   544,   545,   546,   547,   551,
     552,   553,   554,   555,   556,   557,   561,   562,   563,   568,
     572,   573,   584,   586,   588,   589,   591,   593,   595,   597,
     610,   611,   621,   626,   631,   632,   635,   641,   647,   649,
     651,   653,   655,   657,   659,   661,   663,   665,   667,   669,
     671,   673,   675,   677,   679,   689,   690,   691,   696,   699,
     703,   704,   708,   709,   711,   713,   715,   717,   719,   724,
     726,   728,   736,   737,   745,   748,   749,   751,   756,   772,
     774,   776,   778,   780,   782,   784,   786,   788,   796,   797,
     799,   803,   804,   805,   806,   810,   811,   813,   815,   817,
     819,   821,   823,   825,   832,   833,   834,   835,   839,   840,
     844,   845,   850,   851,   853,   855,   860,   861,   863,   868,
     869,   871,   876,   877,   879,   881,   883,   888,   889,   891,
     896,   897,   902,   903,   908,   909,   914,   915,   920,   921,
     926,   927,   930,   935,   940,   941,   949,   955,   956,   960,
     961,   965,   966,   970,   971,   972,   973,   974,   975,   976,
     977,   978,   979,   980,   990,   992,   997,   998,  1000,  1002,
    1007,  1008,  1014,  1015,  1021,  1022,  1023,  1024,  1025,  1026,
    1027,  1028,  1029,  1030,  1031,  1033,  1034,  1040,  1045,  1047,
    1055,  1056,  1061,  1063,  1065,  1067,  1069,  1073,  1074,  1079,
    1086,  1088,  1090,  1100,  1102,  1110,  1113,  1118,  1120,  1122,
    1124,  1132,  1133,  1135,  1139,  1141,  1145,  1146,  1157,  1158,
    1162,  1167,  1168,  1172,  1174,  1179,  1181,  1183,  1185,  1187,
    1189,  1194,  1195,  1217,  1219,  1221,  1224,  1227,  1230,  1232,
    1234,  1236,  1239,  1242,  1244,  1247,  1254,  1256,  1258,  1260,
    1262,  1267,  1269,  1271,  1273,  1278,  1280,  1285,  1287,  1289,
    1291,  1294,  1298,  1301,  1305,  1307,  1309,  1311,  1313,  1315,
    1317,  1319,  1321,  1323,  1325,  1330,  1331,  1335,  1343,  1348,
    1353,  1354,  1358,  1362,  1367,  1368,  1374,  1378,  1380,  1382,
    1384,  1387,  1389,  1394,  1396,  1401,  1403,  1405,  1410,  1412,
    1418,  1419,  1423,  1424,  1425,  1426,  1430,  1435,  1436,  1438,
    1440,  1442,  1446,  1450,  1451,  1455,  1457,  1459,  1461,  1463,
    1469,  1470,  1476,  1477,  1481,  1482,  1487,  1489,  1495,  1496,
    1498,  1503,  1508,  1519,  1520,  1524,  1525,  1531,  1532,  1536,
    1538,  1542,  1544,  1548,  1549,  1553,  1554,  1558,  1559,  1560,
    1564,  1566,  1581,  1582,  1583,  1584,  1586,  1590,  1592,  1596,
    1603,  1605,  1607,  1612,  1613,  1615,  1617,  1619,  1651,  1654,
    1659,  1661,  1667,  1672,  1677,  1688,  1693,  1698,  1703,  1708,
    1717,  1721,  1728,  1730,  1731,  1732,  1738,  1740,  1745,  1746,
    1747,  1756,  1757,  1758,  1762,  1763,  1764,  1773,  1774,  1775,
    1780,  1781,  1790,  1791,  1796,  1797,  1801,  1803,  1805,  1807,
    1809,  1813,  1818,  1819,  1821,  1831,  1832,  1837,  1839,  1841,
    1843,  1845,  1848,  1850,  1852,  1857,  1859,  1861,  1863,  1865,
    1867,  1869,  1871,  1873,  1875,  1877,  1879,  1881,  1883,  1885,
    1887,  1889,  1891,  1893,  1895,  1897,  1899,  1901,  1903,  1905,
    1910,  1911,  1915,  1921,  1922,  1928,  1929,  1931,  1933,  1935,
    1940,  1942,  1947,  1948,  1950,  1952,  1957,  1959,  1961,  1963,
    1965,  1967,  1972,  1973,  1975,  1977,  1982,  1984,  1983,  1987,
    1995,  1996,  1998,  2000,  2005,  2006,  2008,  2013,  2014,  2016,
    2018,  2023,  2024,  2026,  2031,  2033,  2035,  2037,  2038,  2040,
    2045,  2047,  2049,  2054,  2055,  2059,  2060,  2065,  2064,  2069,
    2068,  2076,  2075,  2086,  2085,  2095,  2100,  2101,  2106,  2112,
    2126,  2127,  2131,  2133,  2135,  2141,  2143,  2145,  2147,  2149,
    2151,  2153,  2155,  2161,  2162,  2167,  2169,  2171,  2180,  2182,
    2183,  2184,  2186,  2188,  2189,  2194,  2195,  2196,  2201,  2203,
    2206,  2213,  2214,  2215,  2221,  2226,  2228,  2234,  2235,  2241,
    2242,  2246,  2251,  2254,  2253,  2257,  2260,  2266,  2265,  2274,
    2280,  2284,  2286,  2291,  2293,  2295,  2297,  2303,  2306,  2312,
    2313,  2315,  2316,  2317,  2319,  2321,  2328,  2329,  2331,  2333,
    2338,  2339,  2345,  2346,  2348,  2349,  2354,  2355,  2356,  2358,
    2366,  2367,  2369,  2372,  2374,  2378,  2379,  2380,  2382,  2384,
    2389,  2391,  2396,  2398,  2407,  2409,  2414,  2415,  2416,  2420,
    2421,  2422,  2427,  2428,  2433,  2434,  2435,  2439,  2440,  2445,
    2446,  2447,  2448,  2449,  2464,  2465,  2470,  2471,  2477,  2479,
    2482,  2484,  2486,  2509,  2510,  2516,  2517,  2523,  2522,  2532,
    2531,  2535,  2541,  2547,  2548,  2550,  2552,  2557,  2559,  2561,
    2563,  2569,  2570,  2574,  2575,  2580,  2582,  2589,  2591,  2593,
    2595,  2601,  2603,  2605,  2610,  2612,  2617,  2622,  2630,  2632,
    2637,  2638,  2643,  2644,  2648,  2649,  2650,  2655,  2657,  2663,
    2665,  2670,  2672,  2678,  2679,  2683,  2687,  2691,  2693,  2694,
    2695,  2700,  2703,  2702,  2714,  2713,  2725,  2724,  2736,  2735,
    2749,  2755,  2757,  2763,  2764,  2769,  2776,  2781,  2787,  2790,
    2793,  2797,  2803,  2806,  2809,  2814,  2815,  2816,  2820,  2826,
    2827,  2837,  2838,  2842,  2843,  2848,  2853,  2854,  2860,  2861,
    2863,  2868,  2869,  2870,  2871,  2872,  2874,  2909,  2911,  2916,
    2918,  2919,  2921,  2926,  2928,  2930,  2932,  2937,  2939,  2941,
    2943,  2945,  2947,  2949,  2954,  2956,  2958,  2960,  2969,  2971,
    2972,  2977,  2979,  2981,  2983,  2985,  2990,  2992,  2994,  2996,
    3001,  3003,  3005,  3007,  3009,  3011,  3023,  3024,  3025,  3029,
    3031,  3033,  3035,  3037,  3042,  3044,  3046,  3048,  3053,  3055,
    3057,  3059,  3061,  3063,  3078,  3083,  3088,  3090,  3091,  3093,
    3098,  3100,  3102,  3104,  3109,  3111,  3113,  3115,  3117,  3119,
    3121,  3126,  3128,  3130,  3132,  3134,  3144,  3146,  3148,  3149,
    3151,  3156,  3158,  3160,  3165,  3167,  3169,  3171,  3176,  3178,
    3180,  3194,  3196,  3198,  3199,  3201,  3206,  3208,  3213,  3215,
    3217,  3222,  3224,  3229,  3231,  3248,  3249,  3251,  3256,  3258,
    3260,  3262,  3264,  3269,  3270,  3272,  3274,  3279,  3281,  3283,
    3289,  3291,  3293,  3296,  3300,  3302,  3304,  3306,  3340,  3341,
    3343,  3345,  3350,  3352,  3354,  3356,  3358,  3363,  3364,  3366,
    3368,  3373,  3375,  3377,  3383,  3384,  3386,  3395,  3398,  3400,
    3403,  3405,  3407,  3421,  3422,  3424,  3429,  3431,  3433,  3435,
    3437,  3442,  3443,  3445,  3447,  3452,  3454,  3462,  3463,  3464,
    3469,  3470,  3475,  3477,  3479,  3481,  3483,  3485,  3492,  3494,
    3496,  3498,  3500,  3503,  3505,  3507,  3509,  3511,  3516,  3518,
    3520,  3525,  3551,  3552,  3554,  3558,  3559,  3563,  3565,  3567,
    3569,  3571,  3573,  3580,  3582,  3584,  3586,  3588,  3590,  3595,
    3597,  3599,  3606,  3608,  3626,  3628,  3633,  3634
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
  "uFLOAT32X", "uFLOAT64", "uFLOAT64X", "uFLOAT128", "ZERO_T", "ONE_T",
  "SIZEOF", "TYPEOF", "VALIST", "AUTO_TYPE", "OFFSETOF", "BASETYPEOF",
  "TYPEID", "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR",
  "COROUTINE", "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE",
  "TRAIT", "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE",
  "SWITCH", "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE",
  "GOTO", "RETURN", "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN",
  "WAITFOR", "DISABLE", "ENABLE", "TRY", "THROW", "THROWRESUME", "AT",
  "ASM", "ALIGNAS", "ALIGNOF", "GENERIC", "STATICASSERT", "IDENTIFIER",
  "QUOTED_IDENTIFIER", "TYPEDEFname", "TYPEGENname", "TIMEOUT", "WOR",
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
  "quasi_keyword", "identifier", "string_literal", "string_literal_list",
  "primary_expression", "generic_assoc_list", "generic_association",
  "postfix_expression", "argument_expression_list_opt",
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
  "selection_statement", "if_statement", "if_control_expression",
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
  "external_definition", "$@10", "$@11", "$@12", "$@13",
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
     395,   396,   397,   398,   125,    40,    64,    41,    46,    91,
      93,    44,    58,   123,    96,    94,    42,    38,    43,    45,
      33,   126,    92,    47,    37,    60,    62,   124,    63,    61,
      59
};
# endif

#define YYPACT_NINF (-1638)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-848)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     164, 11538,   196,   420, 15497,   145, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638,   234,   745,   293,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638,    80,   459, -1638, -1638, -1638, -1638,
   -1638, -1638,  4475,  4475,   423, 11538,   432,   458, -1638, -1638,
     476, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
    1959, -1638,   768,   415, -1638, -1638, -1638, -1638, 15351, -1638,
   -1638,   463,   484,    93,   457, -1638,  4475,   484,   484,   484,
     471,  3943,   644,   741, 11693, -1638, -1638, -1638, 15205,  1190,
   -1638, -1638, -1638,  2003,   660,  6149,   762,  1050,  2003,  1125,
     540, -1638, -1638, -1638, -1638,   623, -1638, -1638, -1638, -1638,
     549, -1638, -1638, -1638, -1638, -1638,   576,   570,   623, -1638,
     623,   579, -1638, -1638, -1638, 16139,  4475, -1638, -1638,  4475,
   -1638, 11538,   578, 16285, -1638, -1638,  3957, 17502, -1638,  1168,
    1168, -1638,  2968, -1638, -1638, -1638, -1638, 14755, 13851,  3477,
     623, -1638, -1638, -1638, -1638, -1638, -1638,   600, -1638,   616,
     619,   645, -1638,   685, 18434, 14455,  3554,  1959,   246,   656,
     668,   671,   688,   691,   693, -1638, -1638, 16431, 10905,   698,
   -1638, 15636, -1638, -1638, -1638, -1638,   706, -1638, -1638,   715,
   -1638,   860,  4623, -1638,   739,  4475,   570,   744,   740,   747,
     761, -1638, -1638, -1638,  3400,  3044,   803,   843,   259, -1638,
   -1638,   623,   623,    48,    87,   309,    48, -1638,   623,   623,
   -1638,  3198, -1638, -1638,   751,   837,  1168, 18221, -1638, -1638,
   15351, -1638, -1638,  2003, -1638,  1389,   540,   820,   877,    87,
    4475,    93, -1638, 12976, -1638,  1168,  1168,   832,   877,    87,
    4475, -1638, 11740, -1638, -1638,  1168, -1638,  1168, -1638,   842,
    4267,  4475, -1638,  2116,   852, -1638, -1638, -1638, 15791,   570,
     159, -1638, -1638, 17641, -1638,   843,    60, -1638, 18434, 17502,
    3617,  3198, -1638,   314, -1638, -1638, -1638, 16285,  4475,   870,
   -1638, -1638, -1638, -1638,  4475,  3805,   258,   683, -1638,  4475,
     616, -1638, 18505,   867,   880, 18434, 18576,   893, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, 18647, 18647, 14305,   817,  4023,
   -1638, -1638, -1638, -1638,   851, -1638,   887,   932, -1638,  1227,
    3147, 14755, 18434, -1638,   884,   625,  1084,  1126,   544,  1130,
     892,   900,   894,   943,    98, -1638, -1638, -1638,   548,   922,
   -1638, -1638,   690, -1638, -1638,   623,   946, 16577,   888, 14005,
   18277,  2003,  2003, -1638,  2003,  1168,  2003,  1168, -1638, -1638,
     623, -1638,   951, -1638, 16723, -1638, -1638, -1638, 16869,   706,
   -1638,   955,   354,   721,   958,   540,   970, -1638,  2968,   975,
     616,  2968,  2494,  1002,  1012, -1638, 18434, -1638,   727,   922,
   -1638,   790,  3554,  1018,  1021,  1029,  1031,  1042,  1063, -1638,
   -1638,   328,  1064, -1638,   835,  1064, -1638, -1638, 16139, -1638,
    1118,  1066, 14905, -1638, -1638,  4377,  3715,  1091, 14005,  1103,
     560,   670, -1638, -1638, -1638, -1638, -1638,  4475,  4406, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638,  5042, -1638, -1638,  7726,
    1094, -1638, -1638, -1638, -1638, -1638,  3400,   448,  1097,  1102,
    1108,   667,  1110,  1112,  1116,  3044, -1638, -1638,   623,  1128,
      93,  1111, -1638, -1638,  1132, -1638, -1638,   570,   877, -1638,
   -1638, -1638,   570, -1638, -1638,  3198, -1638, 14755, 14755, -1638,
    1168,  3957,  6542, 13851, -1638, -1638, -1638, -1638, -1638,   570,
     877,    60, -1638, -1638,  2003,  1122,   877,    87, -1638,   570,
     877, -1638, 12205, -1638,  1168,  1168, -1638, -1638,  1135,   417,
    1150,   540,  1152, -1638, 17795, -1638,   836, -1638,  1259, 18124,
   -1638,  3957,  6829, 18221, -1638, 15791, 18718, -1638, -1638, -1638,
   -1638, -1638,  3617,   752,  3198, -1638, 13851,   843, -1638,  1198,
   -1638,  1195, -1638, -1638, -1638, -1638, -1638,  2968, -1638, -1638,
   14605, -1638, 17015, 17015, -1638, 14605, -1638, 18434, 14605, -1638,
   -1638, 15847, 17015, 17015,   817,  1210,  1500,   556,  1519, -1638,
     850,  1203,  1114,  1205, -1638,  7726, 10905,  7991,  1201,  2116,
    2116, -1638, -1638,   976, -1638, -1638,  8554,  2734, 18434,  8554,
    2116, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638,  1214, 18434, -1638, -1638, -1638, -1638, 18434, 18434,
   18434, 18434, 18434, 18434, 18434, 18434, 18434, 18434, 18434, 18434,
   18434, 18434, 18434, 18434, 18434, 18434, 18434,  9915,   548,   795,
   -1638, -1638,   623,   623, -1638,  3907,  1283, 14755,  4390, 16869,
   10905, -1638, 17161, -1638,  1168,  1168, -1638, -1638,   706, -1638,
     571,  1213,  1352, 18434,   903,  1132,  1202, -1638,   623,   623,
   -1638,  1064, -1638, 16577, -1638, -1638, 18068,  1168,  1168, -1638,
    4390,   623, -1638, 17453, -1638, -1638, 16723, -1638,   530,  1226,
     176,  1229,   721,   857, 16285,   859, -1638, -1638, -1638, -1638,
   -1638, -1638,   864, -1638,  1242,  1230, 18434,  1255,   685, -1638,
   -1638,   338,  1064, -1638,   872,  1064, -1638, -1638, -1638,  1132,
   -1638, -1638,  1132, 18789, -1638, -1638, 10905,  1243,  1247,  2863,
    1362,  2790,   372,  1202, -1638,   623,   623,  1202,   394, -1638,
     623,   623, 18434,  4475,  1127,  1134,  1202,   130, 13389, 13389,
    4475,  1287,  4236,  1012,  1298,  1300, -1638,  1253,  4623,   384,
   -1638, -1638, -1638,   871, -1638, 13389,  2116,  3957,  2116,   890,
    1264,  1275,  1288,   891,  1322,  1328,  1333,   414,  1064, -1638,
   -1638,   442,  1064, -1638, -1638, -1638,  3957,   685, -1638,  1064,
   18789, -1638,   570, 17795, -1638, -1638,   907,  1334,   926,  1344,
   -1638,  1311, -1638,   570, -1638, -1638,   570,   877,  1311, -1638,
     570,  1339,  1340,  1341, -1638, -1638, 18068, -1638,  1348, -1638,
   -1638, -1638,  2116,  4475, 10368,  1435,  1331, 17690, -1638,  1066,
   -1638, 13389,   952, -1638,  1311, -1638, 16285, 14755,  1335, -1638,
    1335,  1355,   415,  1357,  1356,  1371,  1372,  1375, 18434,  1378,
    1381,  1382, 10905, 18434, -1638, -1638,  1609, -1638, -1638, -1638,
   18434, -1638,  1393,  1394,  6903,  1142, -1638,  8554, -1638, -1638,
   -1638,  3501, -1638, -1638,   962, -1638, -1638, -1638,  3501, -1638,
   -1638,  1144,   551, -1638,  5042, -1638, -1638,   884,   884,   884,
     625,   625,  1084,  1084,  1126,  1126,  1126,  1126,   544,   544,
    1130,   892,   900,   894,   943, 18434,  1147, 17795,  1395,  1398,
    1399,   795, -1638, -1638, -1638,  3501, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, 16723, -1638, 11063, 15055, -1638, 17795,  1400,
    1402,  1404, -1638,  5351,   623, -1638,   903, -1638, -1638, -1638,
   -1638,  1132, -1638, -1638, -1638,  1168, -1638,  3969, -1638, -1638,
     540,  2254,  1415, -1638,  4623, -1638,   721,  1226, -1638, -1638,
    1407,  1414,  2494,  8554, -1638, -1638, -1638,  1419, -1638, -1638,
   -1638,  1132, -1638, -1638,  1132, 17795, 17795, -1638, -1638,  2863,
     757,  1420,  1422,  1424,  1426,  2988,  2790, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638,  1433, -1638,  1202, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638,  1427,  1429, -1638,    93, -1638, -1638, 18434, -1638,
    5042,  1442, -1638,  1569, -1638, -1638, -1638, -1638,  9340, 13389,
   -1638, -1638, -1638,  1421,   451,  1064, -1638,   468,  1064, -1638,
   -1638, -1638, -1638,  1132, -1638, -1638, -1638,  1132,   843,  1441,
    1132, -1638, -1638, -1638, -1638, -1638, -1638, -1638,  1447, -1638,
   -1638,  1311, -1638,   570, -1638, -1638, -1638, -1638, -1638,  8318,
    1448,  1446, -1638,    52, -1638,   466,   190, 10747,  1453, 13068,
    1455,  1456,  3118,  3388,  2182, 10184,  1458, -1638, -1638,  1459,
    1461, -1638, -1638,   570, 18434, 18434,  1594,  1457,   566, -1638,
    1542,  1462,  1444, -1638, -1638, -1638, 10113, -1638, -1638, -1638,
   -1638, -1638,  2131, -1638, -1638, -1638,  1532, -1638, -1638, -1638,
    2116, -1638, -1638, 12158, 15351,  1467, -1638,  4475, -1638,  1460,
    1479,  1482, -1638,  1162, -1638, -1638, -1638,  3957, -1638, -1638,
    1463,  1466,   963, 16285,   616,   616,   580, 10905,  2116, -1638,
     580, 15993,   580, -1638, 18434, 18434, 18434, -1638, -1638, -1638,
   -1638, 18434, 18434,  1485,  5042, -1638, -1638,  1170,   599, -1638,
    4038, -1638, -1638,  1172, -1638,    75, -1638,  8554,  1178, -1638,
   18434, -1638,  1253, -1638, 18434, -1638,   477,  1064, -1638, -1638,
    1208, -1638, -1638,  1012,  1066, 14905, -1638,   922, -1638, 11221,
   -1638,   489,  1064, -1638,  1168,  5686, -1638, -1638,   721,   623,
     623,   530,   176, -1638, -1638,  1226,  1495,  1498, -1638, -1638,
     969,  1496,  1476, 17795, 17795, -1638, -1638,  1502,   494,  1064,
   -1638,   522,  2146,   623,   623, -1638, -1638, 17795, 17795, -1638,
    1501, -1638, 13851, 13851,  1503,  1504,  1505,  1506, -1638,  3501,
    1215,    46, -1638, -1638, -1638,  4623, -1638, 18434, -1638, -1638,
   -1638,  1510, 18434, -1638, -1638, -1638,  1132, -1638, -1638, -1638,
    1132, 17795, 17795,    93,   623,  1223,  1511,  1508, -1638, -1638,
    1515, 12307, 12456, 12605, 16285, 17015, 17015,  1516, -1638,  1493,
    1499,  2034,  9545, -1638,   266,  4475, -1638, -1638,  4475, -1638,
    8554,   205,   322, -1638, -1638, -1638, -1638, 18434,  1520,  1603,
    1533,  1534, -1638,  1514, -1638,  1521, 18434,  1525,  5042,  1526,
   18434,  7726, 18434,   822, -1638,  1528,    91, -1638,    49,  1540,
   -1638, -1638,  1558, -1638,  1530, -1638,  1535,  1561, 13068,   184,
   13235,   623,   431, -1638, -1638, -1638,  1560, -1638,  1559, -1638,
    1567, -1638,  1563, -1638,  1565, -1638, -1638, -1638, -1638,  1570,
    1571,   980, -1638,  1572, -1638, -1638, -1638, -1638, -1638,  5042,
    1253,  7726, -1638,  1598,  3501, -1638,  1598,  1598, -1638,  3501,
    4251,  4325, -1638, -1638,  1234,  1575, -1638,  1574, -1638, -1638,
   -1638,  1132, -1638, 11379,  1576,  1577,  1578, -1638,  1583, -1638,
   -1638, -1638,  1132, 18434, 18434,  1066,  1581, -1638,  1226, -1638,
    1579,   284, -1638,  1589, -1638, -1638, 16285, -1638, -1638, -1638,
   -1638,  1588,  1592,   623, -1638, -1638, -1638,  1132, -1638, -1638,
   -1638,  1595, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638,  1585, 18434, 18434,  1241,  1593,
   -1638, -1638,  1600,   623, -1638, 17795, 17795, -1638, -1638, -1638,
   -1638, 18434, -1638, -1638,  1601, -1638,  1516,  1516,  1516,   597,
    1552,   453, -1638,  3743,   467, 14755, -1638, -1638, -1638,  3645,
   18434,  3317,   491, -1638, -1638,    83,  1597,  1597,  4475, -1638,
   -1638, 17940, -1638,  1000, -1638, -1638, -1638, -1638,  1003,  1605,
   13068, 10747, 13068, 10579, -1638, -1638,   504, -1638,  1253, -1638,
    1011,  1013,  1019, -1638, -1638, -1638, -1638,   570,   822,  1607,
   -1638, -1638, 18434, -1638,  1608,   685, 10747, -1638, -1638, -1638,
   -1638, 18434,  1651, -1638, 13068, -1638,   623, 13851, -1638, -1638,
   16285, -1638, -1638, -1638, 18434, -1638, 15993, 18434,  1253,  1612,
    1245, -1638,  1252, -1638,  3501, -1638,  3501, -1638, -1638, -1638,
   -1638, 17795, -1638, -1638, -1638,  1611, -1638, 17795, -1638, -1638,
    1613, -1638,  1614,  1618,  1604,   721, -1638, -1638, -1638,  1619,
    1620, -1638, -1638, 17795, 17795,  1622,  1623,  1257, 13543, 13697,
    1621, -1638, -1638, -1638, -1638,  1625, -1638, -1638, -1638, -1638,
    1632,  1635,  1263, -1638, -1638, -1638, -1638,   597,  1471,   511,
   -1638, -1638, -1638, -1638,   623,   623, -1638, -1638, -1638,   543,
   -1638,  1024,  3645,   355, -1638,  3317,   623, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, 13068,   326, 18292, -1638,  1462,
    1636, 18434,   463,  1633,   471,  9744, 16285, -1638, 18434, 18434,
     794,   214, -1638, 18434, -1638,  1642,   507, 13068, -1638, -1638,
    1645, -1638, -1638,  1624,   685,   473,  1644,  1648,  1209,  1673,
   -1638, -1638, -1638,  4475,  3957, -1638, -1638, -1638,  1653, -1638,
   -1638, -1638,  1267,  1270, -1638, -1638, -1638,  1649,  1652, -1638,
   -1638, -1638,   721,  1226,  1654, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638,  1657, -1638, -1638,  1656,  1660,  1662,
   -1638, -1638, -1638,  1663,  1668,  1670,  1471, -1638,   623, -1638,
   -1638, -1638, -1638, -1638,  1669,  3743, -1638, 18434,  1675, -1638,
   -1638, 12692, -1638,  1631,  1055, 13068,  1462, 14159,  1462,  1664,
   -1638, -1638, -1638, -1638,  4140, 18434, 13068, 10579,  1665,  1667,
   -1638, -1638, -1638, -1638, 17307, -1638,  1688,  1671,    59, 13068,
   -1638, 18434,  8554,   692, -1638, -1638, -1638, -1638, -1638, -1638,
    1696,  1703, -1638, -1638,  1226,  1678, -1638,  1707,  1708, 13851,
    1705, -1638, -1638, -1638,   527,  1064, -1638, -1638,   597, -1638,
      73, -1638,  1276, -1638, -1638, 11848, -1638, -1638, -1638,  1687,
   -1638, 18434,  1710, 18434,   808,  1689,    66, -1638, -1638, 18434,
   -1638, 11848, 17307, -1638,  3872, 17161,  2116,  1711, -1638,  1764,
    1721,   498,  1716, -1638,  1798, -1638,  1058, 13068,  1724, 13068,
   13068, -1638, -1638,  1726, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638,  1132, -1638, 18434, 18434, -1638,  1358, 12003, -1638, -1638,
   -1638, -1638,  1462,  1730,  1731, 18434, 18434, 18434, -1638, -1638,
    1358, -1638,  1712,  2600,  2897, -1638, -1638, -1638,    59,  1728,
   18434,  1715,    59,    59, 13068, -1638, -1638, 18434,  1783,  1784,
   -1638, 17795, -1638, -1638, 12692, -1638,  1358, -1638,  1727,  1733,
     212, -1638,  1462, -1638,  1712, 18434,  1744,  2897,  1740,   685,
    1755, -1638,   627, -1638, -1638,  1061,  1673,   379, -1638, -1638,
   12809,  1759, 12692, 18434, 18363, 18434,  1760,  1758, -1638,   570,
     685,  1765, -1638,  1741,   685, -1638, -1638, 13068,  1842,  1766,
   -1638, -1638, 12809,  1462, -1638,  1462,  1462, -1638,   570, -1638,
   -1638,  1281, 18434, -1638,  1082, -1638, 13068, -1638, -1638,   685,
    2116,  1769,  1747, -1638, -1638, -1638,  1085, -1638, -1638,  1748,
    2116, -1638, -1638
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   400,     0,     2,   400,   417,   418,   419,   420,   421,
     422,   423,   424,   406,   408,   407,   409,     0,     0,     0,
     425,   427,   445,   428,   446,   431,   432,   443,   444,   426,
     441,   442,   429,   430,   433,   434,   435,   436,   437,   438,
     439,   440,   447,   448,   731,   450,   523,   524,   527,   529,
     525,   531,     0,     0,     0,   400,     0,     0,    16,   494,
     500,     9,    10,    11,    12,    13,    14,    15,   697,    90,
       0,    18,     0,     2,    88,    89,    17,   747,   400,   698,
     349,     0,   352,   624,   354,   363,     0,   353,   383,   384,
       0,     0,     0,     0,   477,   402,   404,   410,   400,   412,
     415,   462,   449,   388,   455,   460,   389,   472,   390,   487,
     491,   497,   476,   503,   515,   731,   520,   521,   504,   570,
     355,   356,     3,   699,   710,   405,     0,     0,   731,   769,
     731,     2,   786,   787,   788,   400,     0,   945,   946,     0,
       1,   400,     0,   400,   372,   373,     0,   477,   394,   395,
     396,   702,     0,   526,   528,   530,   532,   400,   400,     0,
     732,   733,   522,   451,   617,   618,   616,   676,   671,   661,
       0,     0,   700,     0,     0,   400,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   495,   498,   400,   400,     0,
     947,   477,   776,   794,   951,   944,   942,   949,   348,     0,
     152,   151,     0,   357,     0,     0,     0,     0,     0,     0,
       0,   347,   846,   847,     0,     0,   382,   729,   731,   725,
     750,   731,   731,   727,     2,   731,   726,   807,   731,   731,
     804,     0,   470,   471,     0,     0,   400,   400,   417,     2,
     400,   364,   403,   413,   463,     0,   492,     0,   713,     2,
       0,   624,   365,   477,   456,   473,   488,     0,   713,     2,
       0,   416,   457,   464,   465,   474,   479,   489,   493,     0,
     507,     0,   691,     2,     2,   711,   768,   770,   400,     0,
       2,     2,   955,   477,   958,   729,   729,     3,     0,   477,
       0,     0,   375,   731,   727,   726,     2,   400,     0,     0,
     657,   659,   658,   660,     0,     0,   653,     0,   643,     0,
     652,   663,     0,     0,     0,     0,     0,     0,    22,     4,
       8,    20,     5,     6,     7,     0,     0,   400,     2,     0,
      91,    92,    93,    94,    75,    23,    76,    19,    34,    74,
      95,   400,     0,   110,   112,   116,   119,   122,   127,   130,
     132,   134,   136,   138,   140,   144,   668,    24,   620,   491,
     622,   667,     0,   619,   623,   731,     2,   400,   966,   401,
     400,   412,   391,   455,   392,   480,   393,   487,   484,   505,
     731,   506,     0,   605,   400,   606,   920,   921,   400,   607,
     609,   494,   500,     0,   571,   572,     0,   734,     0,   674,
     662,     0,   738,     0,     2,    95,     0,   143,     0,     0,
     501,     0,     0,     0,     0,     0,     0,     0,     0,   748,
     774,   731,   784,   792,   796,   802,     2,   953,   400,   956,
       2,    88,   400,     3,   604,     0,   966,     0,   401,   455,
     480,   487,     3,     3,   586,   590,   600,   606,   607,     2,
     777,   795,   943,     2,     2,   626,   629,   627,   625,     0,
       0,   715,     2,     2,     2,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   753,   810,   731,     0,
     624,     2,   749,   757,   873,   751,   752,     0,   713,     2,
     806,   814,     0,   808,   809,     0,   378,   400,   400,   461,
     401,     0,   477,   400,   948,   952,   950,   478,   695,     0,
     713,   729,   358,   366,   414,     0,   713,     2,   695,     0,
     713,   672,   458,   459,   475,   490,   496,   499,   494,   500,
     518,   519,     0,   673,   400,   614,     0,   188,   341,   400,
       3,     0,   477,   400,   712,   400,     0,   360,     2,   361,
     692,   380,     0,     0,     0,     2,   400,   729,   695,     0,
       2,     0,   656,   655,   654,   649,   411,     0,   647,   664,
     400,    82,   400,   400,    77,   400,    84,     0,   400,    80,
      81,   400,   400,   400,     2,    91,    92,     0,     0,   170,
       0,     0,   521,     0,   942,     0,   400,     0,     0,     0,
       0,    21,    44,     0,    50,    51,    55,     0,     0,    55,
       0,   153,   154,   155,   156,   157,   158,   159,   160,   161,
     162,   163,   151,     0,   149,   150,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     2,   858,
     621,   855,   731,   731,   863,     0,   453,   400,     0,   400,
     400,   922,   401,   397,   398,   399,   926,   917,   918,   924,
       2,     2,    89,     0,   882,   896,   966,   878,   731,   731,
     887,   894,   612,   400,   485,   608,   401,   481,   482,   486,
       0,   731,   932,   401,   937,   929,   400,   934,     0,   964,
     577,     0,     0,     0,   400,     0,   746,   745,   741,   743,
     744,   742,     0,   736,   739,     0,     0,     0,     0,   502,
     775,   731,   785,   793,   797,   803,     2,   778,   780,   782,
       2,   798,   800,     0,   954,   957,   400,     0,     0,     2,
      89,   882,   731,   966,   828,   731,   731,   966,   731,   843,
     731,   731,     3,   608,     0,     0,   966,   966,   400,   400,
       0,    22,     0,     2,    23,     0,   630,   964,     0,     0,
     636,   167,   166,     0,     2,   400,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   731,   762,   766,
     805,   731,   819,   824,   754,   811,     0,     0,   386,   870,
       0,   716,     0,   400,   717,   379,     0,     0,     0,     0,
     377,     2,   718,     0,   362,   695,     0,   713,     2,   719,
       0,     0,     0,     0,   533,   593,   401,     3,     3,   597,
     596,   789,     0,     0,   400,   342,     0,   477,     3,    88,
       3,   400,     0,     3,     2,   651,   400,   400,   645,   644,
     645,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   400,     0,   109,   108,     0,   105,   104,    25,
       0,    26,     0,     0,     0,     0,     3,    55,    40,    41,
      48,     0,    47,    58,     0,    56,    59,    43,     0,    42,
      46,     0,     0,    39,   629,   145,   111,   113,   114,   115,
     117,   118,   120,   121,   125,   126,   123,   124,   128,   129,
     131,   133,   135,   137,   139,     0,     0,   400,     0,     0,
       0,   859,   860,   856,   857,     0,    27,   454,   452,   670,
     669,   571,   928,   400,   933,   401,   400,   919,   400,     0,
       0,     0,   897,     0,   731,   967,   883,   884,   613,   880,
     881,   895,   923,   927,   925,   483,   518,     0,   931,   936,
     574,   965,     0,   151,     0,   573,     0,   964,   677,   675,
       0,     0,   738,    55,   701,     3,   351,     0,     2,   779,
     781,   783,     2,   799,   801,   400,   400,   875,   874,     2,
       0,     0,     0,     0,     0,   731,   883,   831,   848,     2,
     826,   834,   610,   829,   830,   611,     2,   841,   851,   844,
     845,     0,     3,   966,   370,     2,   959,     2,   601,   602,
     580,     3,     3,     3,     3,   624,     2,   638,     0,   635,
     965,     0,   631,     0,     2,   634,   637,   714,     0,   400,
       3,   374,   376,     0,   731,   763,   767,   731,   820,   825,
       2,   755,   758,   760,     2,   812,   815,   817,   729,     0,
     871,     3,   721,     3,   467,   466,   469,   468,     2,   696,
     722,     2,   720,     0,   696,   723,   533,   533,   533,   400,
       0,     0,   615,     0,   345,     0,     0,   400,     0,     2,
       0,     0,     0,     0,     0,   172,     0,   275,   276,     0,
       0,   314,   313,     0,   147,   147,   320,   494,   500,   186,
       0,   173,     0,   196,   174,   175,   400,   190,   176,   177,
     178,   179,     0,   180,   181,   281,     0,   182,   183,   184,
       0,   185,   192,   477,   400,     0,   194,     0,   339,     0,
       0,     0,     3,     0,   696,   684,   685,     0,     3,   680,
       3,     3,     0,   400,   661,   661,    83,   400,     0,    87,
      85,   400,     0,    99,     0,     0,     0,   103,   107,   106,
     171,     0,     0,     0,   629,    96,   164,     0,     0,    72,
       0,    72,    72,     0,    60,    62,    38,     0,     0,    36,
       0,    37,   964,   142,     0,     3,   731,   866,   869,   861,
       0,   930,   935,     2,    88,   400,     3,   492,     3,   401,
       3,   731,   890,   893,   400,     3,   879,   885,     0,   731,
     731,     0,   577,   562,   578,   964,     0,     2,   735,   737,
       0,     0,     0,   400,   400,     3,     3,     0,   731,   837,
     840,   731,     0,   731,   731,   832,   849,   400,   400,   960,
       0,   603,   400,   400,     0,     0,     0,     0,   359,     0,
     143,     0,     3,     3,   632,     0,   628,     0,   169,   168,
       3,     0,     0,     2,   756,   759,   761,     2,   813,   816,
     818,   400,   400,   624,   731,     0,     0,     0,   696,   724,
       0,   400,   400,   400,   400,   400,   400,   516,   544,     3,
       3,   545,   477,   534,     0,     0,   771,     2,     0,   343,
      55,     0,     0,   266,   267,   193,   195,     0,     0,     0,
       2,     2,   262,     0,   260,     0,     0,     0,   629,     0,
       0,     0,     0,     0,   148,     0,     0,   321,     0,     0,
       3,   199,     0,   191,     0,   257,     0,     0,     2,     0,
     477,   731,     0,   340,   877,   876,     0,     2,     0,   687,
       2,   682,     0,   683,     0,   665,   646,   650,   648,     0,
       0,     0,    30,     0,   100,   102,   101,    98,    97,   629,
     964,     0,    54,    69,     0,    63,    70,    71,    49,     0,
       0,     0,    57,    45,     0,     0,   141,     0,     2,   862,
     864,   865,    28,   400,     0,     0,     0,     3,     0,     2,
     886,   888,   889,     0,     0,    88,     0,     3,   964,   567,
       0,   577,   575,     0,   565,   678,   400,   740,   350,     3,
       3,     0,     0,   731,     2,   833,   835,   836,     2,   850,
     852,     0,   827,   842,     3,     3,   961,     3,   588,   587,
     591,   963,     2,     2,   962,     3,     0,     0,     0,     0,
     633,     3,     0,   731,   381,   400,   400,     3,     3,   387,
     730,     0,   821,   705,     0,   707,   516,   516,   516,   551,
     521,     0,   557,   545,     0,   400,   508,   543,   539,     0,
       0,     0,     0,   546,   548,   731,   559,   559,     0,   540,
     555,   400,   346,     0,   270,   271,   268,   269,     0,     0,
       2,   400,     2,   400,   263,   261,     0,   255,   964,   264,
       0,     0,     0,   302,   303,   304,   305,     0,   295,     0,
     296,   272,     0,   273,     0,     0,   400,   200,   189,   259,
     258,     0,   293,   312,     2,   344,   731,   400,   703,   666,
     400,     2,     2,    86,     0,    29,   400,     0,   964,     0,
       0,    73,     0,    61,     0,    67,     0,    65,    35,   146,
     867,   400,   938,   939,   940,     0,   891,   400,     3,     3,
       0,   899,     0,     0,     0,     0,   576,   564,     3,     0,
       0,   772,   790,   400,   400,     0,     0,     0,   400,   400,
       0,     3,   728,   639,   640,     0,   367,   369,     3,     3,
       0,     0,     0,   709,   512,   514,   510,     0,   906,     0,
     552,   911,   554,   903,   731,   731,   538,   558,   542,     0,
     541,     0,     0,     0,   561,     0,   731,   535,   549,   560,
     550,   556,   595,   599,   598,     2,     0,     0,   226,   207,
       0,     0,   209,   354,   208,   477,   400,   230,     0,   172,
     236,     0,   231,   172,   256,     0,     0,     2,   279,   306,
       0,   297,     2,     0,     0,     0,     0,   284,     0,   280,
     187,   368,   681,     0,     0,    33,    31,    32,     0,    52,
     165,    64,     0,     0,     3,   941,     3,     0,     0,   898,
     900,   566,     0,   964,     2,   773,   791,     3,     3,   838,
     853,   371,     2,   585,     3,   584,   642,     0,     0,     0,
     764,   822,   872,     0,     0,     0,   907,   908,   731,   537,
     904,   905,   536,   517,     0,     0,   278,     0,     0,     2,
     218,     2,   201,     0,     0,     2,   210,   477,   237,     0,
     252,   253,   254,   251,   240,     0,     2,   400,     0,     0,
       2,   203,   277,     2,   400,   274,     0,     0,   322,     2,
     282,     0,    55,     0,   294,   686,   688,    53,    68,    66,
       0,     0,   901,   902,   964,     0,   679,     0,     0,   400,
       0,   641,   765,   823,   731,   914,   916,   909,     0,   547,
     211,   214,     0,   213,   217,   400,   220,   219,   228,     0,
       3,   172,   245,     0,   241,     0,   238,     3,   232,   172,
     265,   400,   400,     3,   307,   401,   311,     0,   315,     0,
       0,     0,   323,   324,   205,   285,     0,     2,     0,     2,
       2,   868,   892,     0,   569,   839,   854,   589,     2,   910,
     912,   913,   553,     0,     0,   216,   221,   400,   335,   227,
     225,   233,   242,   253,   251,     0,   172,     0,   229,   235,
     221,     3,   300,     0,   906,   308,   309,   310,   322,     0,
       0,     0,   322,     0,     2,   283,   290,     0,   287,   289,
     568,   400,   212,   215,     2,     3,   222,   336,   247,   246,
     243,   234,   239,     3,   300,     0,     0,   907,     0,     0,
       0,   316,     0,   325,   206,     0,   280,     0,     3,   197,
     223,     0,     2,     0,     0,     0,     0,     0,   301,     0,
     328,     0,   326,     0,   328,   286,   288,     2,     0,     0,
     198,   202,   224,   249,   250,   248,   244,   204,     0,   298,
     329,     0,     0,   317,     0,   291,     2,   915,   299,     0,
       0,     0,     0,   292,   330,   331,     0,   327,   318,     0,
       0,   319,   332
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1638,  6263,  6343, -1638,    -1,   816,  -106, -1638,  1599, -1638,
     366, -1638,  -603,   743,  -815,  -900, -1638,   256,  7103,  1829,
   -1638,  1556, -1638,  1346,   126,   825,   828,   220,   819,  1292,
    1295,  1291,  1294,  1296, -1638,  -167,  -160,  8433,   838, -1638,
    -317, -1638, -1638,  -568,  1481, -1019,   699, -1638,   161, -1638,
     824,    22, -1638, -1638, -1638,   411,    94, -1638, -1637, -1414,
     285,    82, -1638, -1638, -1638,   197,   142, -1638, -1638, -1638,
   -1638,    40, -1565,   185, -1638, -1638,    44, -1638, -1638, -1638,
      61,   438,   439,   146, -1638, -1638, -1638, -1638,  -779, -1638,
      88,    50, -1638,   152, -1638,   -31, -1638, -1638, -1638,   840,
    -590,  -841, -1325, -1638,    14,    56,   104,  2677,  -821,  -763,
   -1638,  -249, -1638,    58,  -137,    39,  -352,  -228,  3728,  2241,
    -528, -1638,    69,    74,   178,   177, -1638,  1935, -1638,   124,
    4403, -1638, -1638, -1638,    43, -1638, -1638,  1713,   132,  4842,
    3099,   -50,  1737,  -364, -1638, -1638, -1638, -1638, -1638,  -628,
    4472,  5389, -1638,  -297,   110, -1638,   500,   250, -1638,   193,
     696, -1638,   496,   -30, -1638, -1638, -1638,  5825,  -660, -1079,
    -410,  -274,  -508,  1379, -1638, -1170,  -148,  -100,  1308,   865,
    6068,   -67,  -436,  -232,  -178,  -859,   964, -1638,  1228,   -87,
    1148,  1432, -1638, -1638, -1638, -1638,   323,  -165,  -188,  -149,
   -1638,   277, -1638, -1638,   574,   465, -1638, -1638, -1638,  2001,
    -682,  -437,  -935,   112, -1638, -1638, -1638, -1638, -1638,   136,
    -768,  -145, -1559,  -170,  7334,   -53,  7212, -1638,  1046, -1638,
     785,  -182,  -183,  -164,  -136,     1,   -59,   -57,   -54,   781,
     -33,   -29,   -25,  -127,   -27,  -125,   -95,   -84,  -641,  -711,
    -655,  -593,  -610,  -113,  -583, -1638, -1638,  -596,  1373,  1374,
    1376,   555,  8070,  -572,  -612,  -602,  -567,  -677, -1638, -1484,
   -1568, -1545, -1521,  -658,  -138,  -350, -1638, -1638,    23,     8,
     -96, -1638,  8756,   367,  -712,  -456
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1110,   208,   334,   335,   166,   336,   337,   338,  1371,
    1372,   339,   884,   885,  1183,  1184,  1185,  1383,   340,   406,
     342,   343,   587,   588,   344,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   354,   355,  1261,   589,  1335,   623,
     202,   625,   357,   773,  1111,  1112,  1113,  1114,  1115,  1116,
    1117,  1920,  1118,  1119,  1340,  1650,  1801,  1802,  1740,  1741,
    1742,  1895,  1896,  1120,  1661,  1662,  1755,  1121,  1122,  1123,
    1124,  1125,  1126,  1348,  1678,  1840,  1774,  1127,  1128,  1528,
    1906,  1529,  1530,  1823,  1129,  1130,  1131,  1338,  1831,  1832,
    1833,  1951,  1966,  1856,  1857,   279,   280,   834,   835,  1083,
      79,    80,    81,    82,    83,  1653,   434,    86,    87,    88,
      89,    90,   216,   541,   436,   358,   437,    93,   289,    95,
      96,    97,   370,   371,   100,   101,   162,   102,   928,   372,
     148,   105,   236,   106,   149,   245,   374,   375,   376,   150,
     409,   111,   112,   378,   113,   532,   823,   821,   822,  1486,
     379,   380,   116,   117,  1079,  1303,  1492,  1493,  1619,  1620,
    1304,  1481,  1638,  1494,   118,   701,  1584,   381,   699,   965,
    1021,   442,   443,   827,   828,   444,   445,   829,   383,   536,
    1135,   360,   361,   203,   766,   767,   768,   769,   770,   307,
    1154,   308,   850,   848,   565,   309,   399,   310,   311,   362,
     120,   168,   169,   121,  1148,  1149,  1150,  1151,     2,  1068,
    1069,   811,  1287,   122,   299,   247,   257,   515,   123,   206,
     124,   217,  1263,   814,   482,   160,   125,   712,   713,   714,
     126,   219,   220,   221,   222,   294,   128,   129,   130,   193,
     132,   133,   134,   225,   295,   227,   228,   229,   743,   744,
     745,   746,   747,   230,   749,   750,   751,   650,   651,   652,
     653,   483,   135,   676,   677,   678,   679,   680,   681,  1622,
    1623,  1624,  1625,   666,   447,   386,   387,   388,   363,   195,
     137,   138,   139,   390,   962,   682
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      76,   292,   127,    76,   400,   684,   892,   407,   499,   136,
     382,   179,   136,   180,   408,   661,   181,   177,   144,   512,
     385,   368,   186,   624,   458,   953,   411,   875,   991,   540,
     592,   468,   692,   476,   694,  1192,   695,   182,   697,   284,
      92,   183,   967,   146,   107,   184,   396,   480,   843,  1723,
     469,    76,    76,   922,    76,  1031,   127,    84,   939,    91,
     868,   807,   809,   136,   226,  1001,  1136,   403,   940,    76,
      98,  1008,  1724,  1188,   684,    99,  1329,    76,   470,   192,
     661,   818,  1448,  1449,   992,    76,   496,   471,   446,   472,
      76,   429,   223,    76,    92,   248,  1725,    76,   107,   258,
     997,   194,   947,   941,  1807,    85,   136,   468,   145,   476,
    1200,    84,   251,    91,  1773,   547,   549,   190,   413,   473,
     414,   844,   569,   415,    98,   103,   469,   939,  1534,    99,
     474,   998,   204,   108,  1727,    76,  1074,   940,    76,  1290,
      76,   255,   127,  1422,   416,    76,   993,    54,   417,   136,
      54,    76,   418,   479,   470,   179,   994,   180,    76,    85,
     181,   466,  1144,   471,  -689,   472,   194,   172,   243,   947,
     321,  -337,   941,  1456,   282,    76,    76,   192,  1803,   103,
      92,   182,   190,  1532,   107,   183,  1652,   108,   477,   184,
      76,  1390,   450,   488,  1535,   473,   140,    84,   241,    91,
    1853,  -713,   252,  1308,    76,   548,   474,  1457,  1829,  1358,
      98,  1652,   569,    76,    76,    99,   192,   870,   510,   527,
     948,   930,  1309,  1391,   646,   158,   427,  1867,   520,   667,
      76,   179,   481,   180,  1457,   200,   181,   552,  1298,    76,
    -337,   192,  1797,  -338,  1132,    85,  1311,  1312,   798,    76,
    1023,  1023,    76,   287,   516,  1226,   151,  1807,  1299,    76,
     593,  1533,   201,   505,   477,   103,   647,  1023,   969,    76,
      76,   244,    76,   108,  1178,   548,   501,   261,   991,   504,
    1385,   262,   264,   780,   265,  1807,   267,  1002,   275,    76,
      76,  1005,   192,   794,   667,  1803,   838,    76,   684,   673,
    1018,  1019,   781,    76,    76,  1723,  1225,   932,    76,   934,
     890,   703,  -338,  1001,   705,  1380,  1300,    92,   200,  1316,
     505,   107,   684,   748,   244,  1199,    54,   514,  1724,   684,
     782,   952,   735,  1023,   992,   832,   504,   274,  1169,   783,
    1876,   784,  1586,   274,   958,   963,   959,   543,  1136,  1024,
     805,  1773,  1725,  1474,   843,  1245,   810,   513,   274,   487,
    1313,  1756,   492,   870,   194,  1040,  1757,   461,   244,   780,
    1230,   785,   794,  1925,  1217,  1504,    54,   939,  1071,   152,
     190,    54,   786,   964,   509,   562,  1246,   940,   781,  1288,
    1727,   273,    76,   419,   519,    54,   993,    76,  1737,  1738,
      76,   711,   103,   446,   273,    54,  1237,   413,   481,   414,
     108,    76,   415,   450,   563,   564,   782,  1498,   385,   368,
    -690,   261,   941,  1797,  1217,   783,   200,   784,  1448,  1449,
     244,  1142,   269,   416,    76,    76,  1499,   417,   157,    54,
     544,   418,  1894,   243,  1455,   197,    76,    76,   795,  1937,
    1298,  1298,  1298,   963,   489,   764,  1894,   785,   481,   555,
     244,    54,   684,   481,  1100,    76,   244,   734,   786,  1518,
    1299,  1299,  1299,   726,    76,   274,   450,   481,    19,  1739,
    1395,    54,  1922,   978,  1022,  1022,   817,   481,   591,  1563,
    1565,  1567,  1506,   413,    76,   414,   446,  1315,   415,   175,
      76,  1022,   419,   851,   481,   853,   854,  -847,   855,    54,
     197,   857,   243,  1423,   859,   860,   861,   999,    54,   153,
    1558,   671,   154,   155,   364,   156,  1132,   795,  1300,  1300,
    1300,  1023,  1033,  1034,  1411,    54,  1035,   521,    76,  1006,
      76,   204,   364,   671,    54,   667,   244,   953,   533,   261,
     262,    76,   688,    76,   267,   450,    54,  1251,  1418,  1050,
     446,    54,   175,   481,  1440,   187,    76,  1022,   171,  1562,
    -513,   684,    13,    14,    15,    16,    17,   173,   146,  1737,
    1738,  1201,  1308,  1202,    92,   407,   255,  1054,   107,    54,
    1032,   481,  1012,   273,    54,   419,  1273,   481,    76,    76,
     481,  1545,    76,   174,  1627,   527,    76,   506,   205,    76,
    -713,  1310,   977,  1277,   543,   244,   243,   481,  1498,  1847,
    1767,   175,  1398,  1628,   802,  1768,   481,    54,   748,  1270,
      59,    60,  1042,   198,  1409,    -3,   446,  1630,   671,  1434,
    1749,   211,  1636,   481,  1758,  1881,   813,   231,   801,   843,
    1882,  1058,   816,   804,    76,   870,   820,    76,   446,   446,
    1760,  1637,  1728,  -394,   506,   636,   637,  1438,  1559,   103,
     812,   671,  1848,    69,  1664,   446,   481,   108,    72,   244,
     819,  1729,    13,    14,    15,    16,    17,  -398,   269,    76,
      54,  1059,   261,   648,  1636,  1191,    69,   481,  1152,   244,
     271,    76,  1187,   863,    74,    75,  1583,  1503,   197,   638,
     639,   175,   186,  1732,   864,   865,   670,  1366,  -618,   244,
     671,   273,    69,   274,   385,   368,  1173,    74,    75,   936,
     698,   591,  -337,  1174,   669,  1815,   591,    54,    76,   591,
      76,   446,  1617,  1382,  1447,   398,   481,   288,  1193,  1692,
    1187,  1693,    76,    74,    75,   897,   898,   899,   591,    76,
     514,    76,  1837,  1521,   401,  1022,   238,     6,     7,     8,
       9,    10,    11,    12,  1933,    76,    76,    76,  1347,  1934,
     491,   629,   131,   305,   232,   233,  1224,   234,   630,   631,
     402,   235,  1861,  1258,  1838,    76,   321,  -399,   153,   669,
    1869,   154,   155,   420,   156,   936,  1665,    13,    14,    15,
      16,    17,   489,  1560,   790,   421,   481,    77,   422,    58,
     142,   164,   165,    61,    62,    63,    64,    65,    66,    67,
     566,    76,    76,   764,   567,   423,   131,   656,   424,   244,
     425,   657,   136,   449,  1220,   684,  1688,  1901,  1614,  1615,
    1616,   453,  1264,   952,   136,   178,   904,   905,   906,   907,
    1145,  1260,    54,   244,   364,   364,   454,    71,   185,    60,
     244,    77,   224,    92,   717,   249,   218,   107,   718,   259,
      76,   455,  1244,   748,   459,  1147,    77,    76,  1283,   462,
      84,   463,    91,   764,    77,   843,   497,   555,   464,   419,
     243,   481,   999,  1134,   419,   964,   671,    77,    99,  -385,
      77,   514,   465,   654,    77,    13,    14,    15,    16,    17,
      69,  1651,   131,  1663,    76,  1703,  1523,  1524,  1525,  1526,
    1527,   293,  -385,  1750,  1751,  1752,   479,   719,    85,   446,
     648,   657,   526,    60,   481,   870,  1651,  1750,  1863,  1752,
    1146,    74,    75,  1073,   478,  1753,    76,    77,   103,   870,
      76,   204,    77,  1062,  1754,    76,   108,   187,   595,  1864,
      54,   711,   451,   508,  1070,   394,   728,  1072,  -173,   731,
     730,  1075,   498,   831,   481,   518,  1413,   832,    76,   400,
     400,  1785,    77,    77,    76,    76,   537,   869,  1480,   467,
     218,   870,  1359,   244,   968,   599,   970,    77,   567,   243,
     567,   971,   572,    69,   244,   972,   293,   982,  1037,   385,
     368,   481,  1038,   558,   364,   573,   491,  1396,    69,   764,
      77,    77,    76,   670,   517,   273,   489,   671,   577,   481,
     481,   600,  1784,   601,    74,   672,   628,    77,   670,   642,
     954,  1469,   671,  -395,  1064,   643,    77,   673,   870,    74,
     672,   644,    13,    14,    15,    16,    17,   667,   645,    77,
     655,   364,  1843,  1066,    58,   553,   293,   870,    61,    62,
      63,    64,    65,    66,    67,   880,   530,  1460,   764,   535,
    1714,    76,    76,    76,  1373,   136,   659,   555,   691,  1908,
    1260,   481,  1305,  1912,   964,  1450,    77,    77,  -846,  1186,
    1365,  -563,   244,  1187,   657,   764,  1427,    54,  1301,  1496,
    1187,    76,    71,   702,   136,   881,    92,  1555,  -396,    76,
     107,  1556,    76,    76,   248,   258,    76,    13,    14,    15,
      16,    17,   136,    84,   704,    91,    76,  1645,   251,   715,
    1646,  1187,   446,   446,   870,    92,  1134,    76,  1666,   107,
    1667,    99,   870,   595,  1038,   720,  1668,  1663,   721,  1836,
     870,  1733,    84,   764,    91,   657,   722,   255,   723,    76,
      13,    14,    15,    16,    17,  1134,  1291,  1292,  1293,   724,
      99,    85,    54,   451,   238,     6,     7,     8,     9,    10,
      11,    12,  1809,   654,   654,  1885,   870,   243,  1935,  1187,
     725,   103,   870,   426,   364,  1858,    -3,    76,   752,   108,
      85,   742,  -109,  -109,  -109,  -109,  -109,  -109,    77,  1962,
    -397,  1858,  1969,  1959,  1289,    54,  1970,   241,   252,   774,
     103,    76,   632,   633,   787,   634,   635,  1314,   108,   788,
     260,   779,    77,   640,   641,   789,   451,   791,    76,   792,
     218,   799,  1654,   793,  1333,   385,   368,  1897,   659,   595,
     872,   873,   765,   797,  1497,   815,   980,  1014,  1015,   983,
     293,   281,    77,   514,  1016,  1017,   293,  1654,  -511,   407,
     407,    77,  1176,  1038,  1189,  1190,  1601,  1602,   870,  1194,
      76,  1496,   136,  -509,    76,   824,   468,    76,   144,   476,
     244,    77,  1016,  1357,  1305,  1305,  1305,    77,  1482,  1305,
     988,  1381,  1388,  1389,   833,   469,   293,   764,  1393,  1389,
    1301,  1301,  1301,   146,  1479,  1483,  1631,   842,   491,   293,
     847,   602,  1052,   603,   604,   605,  1056,   764,   845,    76,
     871,   516,   874,   470,   877,   142,   927,    77,  1402,  1389,
    1771,  1772,   471,   942,   472,  -144,  -144,   894,    77,   944,
      77,   673,   606,   988,  1471,   607,   608,   961,   764,   995,
     609,   610,   966,    76,  1568,   870,   244,   973,    76,    76,
      76,  1603,  1381,   987,   473,  1690,  1038,   988,   145,  1450,
     974,  1643,  1691,  1389,  1030,   474,    18,  1711,  1712,   385,
     368,  1044,  1824,  1722,   870,   878,   879,  1778,  1389,   882,
    1779,  1389,  1045,   889,   514,   976,   893,  1854,  1855,  1675,
    1737,  1738,  1959,  1960,   136,  1046,  1632,  1386,  1387,   -16,
    1145,    44,    45,    46,    47,    48,    49,    50,    51,   780,
     -17,  1450,  1029,   794,   513,  -693,  1497,   900,   901,   908,
     909,   684,   902,   903,   477,  1147,  1639,  1639,   781,  1047,
    1824,   926,  1505,  1507,   931,  1048,   654,  1367,  1368,   244,
    1049,  1065,    76,    13,    14,    15,    16,    17,    76,  1373,
      76,  1067,  1076,  1077,  1078,  -594,   782,    76,   446,   446,
    1137,  1138,  1156,  1159,  1153,   783,   956,   784,  1158,   764,
    1543,   764,  -108,  -108,  -108,  -108,  -108,  -108,  1160,   136,
     364,   136,  1162,  1161,   990,  1164,   742,   244,  1165,  1166,
    1146,    13,    14,    15,    16,    17,   867,   785,    54,  1776,
    1171,  1172,  1196,   764,   136,  1197,  1198,  1211,   786,  1212,
      92,  1213,    92,  1496,   107,    77,   107,    77,   136,  1223,
    1227,  1228,   293,    76,  1145,    76,  1232,  1238,  1766,  1239,
     407,  1240,   954,  1241,  -582,    92,  -581,  1800,  1027,   107,
    1656,   293,  1656,  1249,    76,    99,  1266,    99,  1284,  1147,
    1272,  -694,  1041,    77,  1043,  1306,    69,  1307,  1317,  1275,
    1320,  1321,  1279,  1330,  1331,  1656,  1332,  1337,   795,  -617,
      99,  1339,    77,   870,  1341,    85,  1617,    85,  1347,  1351,
     481,    13,    14,    15,    16,    17,  1168,    74,    75,  1354,
    1353,    76,  1355,  1361,    76,   103,  1363,   103,  1379,  1424,
      85,  1450,  1425,   108,   764,   108,  1428,  1381,  1082,  1433,
    1451,  1446,  1473,  1454,  1146,  1452,  1453,  1463,  1472,  1475,
     103,  1485,  1830,  1487,   136,  1310,   764,    58,   108,  1488,
     251,    61,    62,    63,    64,    65,    66,    67,  1509,  1875,
    1510,  1512,    76,    76,  1514,  1536,   407,   407,  1669,   446,
     468,  1515,   476,  1892,  1800,  1517,  1519,  1182,  1531,   255,
    1539,    76,  1538,  1548,  1182,  1540,  1541,  1546,  1497,   469,
     765,  1549,  1561,   407,  1551,    71,  1552,  1553,  1026,  1569,
    1910,  1570,  1626,  1554,  1557,   794,  1572,  1573,  1574,   243,
    1576,  1581,  1585,  1587,    76,  1591,  1389,   470,  1928,  1592,
     764,  1182,   419,  1604,   764,  1613,   471,  1606,   472,  1490,
    1647,  1400,  1672,  1674,  1679,   764,  1689,  1702,  1100,   241,
     252,  1695,  1701,  1699,  1700,   136,  1705,  1706,   764,  1709,
    1710,  1716,  1830,  1219,   990,  1603,  1830,  1830,   473,  1720,
    1243,   742,  1721,  1745,   205,   407,  1759,  1826,  1319,   474,
    1763,  1769,  1961,  1436,  1765,  1770,    92,  1777,  1786,  1782,
     107,  1808,  1783,  1931,  -583,    77,  1791,  1792,   590,  1793,
    1794,    77,    77,   136,   179,  1795,   180,  1796,   481,   181,
     552,   514,  1844,    76,  1950,    76,  1656,  1804,  1950,   136,
      78,    99,   244,   143,  1811,  1819,   764,  1820,   764,   764,
    1827,  1828,   830,  1841,    92,  1826,   765,   477,   107,  1027,
    1842,   513,   364,  1964,  1845,  1846,  1712,  1859,  -494,  1866,
      92,    85,  1879,  1878,   107,   136,  1880,  1883,  1884,  1887,
    1890,   574,    76,    76,  1656,   192,  1898,  1899,  1909,    99,
     795,   103,  1905,   764,    78,  1911,  1916,  1917,  1923,   108,
    1656,  1929,  1930,   764,  1924,    99,    92,   626,   627,   176,
     107,   440,  1932,  1941,  1947,  1948,    76,    78,   450,    85,
    1952,  1953,  1956,  1957,   249,   259,  1967,  1968,  1971,   764,
     215,   764,  1686,   240,   244,    85,  1656,    78,   598,   103,
    1392,    99,   293,   866,   910,   912,   764,   108,   911,   913,
    1343,   764,   914,  1336,  1942,   103,   535,  1676,  1893,    77,
      77,  1761,  1903,   108,  1818,   764,  1865,  1938,  1839,    76,
    1936,    85,   626,    77,   143,  1927,  1670,  1671,  1871,    76,
      78,  1913,   143,  1870,  1369,   291,   297,  1352,   806,   808,
     163,   103,   507,  1629,  1954,  1799,   341,   367,   626,   108,
     765,  1852,  1484,  1640,  1265,  1349,  1182,  1036,  1155,   849,
    1588,     3,   244,     0,   341,   176,   176,   238,     6,     7,
       8,     9,    10,    11,    12,  1682,   143,   432,  1229,     0,
     240,   918,   919,     0,   920,     0,    54,  1441,     0,     0,
       0,     0,     0,     0,  1621,     0,     0,   231,     0,     0,
       0,     0,     0,   215,   215,     0,     0,  1542,     0,     0,
       0,   590,     0,     0,     0,     0,   590,    58,    77,   590,
     291,    61,    62,    63,    64,    65,    66,    67,     0,    78,
       0,     0,     0,     0,     0,  1182,     0,     0,   590,     0,
     664,     0,   240,   687,    69,     0,  1495,     0,     0,     0,
    1949,     0,     0,     0,     0,     0,     0,   664,     0,     0,
       0,   664,     0,     0,    70,    71,     0,     0,     0,  1958,
       0,   830,   297,     0,     0,    74,    75,    77,   297,   291,
     291,     0,     0,     0,     0,     0,   143,     0,   916,     0,
       0,   517,    58,     0,   212,   213,    61,    62,    63,    64,
      65,    66,    67,     0,   765,   664,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   341,     0,     0,    69,
       0,     0,     0,     0,     0,     0,    77,     0,     0,     0,
     341,     0,  1621,  1621,     0,     0,     0,     0,     0,  1489,
      71,     0,  1063,     0,   896,     0,  1490,   649,     0,     0,
      74,    75,     0,     0,     0,   765,   367,   674,   683,     0,
    1182,     0,     0,     0,  1344,  1182,  1182,  1182,     0,  1648,
       0,  1657,     0,   367,    58,     0,   440,   367,    61,    62,
      63,    64,    65,    66,    67,   830,     0,     0,     0,    58,
       0,   164,   165,    61,    62,    63,    64,    65,    66,    67,
       0,   176,     0,  1680,    58,     0,   830,     0,    61,    62,
      63,    64,    65,    66,    67,     0,     0,   143,     0,     0,
       0,   432,    71,  1221,     0,   741,     0,   683,  1495,   440,
       0,     0,     0,     0,  1633,     0,  1495,    71,     0,     0,
      58,  1621,   164,   165,    61,    62,    63,    64,    65,    66,
      67,  1242,    71,   830,   830,   215,  1195,     0,     0,    77,
       0,  1345,     0,     0,   215,    77,     0,    77,     0,   440,
       0,     0,     0,     0,     0,     0,     0,  1210,     0,     0,
       0,     0,     0,     0,   291,     0,   341,   341,    71,     0,
     291,     0,   367,     0,     0,   242,     0,     0,  1326,     0,
       0,     0,     0,     0,  1736,     0,   263,     0,   266,  1850,
     268,     0,    58,  1621,   164,   165,    61,    62,    63,    64,
      65,    66,    67,     0,  1235,  1236,  1762,     0,     0,     0,
     291,     0,   664,   440,     0,     0,     0,     0,     0,  1621,
    1182,   291,  1182,   291,     0,   367,     0,     0,   242,     0,
     266,   268,     0,     0,     0,     0,   664,     0,     0,   341,
      71,   143,   143,     0,   341,     0,     0,   341,     0,   664,
     143,   143,   143,     0,  1163,     0,     0,  1734,     0,  1167,
    1495,     0,     0,     0,     0,   432,     0,     0,  1621,  1621,
    1175,     0,   242,     0,     0,     0,     0,     0,     0,     0,
    1806,     0,     0,     0,  1810,     0,     0,     0,    77,   440,
       0,    77,     0,     0,     0,  1817,     0,     0,     0,     0,
       0,     0,  1621,     0,     0,     0,     0,     0,  1834,   293,
       0,     0,     0,     0,     0,     0,     0,   649,   649,     0,
       0,     0,     0,     0,     0,     0,   341,     0,   367,   432,
       0,   683,     0,     0,   242,     0,   266,   268,     0,   674,
      77,     0,     0,   674,     0,     0,   706,     0,     0,     0,
       0,     0,   367,     0,     0,     0,     0,     0,     0,     0,
    1495,     0,   683,     0,   242,   367,     0,     0,     0,     0,
     242,     0,     0,   143,     0,     0,  1886,     0,  1888,  1889,
       0,   830,   830,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,     0,   830,   830,     0,     0,     0,
       0,     0,     0,     0,     0,   432,     0,     0,   741,     0,
     741,     0,     0,     0,     0,   440,     0,   707,     0,     0,
       0,     0,     0,  1914,     0,     0,     0,   367,   367,   830,
     830,     0,   708,  1919,   709,   710,    61,    62,    63,    64,
      65,    66,    67,     0,   367,     0,   291,     0,     0,   293,
     242,     0,  1429,  1430,     0,     0,   689,     0,   268,  1940,
       0,  1919,     0,     0,     0,   291,  1444,  1445,     0,     0,
       0,     0,     0,     0,     0,     0,  1955,     0,     0,     0,
      77,  1940,  1877,   574,     0,     0,   664,     0,     0,   687,
       0,     0,     0,   242,     0,  1963,     0,     0,   553,   293,
    1467,  1468,     0,   341,     0,     0,     0,    54,     0,     0,
     367,  1394,     0,     0,     0,   143,   341,     0,     0,   242,
       0,   689,   268,     0,     0,     0,     0,     0,     0,    77,
      77,   432,   293,     0,     0,     0,     0,     0,    58,     0,
     212,   213,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,   242,     0,     0,     0,
    1374,  1375,  1376,    77,     0,    69,     0,  1377,  1378,     0,
       0,     0,     0,     0,     0,     0,   242,     0,     0,     0,
       0,   242,     0,   242,     0,  1873,    71,     0,     0,   481,
     649,    18,     0,     0,     0,   189,    74,    75,     0,     0,
       0,   626,   367,   242,  1205,   242,   242,     0,     0,     0,
       0,     0,     0,   830,   830,   674,  1965,     0,     0,     0,
       0,     0,     0,   242,     0,     0,  1972,     0,     0,     0,
      48,    49,    50,    51,     0,   242,     0,     0,  1508,     0,
       0,     0,    13,    14,    15,    16,    17,  1516,     0,  1644,
       0,  1520,     0,  1522,     0,     0,     0,     0,   741,     0,
     189,     0,     0,     0,     0,   741,     0,     0,     0,     0,
       0,     0,    58,     0,   189,   189,    61,    62,    63,    64,
      65,    66,    67,   887,  1608,  1609,     0,     0,     0,     0,
       0,     0,   189,     0,     0,     0,     0,    54,     0,     0,
       0,     0,     0,     0,     0,   435,     0,     0,   367,     0,
     440,     0,     0,     0,     0,     0,     0,     0,     0,   830,
      71,     0,     0,   888,     0,   830,     0,     0,    58,     0,
     212,   213,    61,    62,    63,    64,    65,    66,    67,     0,
       0,   830,   830,   242,     0,   689,   268,     0,   143,    13,
      14,    15,    16,    17,     0,    69,   341,   189,   664,     0,
       0,     0,     0,     0,     0,     0,     0,   242,   689,     0,
       0,     0,     0,     0,   242,   739,    71,     0,     0,   671,
       0,     0,     0,     0,     0,   341,    74,   740,     0,     0,
    1694,     0,  1612,     0,     0,     0,  1696,     0,     0,     0,
       0,    58,   240,    78,    54,    61,    62,    63,    64,    65,
      66,    67,  1707,  1708,   189,     0,   291,     0,     0,     0,
       0,     0,   143,     0,     0,     0,   432,     0,    69,     0,
     143,     0,  1649,     0,  1660,    58,     0,   212,   213,    61,
      62,    63,    64,    65,    66,    67,     0,     0,   989,    71,
       0,     0,   671,     0,     0,     0,     0,  1649,   189,    74,
      75,     0,    69,     0,     0,     0,     0,     0,   300,   301,
     302,   303,     0,     0,   432,     0,     0,     0,     0,     0,
       0,     0,  1873,    71,   189,     0,   481,     0,     0,     0,
       0,     0,     0,    74,    75,    54,    13,    14,    15,    16,
      17,     0,     0,     0,     0,     0,    58,   242,   164,   165,
      61,    62,    63,    64,    65,    66,    67,     0,   242,     0,
       0,   367,   367,     0,     0,     0,    58,     0,   212,   213,
      61,    62,    63,    64,    65,    66,    67,  1677,     0,     0,
     110,     0,     0,   110,     0,     0,     0,     0,     0,   189,
       0,    54,     0,     0,    71,     0,   440,   304,     0,     0,
     143,   143,   143,   143,   143,   143,     0,     0,  1744,     0,
    1491,   297,  1746,  1242,    71,   305,     0,     0,     0,  1748,
       0,     0,    58,     0,   212,   213,    61,    62,    63,    64,
      65,    66,    67,     0,   110,     0,     0,     0,     0,     0,
       0,     0,   242,     0,     0,     0,     0,     0,     0,    69,
       0,     0,     0,     0,   189,   189,   242,   110,     0,   240,
     435,     0,     0,     0,     0,     0,     0,   242,     0,   214,
      71,     0,     0,   246,     0,     0,   242,   110,     0,   830,
      74,    75,     0,     0,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,    58,     0,   164,   165,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,   432,   189,   110,  1814,  1816,   242,  1660,     0,
     110,     0,   110,     0,     0,     0,   246,     0,     0,     0,
       0,     0,     0,     0,     0,   143,   359,   110,   395,     0,
       0,     0,     0,     0,    71,    54,     0,     0,     0,     0,
    1918,     0,     0,   435,   359,   611,   612,   613,   614,   615,
     616,   617,   618,   619,   620,   621,   110,   359,  1322,   200,
     246,     0,     0,     0,  1862,     0,    58,     0,   212,   213,
      61,    62,    63,    64,    65,    66,    67,     0,  1618,     0,
       0,     0,  1491,     0,   341,     0,   622,     0,  1491,     0,
    1491,     0,     0,    69,     0,     0,     0,  1835,     0,    13,
      14,    15,    16,    17,   189,     0,   110,   435,     0,   110,
     341,     0,   341,   290,    71,     0,  1900,     0,  1902,     0,
       0,     0,   246,     0,    74,    75,     0,     0,     0,     0,
     189,     0,     0,     0,     0,   341,     0,     0,  1915,   531,
       0,     0,     0,     0,   242,     0,   367,   110,     0,   143,
       0,   189,   246,     0,    54,   143,     0,     0,   246,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,     0,
       0,     0,     0,     0,  1943,  1945,  1946,     0,     0,     0,
       0,     0,     0,   435,     0,    58,     0,   212,   213,    61,
      62,    63,    64,    65,    66,    67,   359,   367,   367,     0,
       0,     0,     0,     0,     0,   435,   435,     0,     0,     0,
     359,     0,    69,     0,     0,     0,  1618,  1618,     0,     0,
     242,     0,   435,     0,     0,     0,   242,     0,     0,     0,
       0,  1491,  1489,    71,  1491,     0,   110,    54,   246,   110,
       0,     0,     0,    74,    75,     0,     0,   664,     0,     0,
       0,     0,     0,   110,   297,   143,    58,   110,   164,   165,
      61,    62,    63,    64,    65,    66,    67,     0,    58,     0,
     212,   213,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,   291,     0,     0,     0,     0,   435,     0,
       0,     0,     0,     0,   189,    69,     0,   110,     0,     0,
       0,   359,     0,     0,    71,   664,     0,   246,     0,   189,
       0,     0,     0,   242,     0,   214,    71,     0,     0,     0,
       0,     0,     0,     0,     0,  1618,    74,    75,  1324,     0,
       0,     0,     0,     0,  1491,     0,    13,    14,    15,    16,
      17,     0,     0,     0,     0,    58,   297,   391,   392,    61,
      62,    63,    64,    65,    66,    67,   341,     0,     0,     0,
       0,   242,     0,   143,     0,     0,   359,   359,     0,    58,
       0,   246,   110,    61,    62,    63,    64,    65,    66,    67,
    1179,     0,     0,     0,  1180,     0,  1181,     0,   367,     0,
       0,    54,     0,    71,     0,    72,     0,  1618,     0,     0,
     393,     0,     0,   110,   143,     0,     0,     0,   110,     0,
       0,   246,   110,     0,   110,     0,     0,    71,     0,     0,
     143,   143,    58,  1874,   297,   110,    61,    62,    63,    64,
      65,    66,    67,     0,     0,     0,     0,     0,     0,   359,
       0,   110,   110,     0,   359,     0,     0,   359,     0,    69,
     110,   110,   110,     0,    54,     0,   143,     0,     0,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,    70,
      71,     0,  1874,  1874,     0,     0,     0,     0,     0,     0,
      74,    75,    54,     0,     0,    58,   435,   212,   213,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,    94,
       0,     0,   147,     0,     0,     0,  1874,     0,     0,     0,
       0,     0,    69,    58,     0,   212,   213,    61,    62,    63,
      64,    65,    66,    67,     0,     0,   359,   395,   110,   359,
       0,   246,   290,    71,     0,     0,     0,     0,     0,     0,
      69,     0,     0,    74,    75,     0,     0,     0,     0,     0,
       0,     0,   110,    94,     0,   246,     0,     0,     0,   531,
    1489,    71,   246,     0,     0,   110,     0,   960,     0,     0,
       0,    74,    75,   110,     0,     0,   191,     0,     0,     0,
       0,     0,     0,    58,     0,   212,   213,    61,    62,    63,
      64,    65,    66,    67,     0,     0,   253,     0,     0,     0,
     189,     0,     0,     0,   189,   359,     0,     0,     0,     0,
      69,    58,     0,   212,   213,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,   110,   110,     0,
     739,    71,     0,   283,   671,     0,     0,     0,    69,    94,
       0,    74,   740,     0,   110,     0,   242,     0,     0,     0,
       0,     0,   189,     0,   673,     0,   369,     0,  1489,    71,
       0,     0,     0,     0,     0,  1490,   242,     0,     0,    74,
      75,     0,   110,    58,   412,   164,   165,    61,    62,    63,
      64,    65,    66,    67,     0,   283,   438,     0,     0,     0,
       0,     0,     0,     0,     0,   246,     0,     0,     0,   435,
     435,     0,     0,   359,     0,     0,   246,     0,     0,     0,
     110,     0,     0,   475,     0,   110,   359,     0,     0,     0,
       0,    71,     0,     0,     0,     0,     0,     0,   560,   495,
       0,   359,     0,     0,   500,   502,     0,   242,   191,     0,
      58,     0,   212,   213,    61,    62,    63,    64,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,   242,     0,
     522,     0,     0,   524,     0,   525,     0,    69,     0,     0,
       0,     0,     0,     0,     0,    58,   542,   526,    60,    61,
      62,    63,    64,    65,    66,    67,   110,  1873,    71,   554,
       0,   481,     0,     0,     0,     0,     0,     0,    74,    75,
       0,     0,   110,     0,  1207,   359,     0,   110,     0,     0,
       0,    58,     0,   212,   213,    61,    62,    63,    64,    65,
      66,    67,     0,    71,     0,    58,   925,   212,   213,    61,
      62,    63,    64,    65,    66,    67,   242,    58,    69,   212,
     213,    61,    62,    63,    64,    65,    66,    67,     0,     0,
     189,     0,    69,     0,   110,   110,     0,     0,   214,    71,
       0,     0,     0,     0,     0,   662,     0,     0,   686,    74,
      75,     0,   290,    71,     0,     0,     0,     0,     0,     0,
       0,     0,   693,    74,    75,    71,   693,     0,     0,   317,
       0,   318,  1218,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,    58,     0,   110,     0,
      61,    62,    63,    64,    65,    66,    67,  1179,   242,     0,
       0,  1180,     0,  1181,     0,     0,   283,     0,     0,     0,
     662,     0,   189,     0,     0,     0,     0,     0,   597,     0,
       0,    72,   404,     0,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,    71,   312,   359,  1384,     0,   313,
       0,   314,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   315,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,   189,     0,     0,     0,     0,     0,
       0,   438,   246,   110,     0,   316,   317,     0,   318,     0,
    1812,    60,    61,    62,    63,    64,    65,    66,    67,   319,
     320,   321,   110,   322,   323,   324,   359,   325,   326,     0,
     110,     0,   826,     0,     0,    69,     0,   502,     0,     0,
       0,   837,     0,   542,     0,   435,   435,     0,     0,     0,
       0,     0,     0,     0,   369,   327,     0,     0,    72,   404,
       0,     0,     0,     0,     0,   329,    74,    75,   330,   331,
     332,   333,     0,     0,   359,     0,     0,     0,  1207,  1813,
    -172,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1421,     0,     0,     0,   438,     0,     0,     0,     0,     0,
       0,     0,   110,   110,    58,     0,   185,    60,    61,    62,
      63,    64,    65,    66,    67,     0,   110,   110,     0,    58,
       0,   110,   110,    61,    62,    63,    64,    65,    66,    67,
    1179,     0,     0,     0,  1180,    58,  1181,   528,   529,    61,
      62,    63,    64,    65,    66,    67,     0,   921,     0,     0,
     110,   110,    71,     0,     0,  1026,     0,   693,   935,     0,
     110,   110,   110,   110,   110,   110,     0,    71,     0,     0,
    1564,   246,   946,     0,   104,     0,     0,     0,     0,     0,
       0,   662,     0,    71,     0,    72,   955,     0,     0,     0,
       0,     0,     0,    58,   693,     0,     0,    61,    62,    63,
      64,    65,    66,    67,  1179,     0,     0,     0,  1180,     0,
    1181,     0,     0,     0,     0,     0,     0,     0,     0,   246,
       0,     0,     0,     0,     0,     0,     0,     0,   104,     0,
       0,     0,     0,     0,   935,     0,   435,     0,     0,   996,
       0,    71,     0,   114,  1566,    58,   114,   164,   165,    61,
      62,    63,    64,    65,    66,    67,   438,   438,    58,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
       0,   254,   359,   438,    58,     0,   164,   165,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,   449,    71,     0,   110,     0,   114,     0,     0,
       0,   826,     0,     0,     0,     0,    71,     0,    72,     0,
       0,     0,     0,     0,   104,     0,     0,     0,     0,     0,
     114,   453,    71,     0,     0,     0,     0,     0,     0,     0,
       0,   373,  1133,     0,   110,   110,     0,     0,     0,   438,
     114,     0,     0,    58,   147,   164,   165,    61,    62,    63,
      64,    65,    66,    67,   359,     0,     0,     0,     0,     0,
     369,   439,     0,     0,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,   114,     0,     0,
     359,     0,   359,   114,     0,   114,     0,     0,     0,     0,
       0,    71,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   826,   110,   114,     0,   110,
       0,     0,     0,     0,     0,   110,     0,     0,     0,   114,
       0,   693,     0,     0,  1209,   523,   826,     0,   312,     0,
     110,  1215,   313,     0,   314,     0,   110,     0,     0,     0,
       0,   104,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   315,   110,   110,     0,     0,     0,   110,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,   114,   826,   826,     0,     0,   114,   316,   317,
       0,   318,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,   319,   320,   321,     0,   322,   323,   324,     0,
     325,   326,     0,     0,     0,     0,     0,     0,    69,     0,
     114,     0,     0,     0,   246,   110,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   438,   327,   114,
     663,    72,   404,   254,     0,     0,   456,     0,   329,    74,
      75,   330,   331,   332,   333,     0,     0,   663,     0,     0,
       0,   663,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,     0,     0,  1302,     0,     0,
       0,     0,     0,   114,     0,  1133,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   663,     0,     0,     0,     0,
       0,     0,     0,   109,  1133,     0,   246,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
       0,     0,  1350,   110,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   369,     0,     0,   110,     0,
       0,     0,     0,     0,     0,     0,     0,   109,     0,     0,
     114,     0,     0,     0,   110,     0,   439,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     110,   110,     0,     0,   246,     0,     0,     0,     0,     0,
       0,     0,     0,   662,     0,     0,     0,   373,     0,     0,
     256,     0,   500,     0,     0,     0,   254,     0,   104,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,   439,
       0,   826,   826,     0,     0,     0,     0,     0,     0,   114,
     114,     0,     0,     0,     0,   826,   826,     0,     0,     0,
     438,   438,     0,   109,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,     0,     0,     0,     0,     0,   439,
     377,     0,     0,     0,     0,     0,     0,     0,     0,   826,
     826,   114,     0,     0,     0,   114,     0,   114,     0,  1302,
    1302,  1302,   147,     0,     0,     0,     0,     0,     0,     0,
     441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,   114,   114,     0,   114,     0,     0,
     114,     0,     0,   114,   114,   114,     0,     0,     0,     0,
       0,     0,   663,   439,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   663,   312,     0,     0,
       0,   313,     0,   314,     0,     0,     0,     0,     0,   663,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,   369,     0,     0,     0,     0,     0,   316,   317,   439,
     761,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   319,   320,   321,   147,   322,   323,   324,     0,   325,
     326,   373,   373,     0,     0,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,   373,     0,
       0,     0,     0,     0,     0,     0,     0,   327,    71,     0,
     762,   763,     0,   826,   826,   456,     0,   329,    74,    75,
     330,   331,   332,   333,     0,     0,   373,     0,     0,   665,
       0,     0,   256,     0,     0,     0,     0,     0,     0,  1635,
       0,     0,     0,     0,     0,     0,   665,     0,     0,   826,
     665,     0,     0,     0,     0,     0,     0,   104,     0,  1655,
       0,  1655,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1655,   439,     0,     0,     0,     0,
       0,     0,     0,     0,   665,   369,     0,     0,   147,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   826,
       0,     0,     0,     0,     0,   826,   114,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,   114,
     373,   826,   826,     0,     0,     0,   438,   438,     0,     0,
       0,     0,     0,     0,     0,     0,   663,     0,     0,   254,
       0,   373,     0,     0,     0,   441,  1726,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1214,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
       0,     0,     0,     0,     0,     0,   377,     0,     0,     0,
       0,     0,     0,     0,  1747,   256,     0,   109,   373,   373,
     115,     0,     0,   115,     0,     0,   312,     0,   441,     0,
     313,     0,   314,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    54,   315,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   441,     0,
       0,     0,   373,     0,   115,     0,   316,   317,     0,   318,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
     319,   320,   321,     0,   322,   323,   324,   115,   325,   326,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,  1655,     0,   115,     0,     0,
     104,     0,  1825,     0,     0,     0,   327,     0,     0,    72,
     404,   665,   441,     0,     0,     0,   329,   431,    75,   330,
     331,   332,   333,     0,     0,     0,     0,   438,     0,   104,
       0,     0,     0,     0,   115,   665,     0,     0,     0,     0,
     115,     0,   115,  1655,     0,     0,     0,   254,   665,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,  1655,
    1825,   114,     0,     0,     0,     0,     0,     0,     0,   114,
     439,     0,     0,     0,   115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,   441,     0,
       0,     0,     0,     0,     0,  1655,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     377,   377,  1907,     0,     0,     0,   114,     0,   663,     0,
       0,     0,     0,     0,     0,     0,     0,   377,     0,   826,
       0,     0,     0,     0,     0,   114,   115,     0,     0,   115,
       0,     0,     0,   114,   115,     0,   373,   373,     0,     0,
       0,     0,     0,     0,     0,   377,     0,     0,     0,     0,
     373,   373,     0,     0,     0,   373,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,   377,   373,   373,   115,     0,     0,     0,
       0,  1414,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,   441,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,   312,     0,     0,     0,   313,     0,   314,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    54,   315,     0,     0,     0,     0,   377,
       0,     0,     0,   114,   114,   114,   114,   114,   114,     0,
       0,     0,     0,     0,     0,   665,     0,     0,   256,     0,
     377,   316,   317,     0,   318,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   319,   320,   321,     0,   322,
     323,   324,     0,   325,   326,     0,   439,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,   119,   377,   377,   119,
       0,   327,     0,     0,    72,   404,     0,     0,     0,     0,
       0,   329,  1415,    75,   330,   331,   332,   333,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   377,     0,     0,     0,     0,   115,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,     0,   119,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   104,     0,   104,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,   115,   109,
       0,     0,   115,     0,   115,     0,     0,     0,     0,   104,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,     0,     0,   114,   109,   115,
     119,   115,   115,     0,   115,     0,   119,   115,   119,     0,
     115,   115,   115,     0,   373,     0,   256,     0,     0,     0,
     373,     0,   119,   114,     0,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,   373,     0,   441,
     119,   373,   373,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,   665,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   254,
       0,     0,   119,     0,     0,   119,     0,     0,     0,     0,
     119,     0,     0,     0,     0,   377,   377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
     377,     0,     0,   115,   377,   377,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,   170,   119,   377,   377,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   207,     0,     0,     0,     0,     0,
     104,    13,    14,    15,    16,    17,   119,   663,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,   373,     0,     0,     0,    42,    43,     0,     0,
       0,     0,     0,     0,   285,     0,     0,   286,   104,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
     306,     0,     0,   115,   104,   663,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   115,   115,     0,     0,     0,
       0,     0,     0,     0,     0,   441,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
     104,     0,     0,     1,     0,     0,   141,     0,     0,     0,
       0,     0,     0,   460,     0,     0,     0,   114,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   377,   377,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   511,     0,
       0,     0,   119,   119,     0,     0,     0,     0,   167,   114,
       0,     0,     0,     0,     0,     0,   188,     0,     0,   167,
       0,     0,     0,   377,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,   557,     0,   119,     0,
     119,     0,   559,   561,     0,     0,     0,   568,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,     0,     0,   278,   119,     0,   119,   119,     0,
     119,     0,     0,   119,     0,     0,   119,   119,   119,     0,
       0,     0,     0,   377,     0,     0,     0,     0,     0,   377,
       0,     0,     0,     0,     0,   199,     0,     0,     0,     0,
       0,   209,   210,     0,     0,   377,   377,     0,     0,     0,
     377,   377,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   700,     0,     0,     0,   272,   306,     0,   115,   306,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,   278,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   256,     0,
       0,     0,   503,   207,     0,   115,     0,     0,     0,     0,
       0,     0,   278,     0,     0,   756,   757,     0,     0,     0,
       0,     0,   278,   115,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,   534,   538,     0,     0,
       0,     0,   115,   545,   546,     0,     0,     0,     0,     0,
     115,     0,     0,     0,    13,    14,    15,    16,    17,   556,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,  -401,  -401,     0,  -401,    42,
      43,   596,  -401,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,   665,     0,     0,    54,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   660,
     550,   377,     0,     0,     0,   306,     0,     0,     0,     0,
       0,     0,    59,    60,     0,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,   109,   665,     0,     0,   716,     0,     0,
       0,   119,   119,     0,     0,     0,     0,     0,     0,     0,
     115,   115,   115,   115,   115,   115,     0,     0,     0,   733,
      72,     0,     0,   736,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   758,     0,     0,     0,   759,   760,     0,     0,
       0,     0,     0,     0,     0,   775,   776,   777,   778,     0,
       0,     0,     0,   377,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   800,     0,     0,     0,     0,     0,
       0,     0,   803,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     700,     0,     0,     0,   737,     0,   738,     0,     0,     0,
     278,     0,     0,     0,     0,   754,   755,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   841,     0,     0,     0,   115,     0,     0,   534,     0,
       0,  1013,     0,   846,     0,     0,     0,     0,  1025,     0,
       0,     0,   237,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,   862,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,  -401,  -401,   115,  -401,    42,    43,     0,  -401,
       0,     0,     0,   836,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
     115,  1084,   115,     0,   119,     0,     0,     0,     0,     0,
       0,   917,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,    59,
      60,     0,     0,   938,   943,     0,     0,     0,     0,   115,
       0,   119,     0,     0,     0,   115,     0,     0,   312,     0,
       0,     0,   313,     0,   314,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   315,     0,     0,     0,     0,     0,    72,   119,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,   985,
       0,     0,     0,   986,     0,     0,     0,     0,   316,   317,
       0,   318,   938,    59,    60,    61,    62,    63,    64,    65,
      66,    67,   319,   320,   321,     0,   322,   323,   324,     0,
     325,   326,     0,     0,     0,     0,  1028,     0,    69,  1222,
       0,     0,     0,     0,   700,     0,     0,  1039,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,   327,  1173,
       0,    72,   404,     0,     0,     0,  1174,     0,   329,    74,
      75,   330,   331,   332,   333,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1011,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,     0,     0,     0,  1157,   119,   119,   119,   119,
     119,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1323,  1325,  1327,     0,     0,     0,     0,     0,     0,     0,
    1080,  1081,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1139,  1140,  1141,     0,     0,  1143,     0,     0,     0,
    1346,     0,     0,     0,   115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1084,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,  1177,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1233,     0,     0,     0,  1234,   115,     0,     0,     0,
       0,   119,   938,     0,     0,     0,   161,     0,     0,     0,
       0,     0,  1247,     0,     0,     0,     0,     0,     0,  1248,
       0,     0,     0,     0,     0,     0,     0,   405,  1252,     0,
    1253,     0,   161,     0,     0,     0,   700,     0,     0,  1259,
       0,     0,     0,     0,     0,     0,     0,  1267,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,  1281,     0,     0,     0,  1282,  1231,     0,
       0,     0,     0,     0,     0,     0,     0,   161,     0,     0,
       0,   141,     0,     0,     1,     0,   119,     0,   119,     0,
     161,     0,   161,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1250,     0,     0,     0,     0,
       0,   119,     0,     0,  1254,  1255,  1256,  1257,     0,     0,
       0,     0,   397,  1500,     0,   119,  1502,     0,   159,     0,
       0,   119,     0,  1271,     0,     0,     0,     0,     0,   397,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1285,     0,  1286,     0,     0,     0,
       0,     0,     0,     0,     0,   571,     0,     0,   405,   576,
       0,     0,     0,     0,     0,     0,   161,     0,   579,   580,
     161,     0,     0,   161,   161,     0,     0,   161,     0,     0,
     161,   161,     0,     0,   405,   405,     0,     0,     0,   270,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1342,
       0,     0,   276,     0,   277,     0,  1403,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,  1356,     0,     0,     0,     0,
    1426,  1360,     0,  1362,  1364,     0,     0,     0,     0,     0,
       0,     0,   161,     0,     0,   161,     0,     0,     0,   405,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   405,  1465,     0,  1397,     0,
    1466,     0,     0,     0,     0,     0,     0,  1404,     0,  1405,
       0,  1406,     0,  1408,     0,   485,   486,     0,  1416,   490,
       0,     0,   493,   494,     0,     0,  1641,     0,     0,     0,
    1501,     0,     0,     0,     0,     0,     0,   161,  1431,  1432,
       0,     0,   119,  1511,  1513,     0,     0,     0,     0,     0,
       0,     0,   161,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1458,  1459,     0,     0,     0,
       0,     0,     0,  1462,     0,     0,     0,     0,     0,     0,
    1547,     0,     0,  1550,     0,     0,     0,     0,     0,     0,
     119,     0,     0,   161,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   209,     0,     0,   119,     0,     0,     0,
       0,     0,     0,   700,     0,     0,     0,     0,     0,     0,
       0,  1571,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1577,     0,     0,     0,     0,     0,   397,     0,
       0,     0,   119,  1537,     0,     0,     0,     0,     0,     0,
     161,     0,     0,     0,     0,     0,     0,  1593,     0,   658,
       0,  1594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   690,  1598,  1599,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   405,   405,   405,   405,   405,   405,   405,   405,   405,
     405,   405,   405,   405,   405,   405,   405,   405,   405,   405,
    1575,  1775,     0,     0,     0,   727,     0,     0,  1580,     0,
    1582,     0,     0,     0,   397,     0,     0,     0,     0,     0,
     700,   312,  1589,  1590,     0,   313,     0,   314,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1595,  1596,     0,
    1597,     0,     0,     0,   315,     0,     0,     0,  1600,     0,
       0,     0,     0,     0,  1605,     0,     0,     0,     0,     0,
    1610,  1611,   796,     0,  1683,  1684,     0,     0,     0,     0,
       0,   316,   317,     0,   318,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   319,   320,   321,     0,   322,
     323,   324,     0,   325,   326,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,   405,     0,     0,     0,     0,
       0,     0,     0,     0,   161,   161,     0,     0,     0,     0,
       0,   327,   771,     0,    72,   404,     0,     0,     0,     0,
       0,   329,    74,    75,   330,   331,   332,   333,     0,     0,
     161,   161,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   161,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1697,  1698,     0,     0,     0,     0,     0,     0,     0,
       0,  1704,     0,   161,     0,  1764,     0,     0,     0,     0,
       0,     0,     0,     0,  1717,     0,     0,     0,     0,     0,
       0,  1718,  1719,     0,   161,     0,     0,   161,   161,     0,
     161,   405,   161,   161,     0,     0,   405,  1550,     0,     0,
       0,     0,     0,     0,     0,  1789,     0,   405,     0,     0,
       0,     0,     0,     0,     0,     0,   923,   924,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   161,
       0,     0,  1805,   161,     0,     0,     0,     0,     0,     0,
       0,     0,   949,   950,     0,     0,     0,     0,   405,     0,
       0,     0,     0,  1821,     0,   957,  1822,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   312,  1780,     0,  1781,
     313,     0,   314,     0,     0,     0,     0,     0,     0,     0,
    1787,  1788,     0,     0,     0,   979,     0,  1790,     0,   315,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1000,     0,     0,  1003,
    1004,     0,  1007,     0,  1009,  1010,   316,   317,     0,   318,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
     319,   320,   321,     0,   322,   323,   324,     0,   325,   326,
       0,  1891,     0,     0,     0,     0,    69,     0,     0,     0,
       0,  1051,     0,     0,     0,  1055,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   327,     0,     0,    72,
     404,     0,     0,     0,   274,     0,   329,    74,    75,   330,
     331,   332,   333,  1860,     0,     0,   161,     0,     0,     0,
    1868,     0,     0,     0,     0,     0,  1872,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   161,     0,     0,
       0,     0,     0,     0,  1904,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   384,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1921,     0,
       0,     0,     0,     0,     0,     0,  1926,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   161,   428,   384,   161,
       0,  1939,     0,     0,     0,     0,     0,   405,   405,   405,
       0,     0,     0,     0,   405,   405,     0,     0,  1216,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   484,     0,
       0,     0,     0,     0,     0,   484,     0,   405,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   405,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1294,     0,     0,     0,     0,     0,  1295,     0,  1216,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,   484,     0,    42,    43,     0,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,  1274,     0,
       0,  1278,     0,     0,     0,    54,  1296,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   161,     0,
       0,     0,     0,     0,     0,    57,     0,     0,    59,    60,
       0,     0,     0,   161,     0,     0,     0,     0,   484,     0,
       0,   161,   161,     0,     0,     0,     0,   384,   675,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
     161,     0,     0,   161,     0,   161,   161,     0,   696,     0,
       0,     0,  1297,     0,     0,     0,    72,   852,     0,     0,
       0,     0,     0,     0,    74,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   484,   729,     0,   484,   732,   161,     0,     0,     0,
       0,     0,   384,     0,     0,     0,   675,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1399,     0,     0,     0,     0,     0,     0,   484,     0,     0,
       0,   484,     0,     0,     0,  1410,     0,     0,     0,     0,
       0,     0,     0,  1419,  1420,     0,     0,     0,     0,   405,
     405,     0,     0,   161,     0,     0,     0,     0,     0,     0,
       0,     0,  1435,   384,     0,  1439,     0,  1442,  1443,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     356,     0,     0,     0,     0,     0,     0,     0,     0,   312,
       0,     0,     0,   313,     0,   314,     0,     0,   356,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1470,     0,
       0,   433,   315,   484,     0,     0,   384,     0,     0,     0,
       0,     0,     0,     0,     0,   457,     0,     0,     0,     0,
       0,     0,     0,     0,   405,   161,     0,     0,     0,   316,
     317,     0,   318,     0,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   319,   320,   321,   384,   322,   323,   324,
       0,   325,   326,     0,     0,   161,     0,     0,     0,    69,
       0,     0,     0,     0,     0,  1544,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   327,
     883,   161,    72,   404,     0,     0,     0,   161,     0,   329,
      74,    75,   330,   331,   332,   333,     0,     0,   484,   484,
       0,   551,     0,     0,     0,     0,     0,     0,     0,   933,
     384,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     675,     0,     0,     0,   675,     0,     0,     0,     0,     0,
       0,   951,     0,   384,     0,     0,     0,     0,   161,     0,
       0,     0,     0,     0,     0,     0,     0,  1439,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   484,   981,     0,   484,   984,     0,  1607,     0,     0,
       0,     0,     0,     0,     0,     0,   384,     0,     0,   675,
       0,   675,   675,     0,     0,     0,     0,     0,   675,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   384,   384,
       0,     0,     0,     0,   196,     0,   161,   161,     0,     0,
     405,     0,     0,     0,   397,   384,     0,     0,   161,   484,
     250,     0,     0,   484,     0,     0,     0,   484,  1053,     0,
       0,   484,  1057,     0,     0,     0,     0,     0,     0,  1060,
       0,     0,     0,     0,   405,     0,     0,     0,     0,     0,
    1681,     0,     0,     0,     0,     0,     0,     0,     0,   457,
       0,     0,   772,     0,     0,     0,     0,     0,     0,   196,
       0,     0,     0,   298,     0,     0,     0,     0,     0,     0,
       0,   384,   484,   196,   389,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   196,   384,     0,     0,     0,     0,     0,     0,     0,
     161,     0,     0,     0,   448,     0,     0,   452,     0,     0,
       0,     0,     0,     0,     0,     0,   405,   405,  1730,  1731,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1735,     0,     0,     0,     0,     0,     0,     0,     0,   840,
       0,     0,     0,   405,     0,     0,     0,     0,     0,     0,
       0,   484,     0,     0,     0,     0,   196,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   161,     0,   405,   250,
     856,     0,     0,     0,     0,     0,   675,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   772,   876,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   886,
       0,   891,   886,     0,     0,   452,     0,     0,     0,     0,
       0,     0,     0,   196,     0,   405,   895,     0,     0,   675,
     675,     0,  1798,     0,     0,     0,   675,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   594,     0,   161,     0,     0,     0,     0,
     929,     0,     0,   433,     0,     0,     0,   196,     0,     0,
       0,     0,     0,     0,     0,     0,   945,     0,     0,   384,
       0,     0,     0,     0,   484,  1276,     0,   484,  1280,     0,
       0,     0,     0,   668,     0,   685,     0,     0,  1849,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   975,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   433,     0,     0,   876,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   668,     0,
       0,     0,     0,     0,   753,     0,     0,     0,     0,     0,
       0,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   384,     0,     0,
       0,     0,     0,  1061,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   196,   196,     0,     0,     0,     0,   448,
       0,     0,     0,     0,     0,     0,   484,  1401,     0,     0,
       0,     0,     0,     0,     0,   384,     0,     0,     0,     0,
     356,   675,  1412,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   876,     0,     0,     0,     0,
       0,     0,     0,  1170,     0,     0,     0,     0,   484,  1437,
     886,   675,   389,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   384,   384,     0,     0,   594,   457,   594,   594,
       0,   594,     0,     0,   594,     0,     0,   594,   594,   594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   448,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1206,  1208,
       0,     0,     0,     0,     0,     0,   433,     0,     0,     0,
       0,     0,     0,     0,     0,   312,     0,     0,     0,   313,
       0,   314,     0,     0,     0,     0,     0,   457,     0,     0,
       0,     0,     0,     0,     0,     0,   886,     0,   315,     0,
       0,     0,     0,   196,     0,     0,   448,     0,   937,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   316,   317,     0,   318,   668,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   319,
     320,   321,     0,   322,   323,   324,     0,   325,   326,     0,
     196,  1262,     0,   457,     0,    69,     0,     0,     0,     0,
       0,  1269,     0,   384,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   327,  1268,     0,    72,   404,
       0,     0,   448,     0,     0,   329,    74,    75,   330,   331,
     332,   333,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   448,   448,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   448,     0,     0,     0,     0,     0,  1334,  1334,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   237,   484,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,   484,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,  -401,  -401,
     433,  -401,    42,    43,     0,  -401,     0,   448,     0,     0,
       0,     0,     0,   196,     0,     0,     0,   457,     0,     0,
       0,     0,    54,     0,     0,     0,     0,   384,   389,     0,
     886,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1407,     0,     0,    59,    60,     0,  1417,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   384,   384,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   484,   484,     0,
       0,   753,     0,    72,   296,     0,     0,     0,   457,     0,
    1461,    74,    75,   484,     0,  1464,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   886,     0,     0,     0,   237,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,   457,     0,    19,   772,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,  -401,  -401,     0,
    -401,    42,    43,     0,  -401,   448,   484,     0,     0,     0,
       0,     0,     0,     0,   484,     0,     0,     0,     0,     0,
       0,    54,   457,     0,   772,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   594,   975,     0,     0,     0,
       0,     0,     0,     0,    59,    60,  1578,  1579,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   384,
       0,     0,     0,     0,   484,  1851,     0,     0,   484,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   250,
       0,     0,    72,   239,   484,     0,     0,     0,     0,     0,
      74,    75,     0,     0,     0,     0,     0,     0,     0,   196,
       0,     0,     0,   389,     0,     0,     0,   594,   356,     0,
       0,     0,     0,  1634,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   484,   484,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     312,   668,     0,     0,   313,  1673,   314,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   484,     0,     0,
       0,     0,     0,   315,     0,     0,     0,  1685,     0,     0,
    1687,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   448,   448,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,   594,   594,   594,
       0,   594,   594,     0,     0,     0,     0,     0,   452,     0,
     327,     0,     0,    72,   404,     0,     0,   915,     0,     0,
     329,    74,    75,   330,   331,   332,   333,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   250,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1085,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,   312,   389,
      42,    43,   313,     0,   314,    44,    45,    46,    47,    48,
      49,    50,    51,    52,     0,     0,     0,    53,     0,  1086,
      54,  1087,    -2,     0,  1088,     0,     0,  1089,  1090,  1091,
    1092,  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,  -280,
    1101,  1102,  1103,  1104,  1105,   886,  1106,     0,   316,   317,
      57,   761,     0,  1107,  1108,    61,    62,    63,    64,    65,
      66,    67,   319,   320,   321,  1109,   322,   323,   324,   312,
     325,   326,     0,   313,     0,   314,     0,     0,    69,     0,
       0,   196,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   315,     0,     0,     0,     0,    -3,   327,    71,
       0,    72,   328,     0,     0,     0,   274,     0,   329,    74,
      75,   330,   331,   332,   333,     0,     0,     0,     0,   316,
     317,     0,   318,  -172,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   319,   320,   321,     0,   322,   323,   324,
       0,   325,   326,   389,     0,     0,     0,     0,     0,    69,
       0,     0,   594,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   327,
       0,     0,    72,   404,     0,     0,     0,  1328,     0,   329,
      74,    75,   330,   331,   332,   333,     0,     0,     0,     0,
       0,     0,     0,     0,   448,   448,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     4,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1085,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,   250,     0,   312,     0,    42,    43,   313,     0,   314,
      44,    45,    46,    47,    48,    49,    50,    51,    52,     0,
       0,     0,    53,     0,  1086,    54,  1087,    -2,     0,  1088,
       0,     0,  1089,  1090,  1091,  1092,  1093,  1094,  1095,  1096,
    1097,  1098,  1099,  1100,  -280,  1101,  1102,  1103,  1104,  1105,
       0,  1106,     0,   316,   317,    57,   761,     0,  1107,  1108,
      61,    62,    63,    64,    65,    66,    67,   319,   320,   321,
    1109,   322,   323,   324,     0,   325,   326,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   250,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   327,    71,     0,    72,   328,     0,     0,
     594,   274,     0,   329,    74,    75,   330,   331,   332,   333,
       0,     0,     0,     0,     0,     0,     0,     0,  -172,     0,
       0,     0,     0,     0,     0,   448,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   594,     0,
       0,   452,     4,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,   312,     0,    42,    43,   313,     0,
     314,    44,    45,    46,    47,    48,    49,    50,    51,    52,
       0,     0,     0,    53,     0,     0,    54,   315,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   316,   317,    57,   318,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,   319,   320,
     321,     0,   322,   323,   324,     0,   325,   326,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,     0,     0,    72,   328,     0,
       0,     0,     0,     0,   329,    74,    75,   330,   331,   332,
     333,     0,     0,     0,     0,     0,     0,     0,  1658,  1659,
       4,   238,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,   312,     0,    42,    43,   313,     0,   314,    44,
      45,    46,    47,    48,    49,    50,    51,    52,     0,     0,
       0,    53,     0,     0,    54,   315,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   316,   317,    57,   318,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,   319,   320,   321,     0,
     322,   323,   324,     0,   325,   326,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   327,     0,     0,    72,   328,     0,     0,     0,
       0,     0,   329,    74,    75,   330,   331,   332,   333,   238,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,   365,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,    72,   430,     0,     0,     0,     0,     0,
     329,   431,    75,   330,   331,   332,   333,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,   312,     0,
      42,    43,   313,     0,   314,   365,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      54,   315,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   316,   317,
       0,   318,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,   319,   320,   321,     0,   322,   323,   324,     0,
     325,   326,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   327,     0,
       0,    72,  1203,     0,     0,     0,     0,     0,   329,  1204,
      75,   330,   331,   332,   333,   238,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,   312,     0,    42,    43,
     313,     0,   314,   365,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,   315,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   316,   317,     0,   318,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
     319,   320,   321,     0,   322,   323,   324,     0,   325,   326,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   327,     0,     0,    72,
     404,     0,     0,     0,     0,     0,   329,    74,    75,   330,
     331,   332,   333,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,   312,     0,    42,    43,   313,     0,
     314,   365,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,   315,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   316,   317,     0,   318,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,   319,   320,
     321,     0,   322,   323,   324,     0,   325,   326,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,     0,     0,    72,   430,     0,
       0,     0,     0,     0,   329,    74,    75,   330,   331,   332,
     333,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,     0,     0,    42,    43,     0,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,    52,     0,
       0,     0,    53,     0,     0,    54,    55,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    56,     0,     0,     0,    57,    58,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
      68,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,    71,     0,    72,    73,     0,     0,
       0,     0,     0,     0,    74,    75,   237,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,  -401,  -401,     0,  -401,
      42,    43,     0,  -401,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      54,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,  -400,  -400,     0,  -400,    42,    43,     0,
    -400,    58,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,    54,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,    71,
       0,    72,   239,     0,     0,     0,  -704,     0,     0,    74,
      75,     4,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,     0,     0,    42,    43,     0,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,    52,     0,
       0,     0,    53,     0,     0,    54,     0,     0,     0,     0,
    -333,  -333,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,    59,    60,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -333,     0,     0,     0,    72,    73,     0,     0,
       0,     0,     0,     0,    74,    75,     4,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,     0,     0,
      42,    43,     0,     0,     0,    44,    45,    46,    47,    48,
      49,    50,    51,    52,     0,     0,     0,    53,     0,     0,
      54,     0,     0,     0,     0,  -334,  -334,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,    59,    60,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -334,     0,     0,
       0,    72,    73,     0,     0,     0,     0,     0,     0,    74,
      75,   237,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,  -401,  -401,     0,  -401,    42,    43,     0,  -401,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,    54,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,  -401,  -401,
       0,  -401,    42,    43,     0,  -401,    58,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    70,    71,     0,    72,   239,     0,     0,
    1294,     0,     0,     0,    74,    75,  1295,     0,     0,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,     0,     0,    42,    43,     0,     0,     0,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,  1296,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1476,     0,     0,     0,    72,   852,     0,     0,  1294,
       0,     0,     0,    74,    75,  1295,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,     0,     0,    42,    43,     0,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    54,  1296,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,    59,    60,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1477,     0,     0,     0,    72,   852,     0,     0,  1294,     0,
       0,     0,    74,    75,  1295,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
       0,     0,    42,    43,     0,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,  1296,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,    59,    60,     0,     0,  1318,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,   312,     0,     0,
       0,   313,     0,   314,     0,     0,     0,     0,     0,  1478,
       0,     0,     0,    72,   852,     0,     0,     0,  1086,     0,
     315,    74,    75,  1088,  1737,  1738,  1089,  1090,  1091,  1092,
    1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,  -280,  1101,
    1102,  1103,  1104,  1105,     0,  1106,     0,   316,   317,     0,
     761,     0,  1107,  1108,    61,    62,    63,    64,    65,    66,
      67,   319,   320,   321,  1109,   322,   323,   324,     0,   325,
     326,     0,     0,     0,     0,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,  1318,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   327,    71,     0,
      72,   404,     0,     0,     0,   274,     0,   329,    74,    75,
     330,   331,   332,   333,   312,     0,     0,     0,   313,     0,
     314,     0,  -172,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1086,     0,   315,    -2,     0,
    1088,     0,     0,  1089,  1090,  1091,  1092,  1093,  1094,  1095,
    1096,  1097,  1098,  1099,  1100,  -280,  1101,  1102,  1103,  1104,
    1105,     0,  1106,     0,   316,   317,     0,   761,     0,  1107,
    1108,    61,    62,    63,    64,    65,    66,    67,   319,   320,
     321,  1109,   322,   323,   324,     0,   325,   326,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,    71,     0,    72,   404,     0,
       0,     0,   274,     0,   329,    74,    75,   330,   331,   332,
     333,     0,     0,     0,     0,     0,     0,     0,     0,  -172,
     238,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,  -401,
    -401,     0,  -401,    42,    43,     0,  -401,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    54,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,  1318,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   312,     0,     0,     0,   313,     0,   314,
       0,    70,    71,     0,    72,   239,     0,     0,     0,  -708,
       0,     0,    74,    75,  1086,     0,   315,     0,     0,  1088,
       0,     0,  1089,  1090,  1091,  1092,  1093,  1094,  1095,  1096,
    1097,  1098,  1099,  1100,  -280,  1101,  1102,  1103,  1104,  1105,
       0,  1106,     0,   316,   317,     0,   761,     0,  1107,  1108,
      61,    62,    63,    64,    65,    66,    67,   319,   320,   321,
    1109,   322,   323,   324,     0,   325,   326,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   327,    71,     0,    72,   404,     0,     0,
       0,   274,     0,   329,    74,    75,   330,   331,   332,   333,
       0,     0,     0,     0,     0,     0,     0,     0,  -172,   238,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,  -401,  -401,
       0,  -401,    42,    43,     0,  -401,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,    71,     0,    72,   239,     0,     0,     0,     0,     0,
       0,    74,    75,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,   365,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,  1020,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -579,    72,   366,     0,
       0,     0,     0,     0,     0,    74,    75,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,     0,     0,
      42,    43,     0,     0,     0,   365,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    59,    60,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
    1713,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   366,     0,     0,     0,     0,     0,     0,    74,
      75,   238,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,     0,     0,    42,    43,     0,     0,     0,   365,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,  1715,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,   366,     0,     0,     0,
       0,     0,     0,    74,    75,   238,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
       0,     0,     0,   365,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
     366,     0,     0,     0,     0,     0,     0,    74,    75,   238,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
       0,     0,    42,    43,     0,     0,     0,   365,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,   296,     0,     0,     0,     0,     0,
       0,    74,    75,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,  -401,  -401,     0,  -401,    42,    43,     0,  -401,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,   239,     0,
       0,     0,     0,     0,     0,    74,    75,    13,    14,    15,
      16,    17,    18,   581,    19,   582,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   583,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,    72,   584,     0,     0,     0,   274,     0,
     329,    74,    75,   585,   586,   332,   333,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,   410,    72,   328,     0,     0,     0,     0,     0,
     329,    74,    75,   330,   331,   332,   333,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,    72,   584,     0,     0,     0,   274,     0,
     329,    74,    75,   330,   331,   332,   333,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,    72,   328,     0,     0,     0,     0,     0,
     329,    74,    75,   330,   331,   332,   333,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,   365,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,    72,   430,     0,     0,     0,     0,     0,
     329,    74,    75,   330,   331,   332,   333,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
     312,     0,    42,    43,   313,     0,   314,   365,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,     0,   325,   326,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,    72,   404,     0,     0,     0,     0,     0,
     329,    74,    75,   330,   331,   332,   333,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
       0,     0,    42,    43,     0,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,    71,     0,    72,    73,     0,     0,     0,  -706,     0,
       0,    74,    75,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
       0,     0,     0,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    70,    71,     0,    72,
      73,     0,     0,     0,     0,     0,     0,    74,    75,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,     0,     0,    42,    43,     0,     0,     0,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    71,     0,    72,    73,     0,    13,    14,
      15,    16,    17,    74,    75,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,  -401,
    -401,     0,  -401,    42,    43,     0,  -401,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    54,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    70,    71,     0,    72,   296,     0,     0,     0,     0,
       0,     0,    74,    75,   539,   238,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
       0,     0,     0,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,    59,    60,     0,    42,    43,     0,     0,     0,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   858,    72,   852,     0,     0,     0,
       0,     0,     0,    74,    75,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,     0,     0,
      42,    43,     0,     0,     0,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,  1370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    59,    60,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   852,     0,     0,     0,     0,     0,     0,    74,
      75,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,   281,     0,
       0,     0,     0,     0,     0,    74,    75,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
       0,     0,    42,    43,     0,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,    73,     0,     0,     0,     0,     0,
       0,    74,    75,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
       0,     0,     0,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
     426,     0,     0,     0,     0,     0,     0,    74,    75,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,     0,     0,    42,    43,     0,     0,     0,   365,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,   366,     0,     0,     0,
       0,     0,     0,    74,    75,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,     0,     0,
      42,    43,     0,     0,     0,   365,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    59,    60,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   281,     0,     0,     0,     0,     0,     0,    74,
      75,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,   365,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,   426,     0,
       0,     0,     0,     0,     0,    74,    75,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
       0,     0,    42,    43,     0,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,   852,     0,     0,     0,     0,     0,
       0,    74,    75,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
       0,     0,     0,   365,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
     296,     0,     0,     0,     0,     0,     0,    74,    75,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,     0,     0,    42,    43,     0,     0,     0,   365,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,   852,     0,     0,     0,
       0,     0,     0,    74,    75,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,     0,     0,
      42,    43,     0,     0,     0,   365,    45,    46,    47,    48,
      49,    50,    51,     0,    13,    14,    15,    16,    17,     0,
      54,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,  -401,  -401,     0,  -401,    42,
      43,     0,  -401,    59,    60,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,    59,    60,     0,     0,     0,     0,     0,    74,
      75,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,   296,     0,    13,    14,    15,    16,    17,    74,    75,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,  -401,  -401,     0,  -401,    42,    43,
       0,  -401,     0,     0,   238,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,    54,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,  -401,  -401,     0,  -401,    42,    43,     0,
    -401,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    54,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
      59,    60,     0,     0,     0,     0,     0,    74,    75,   238,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    72,     0,
       0,     0,    42,    43,     0,     0,     0,   365,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   825,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -592,    72,   238,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,     0,     0,     0,     0,    42,    43,     0,
       0,     0,   365,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    54,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    60,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1642,     0,     0,
       0,     0,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,    72,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,     0,     0,    42,    43,     0,     0,     0,
     365,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    54,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,    59,    60,
       0,    42,    43,     0,     0,     0,    44,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,    58,     0,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
      71,     0,    72,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,    59,    60,     0,    42,    43,     0,     0,     0,   365,
      45,    46,    47,    48,    49,    50,    51,   312,     0,     0,
       0,   313,     0,   314,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,   316,   317,     0,
     318,     0,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   319,   320,   321,     0,   322,   323,   324,   312,   325,
     326,     0,   313,     0,   314,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,   315,     0,     0,     0,     0,     0,   327,     0,  1743,
      72,   404,     0,     0,     0,     0,     0,   329,    74,    75,
     330,   331,   332,   333,     0,     0,     0,     0,   316,   317,
       0,   318,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,   319,   320,   321,     0,   322,   323,   324,   312,
     325,   326,     0,   313,     0,   314,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   315,     0,     0,     0,     0,     0,   327,  1944,
       0,    72,   404,     0,     0,     0,     0,     0,   329,    74,
      75,   330,   331,   332,   333,     0,     0,     0,     0,   316,
     317,     0,   318,     0,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   319,   320,   321,     0,   322,   323,   324,
     312,   325,   326,     0,   313,     0,   314,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   315,     0,     0,     0,     0,     0,   327,
       0,     0,    72,   404,     0,     0,     0,     0,     0,   329,
      74,    75,   330,   331,   332,   333,     0,     0,     0,     0,
     316,   317,     0,   318,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,   319,   320,   321,     0,   322,   323,
     324,   312,   325,   326,     0,   313,     0,   314,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   315,     0,     0,     0,     0,     0,
     570,     0,     0,    72,   404,     0,     0,     0,     0,     0,
     329,    74,    75,   330,   331,   332,   333,     0,     0,     0,
       0,   316,   317,     0,   318,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   319,   320,   321,     0,   322,
     323,   324,   312,   325,   326,     0,   313,     0,   314,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   315,     0,     0,     0,     0,
       0,   575,     0,     0,    72,   404,     0,     0,     0,     0,
       0,   329,    74,    75,   330,   331,   332,   333,     0,     0,
       0,     0,   316,   317,     0,   318,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,   319,   320,   321,     0,
     322,   323,   324,   312,   325,   326,     0,   313,     0,   314,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   315,     0,     0,     0,
       0,     0,   578,     0,     0,    72,   404,     0,     0,     0,
       0,     0,   329,    74,    75,   330,   331,   332,   333,     0,
       0,     0,     0,   316,   317,     0,   318,     0,    59,    60,
      61,    62,    63,    64,    65,    66,    67,   319,   320,   321,
       0,   322,   323,   324,   312,   325,   326,     0,   313,     0,
     314,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   315,     0,     0,
       0,     0,     0,   327,     0,     0,    72,   404,     0,     0,
       0,     0,     0,   329,   839,    75,   330,   331,   332,   333,
       0,     0,     0,     0,   316,   317,     0,   318,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,   319,   320,
     321,     0,   322,   323,   324,     0,   325,   326,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,     0,     0,    72,   404,     0,
       0,     0,     0,     0,   329,   431,    75,   330,   331,   332,
     333
};

static const yytype_int16 yycheck[] =
{
       1,   146,     1,     4,   169,   369,   609,   174,   236,     1,
     158,    70,     4,    70,   174,   367,    70,    70,     4,   251,
     158,   158,    72,   340,   202,   683,   175,   595,   739,   278,
     327,   214,   384,   215,   384,   894,   388,    70,   388,   135,
       1,    70,   702,     4,     1,    70,   159,   217,   556,  1617,
     214,    52,    53,   649,    55,   767,    55,     1,   670,     1,
     588,   497,   498,    55,    91,   742,   834,   173,   670,    70,
       1,   748,  1617,   888,   438,     1,  1095,    78,   214,    78,
     432,   518,  1252,  1253,   739,    86,   231,   214,   188,   214,
      91,   187,    91,    94,    55,    94,  1617,    98,    55,    98,
     741,    78,   674,   670,  1741,     1,    98,   290,     4,   291,
     925,    55,    98,    55,  1679,   285,   286,    78,   177,   214,
     177,   558,   310,   177,    55,     1,   290,   739,    79,    55,
     214,   741,    84,     1,  1618,   136,   818,   739,   139,  1074,
     141,    98,   141,  1222,   177,   146,   739,    67,   177,   141,
      67,   152,   177,    93,   290,   214,   739,   214,   159,    55,
     214,   214,   844,   290,     0,   290,   143,    55,    94,   741,
     111,    84,   739,   127,   135,   176,   177,   176,  1737,    55,
     141,   214,   143,    92,   141,   214,  1511,    55,   215,   214,
     191,   116,   191,   224,   145,   290,     0,   141,    94,   141,
     127,   153,    98,   151,   205,   145,   290,   161,   149,  1144,
     141,  1536,   400,   214,   215,   141,   215,   151,   249,   269,
     676,   657,   170,   148,   126,   145,   187,   161,   259,   367,
     231,   290,   149,   290,   161,   142,   290,   290,  1079,   240,
     153,   240,  1726,    84,   834,   141,    56,    57,   480,   250,
     758,   759,   253,   141,   253,   967,   111,  1894,  1079,   260,
     327,   170,   169,   240,   291,   141,   168,   775,   704,   270,
     271,    94,   273,   141,   877,   145,   237,    99,   989,   240,
    1180,   103,   105,   466,   106,  1922,   108,   743,   127,   290,
     291,   747,   291,   475,   432,  1854,   545,   298,   662,   169,
     756,   757,   466,   304,   305,  1873,   966,   659,   309,   659,
     607,   398,   153,   990,   401,  1174,  1079,   278,   142,  1087,
     297,   278,   686,   436,   147,   921,    67,   253,  1873,   693,
     466,   683,   428,   841,   989,   151,   297,   153,   866,   466,
    1824,   466,  1421,   153,   696,   169,   696,   278,  1116,   759,
     495,  1916,  1873,  1288,   862,   996,   501,   253,   153,   223,
     170,   147,   226,   151,   341,   775,   152,   206,   191,   552,
     973,   466,   554,   161,   946,   170,    67,   989,   815,   145,
     341,    67,   466,   700,   248,   127,   996,   989,   552,  1071,
    1874,   145,   393,   147,   258,    67,   989,   398,    72,    73,
     401,   402,   278,   503,   145,    67,   989,   466,   149,   466,
     278,   412,   466,   412,   156,   157,   552,   151,   556,   556,
       0,   243,   989,  1907,   996,   552,   142,   552,  1598,  1599,
     253,   841,   148,   466,   435,   436,   170,   466,   145,    67,
     279,   466,  1856,   369,  1259,    78,   447,   448,   475,    70,
    1291,  1292,  1293,   169,   145,   456,  1870,   552,   149,   145,
     283,    67,   826,   149,    85,   466,   289,   428,   552,  1328,
    1291,  1292,  1293,   145,   475,   153,   475,   149,    19,   153,
    1192,    67,  1896,   145,   758,   759,   517,   149,   327,  1389,
    1390,  1391,   170,   552,   495,   552,   596,  1087,   552,   145,
     501,   775,   147,   570,   149,   572,   573,   153,   575,    67,
     143,   578,   438,  1225,   581,   582,   583,   145,    67,    53,
    1379,   149,    56,    57,   157,    59,  1116,   554,  1291,  1292,
    1293,  1039,   148,   149,  1211,    67,   152,   260,   539,   145,
     541,    84,   175,   149,    67,   683,   369,  1205,   271,   371,
     372,   552,   374,   554,   376,   554,    67,  1013,  1218,   145,
     660,    67,   145,   149,  1241,   150,   567,   841,   145,  1384,
     153,   935,    12,    13,    14,    15,    16,   145,   539,    72,
      73,   933,   151,   933,   545,   752,   543,   145,   545,    67,
     768,   149,   752,   145,    67,   147,   145,   149,   599,   600,
     149,   170,   603,   145,   151,   655,   607,   240,   151,   610,
     153,   145,   718,   145,   545,   438,   542,   149,   151,  1789,
     147,   145,   145,   170,   488,   152,   149,    67,   741,  1039,
     100,   101,   777,   170,   145,   151,   736,   170,   149,   145,
    1659,   170,   151,   149,  1663,   147,   510,     3,   487,  1157,
     152,   796,   516,   492,   655,   151,   520,   658,   758,   759,
     153,   170,   151,     3,   297,   121,   122,   145,  1380,   545,
     509,   149,   145,   125,   170,   775,   149,   545,   148,   502,
     519,   170,    12,    13,    14,    15,    16,   127,   148,   690,
      67,   797,   514,   145,   151,   144,   125,   149,   847,   522,
     151,   702,   151,   147,   156,   157,  1418,  1310,   341,   165,
     166,   145,   762,   170,   158,   159,   145,  1153,   152,   542,
     149,   145,   125,   153,   862,   862,   146,   156,   157,   660,
       9,   570,   153,   153,   367,  1754,   575,    67,   739,   578,
     741,   841,   145,   144,  1252,   145,   149,   169,   915,  1564,
     151,  1566,   753,   156,   157,   629,   630,   631,   597,   760,
     686,   762,    70,  1331,   145,  1039,     4,     5,     6,     7,
       8,     9,    10,    11,   147,   776,   777,   778,    86,   152,
     225,   156,     1,   167,    43,    44,   964,    46,   163,   164,
     145,    50,  1811,  1025,   102,   796,   111,   127,    53,   432,
    1819,    56,    57,   147,    59,   736,  1518,    12,    13,    14,
      15,    16,   145,  1381,   147,   147,   149,     1,   147,    98,
       4,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     147,   832,   833,   834,   151,   147,    55,   147,   147,   662,
     147,   151,   834,   145,   957,  1209,  1558,  1866,  1476,  1477,
    1478,   145,  1030,  1205,   846,    70,   636,   637,   638,   639,
     846,  1028,    67,   686,   497,   498,   151,   146,   100,   101,
     693,    55,    91,   834,   147,    94,    91,   834,   151,    98,
     881,    21,   995,   996,   145,   846,    70,   888,  1058,   145,
     834,   151,   834,   894,    78,  1403,   145,   145,   151,   147,
     826,   149,   145,   834,   147,  1222,   149,    91,   834,   147,
      94,   837,   151,   358,    98,    12,    13,    14,    15,    16,
     125,  1511,   141,  1513,   925,  1585,   104,   105,   106,   107,
     108,   146,   170,   139,   140,   141,    93,   147,   834,  1039,
     145,   151,   100,   101,   149,   151,  1536,   139,   140,   141,
     846,   156,   157,   817,   151,   161,   957,   141,   834,   151,
     961,    84,   146,   802,   170,   966,   834,   150,   151,   161,
      67,   972,   191,   153,   813,   159,   421,   816,   170,   424,
     145,   820,   145,   147,   149,   153,  1214,   151,   989,  1154,
    1155,  1703,   176,   177,   995,   996,   144,   147,  1295,   214,
     215,   151,  1147,   826,   147,   154,   147,   191,   151,   935,
     151,   147,   145,   125,   837,   151,   231,   145,   147,  1157,
    1157,   149,   151,   153,   657,   145,   471,  1194,   125,  1030,
     214,   215,  1033,   145,   253,   145,   145,   149,   145,   149,
     149,   154,  1702,   111,   156,   157,   162,   231,   145,   157,
     683,  1283,   149,     3,   147,   155,   240,   169,   151,   156,
     157,   167,    12,    13,    14,    15,    16,  1205,   125,   253,
     148,   704,  1784,   147,    98,   290,   291,   151,   102,   103,
     104,   105,   106,   107,   108,   109,   270,  1265,  1089,   273,
    1598,  1092,  1093,  1094,  1161,  1087,   150,   145,   147,  1878,
    1267,   149,  1079,  1882,  1421,  1253,   290,   291,   153,   147,
     147,   153,   935,   151,   151,  1116,   147,    67,  1079,  1301,
     151,  1122,   146,   153,  1116,   149,  1087,   147,     3,  1130,
    1087,   151,  1133,  1134,  1133,  1134,  1137,    12,    13,    14,
      15,    16,  1134,  1087,   169,  1087,  1147,   147,  1134,   147,
     147,   151,  1252,  1253,   151,  1116,  1087,  1158,   147,  1116,
     147,  1087,   151,   151,   151,   147,   147,  1757,   147,  1772,
     151,   147,  1116,  1174,  1116,   151,   147,  1134,   147,  1180,
      12,    13,    14,    15,    16,  1116,  1076,  1077,  1078,   147,
    1116,  1087,    67,   412,     4,     5,     6,     7,     8,     9,
      10,    11,   147,   648,   649,   147,   151,  1133,   147,   151,
     147,  1087,   151,   149,   847,  1805,   150,  1218,   127,  1087,
    1116,   436,    12,    13,    14,    15,    16,    17,   412,   147,
     127,  1821,   147,   151,  1073,    67,   151,  1133,  1134,   145,
    1116,  1242,   158,   159,   147,   119,   120,  1086,  1116,   147,
      60,   466,   436,   123,   124,   147,   475,   147,  1259,   147,
     475,   150,  1511,   147,  1103,  1403,  1403,  1857,   150,   151,
     156,   157,   456,   145,  1301,   153,   721,   150,   151,   724,
     495,   149,   466,  1209,   150,   151,   501,  1536,   153,  1456,
    1457,   475,   150,   151,   150,   151,  1456,  1457,   151,   152,
    1301,  1483,  1294,   153,  1305,   153,  1489,  1308,  1294,  1491,
    1133,   495,   150,   151,  1291,  1292,  1293,   501,  1295,  1296,
     150,   151,   150,   151,    65,  1489,   541,  1328,   150,   151,
    1291,  1292,  1293,  1294,  1295,  1296,  1485,   552,   783,   554,
     145,   114,   787,   116,   117,   118,   791,  1348,   150,  1350,
     147,  1350,   147,  1489,   153,   539,    73,   541,   150,   151,
     151,   152,  1489,   150,  1489,   150,   151,   153,   552,    17,
     554,   169,   145,   150,   151,   148,   149,   151,  1379,    17,
     153,   154,   153,  1384,   150,   151,  1209,   145,  1389,  1390,
    1391,   150,   151,   150,  1489,   150,   151,   150,  1294,  1547,
     170,  1501,   150,   151,   151,  1489,    17,   150,   151,  1547,
    1547,   147,  1764,   150,   151,   599,   600,   150,   151,   603,
     150,   151,   147,   607,  1350,   170,   610,   151,   152,  1535,
      72,    73,   151,   152,  1426,   147,  1489,  1181,  1182,   152,
    1426,    52,    53,    54,    55,    56,    57,    58,    59,  1632,
     152,  1599,   152,  1635,  1350,   144,  1483,   632,   633,   640,
     641,  1825,   634,   635,  1491,  1426,  1496,  1497,  1632,   147,
    1822,   655,  1311,  1312,   658,   147,   921,  1154,  1155,  1302,
     147,   147,  1483,    12,    13,    14,    15,    16,  1489,  1556,
    1491,   147,   153,   153,   153,   147,  1632,  1498,  1598,  1599,
      65,   170,   147,   147,   169,  1632,   690,  1632,   151,  1510,
    1349,  1512,    12,    13,    14,    15,    16,    17,   147,  1511,
    1153,  1513,   147,   151,   739,   147,   741,  1350,   147,   147,
    1426,    12,    13,    14,    15,    16,    17,  1632,    67,  1684,
     147,   147,   147,  1544,  1536,   147,   147,   147,  1632,   147,
    1511,   147,  1513,  1735,  1511,   739,  1513,   741,  1550,   144,
     153,   147,   777,  1564,  1550,  1566,   147,   147,  1674,   147,
    1737,   147,  1205,   147,   147,  1536,   147,  1737,   762,  1536,
    1511,   796,  1513,   150,  1585,  1511,   144,  1513,   147,  1550,
     169,   144,   776,   777,   778,   147,   125,   151,   145,  1044,
     145,   145,  1047,   145,   145,  1536,   145,    13,  1635,   152,
    1536,    69,   796,   151,   170,  1511,   145,  1513,    86,   152,
     149,    12,    13,    14,    15,    16,    17,   156,   157,   150,
     170,  1632,   150,   170,  1635,  1511,   170,  1513,   153,   144,
    1536,  1789,   144,  1511,  1645,  1513,   170,   151,   832,   147,
     147,   150,   144,   147,  1550,   151,   151,   147,   147,   144,
    1536,   145,  1768,   170,  1656,   145,  1667,    98,  1536,   170,
    1656,   102,   103,   104,   105,   106,   107,   108,    75,  1824,
     147,   147,  1683,  1684,   170,   145,  1853,  1854,  1527,  1789,
    1873,   170,  1874,  1853,  1854,   170,   170,   881,   170,  1656,
     170,  1702,   144,   144,   888,   170,   145,   147,  1735,  1873,
     894,   144,   114,  1880,   151,   146,   151,   147,   149,   144,
    1880,   147,   170,   152,   152,  1907,   150,   150,   150,  1655,
     147,   150,   153,   144,  1735,   147,   151,  1873,  1905,   147,
    1741,   925,   147,   150,  1745,   144,  1873,   147,  1873,   152,
     145,  1196,   145,   145,   103,  1756,   144,   153,    85,  1655,
    1656,   150,   144,   150,   150,  1757,   147,   147,  1769,   147,
     147,   150,  1878,   957,   989,   150,  1882,  1883,  1873,   147,
     995,   996,   147,   147,   151,  1952,   144,  1764,  1089,  1873,
     145,   147,  1952,  1238,   170,   147,  1757,   144,   144,   150,
    1757,   170,   150,  1909,   147,   989,   150,   147,   327,   147,
     147,   995,   996,  1805,  1873,   147,  1873,   147,   149,  1873,
    1873,  1747,   144,  1824,  1930,  1826,  1757,   152,  1934,  1821,
       1,  1757,  1655,     4,   170,   170,  1837,   170,  1839,  1840,
     152,   170,   534,   147,  1805,  1822,  1030,  1874,  1805,  1033,
     147,  1747,  1485,  1959,   147,   147,   151,   170,   148,   170,
    1821,  1757,    98,   152,  1821,  1857,   145,   151,    70,   145,
     144,   315,  1873,  1874,  1805,  1874,   146,   146,   150,  1805,
    1907,  1757,   170,  1884,    55,   170,   103,   103,   161,  1757,
    1821,   147,   152,  1894,   161,  1821,  1857,   341,   342,    70,
    1857,   188,   147,   144,   144,   147,  1907,    78,  1907,  1805,
     145,   170,    70,   147,  1133,  1134,   147,   170,   170,  1920,
      91,  1922,  1556,    94,  1747,  1821,  1857,    98,   329,  1805,
    1187,  1857,  1147,   587,   642,   644,  1937,  1805,   643,   645,
    1116,  1942,   646,  1105,  1922,  1821,  1130,  1536,  1854,  1133,
    1134,  1666,  1870,  1821,  1757,  1956,  1814,  1917,  1773,  1960,
    1916,  1857,   406,  1147,   135,  1904,  1528,  1528,  1822,  1970,
     141,  1883,   143,  1821,  1158,   146,   147,  1137,   497,   498,
      45,  1857,   245,  1483,  1934,  1735,   157,   158,   432,  1857,
    1174,  1798,  1296,  1497,  1030,  1130,  1180,   769,   850,   567,
    1426,     0,  1825,    -1,   175,   176,   177,     4,     5,     6,
       7,     8,     9,    10,    11,  1550,   187,   188,   972,    -1,
     191,   648,   648,    -1,   648,    -1,    67,  1242,    -1,    -1,
      -1,    -1,    -1,    -1,  1479,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,   214,   215,    -1,    -1,  1348,    -1,    -1,
      -1,   570,    -1,    -1,    -1,    -1,   575,    98,  1242,   578,
     231,   102,   103,   104,   105,   106,   107,   108,    -1,   240,
      -1,    -1,    -1,    -1,    -1,  1259,    -1,    -1,   597,    -1,
     367,    -1,   253,   370,   125,    -1,  1301,    -1,    -1,    -1,
    1929,    -1,    -1,    -1,    -1,    -1,    -1,   384,    -1,    -1,
      -1,   388,    -1,    -1,   145,   146,    -1,    -1,    -1,  1948,
      -1,   803,   283,    -1,    -1,   156,   157,  1301,   289,   290,
     291,    -1,    -1,    -1,    -1,    -1,   297,    -1,   647,    -1,
      -1,  1350,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,   107,   108,    -1,  1328,   432,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   327,    -1,    -1,   125,
      -1,    -1,    -1,    -1,    -1,    -1,  1350,    -1,    -1,    -1,
     341,    -1,  1617,  1618,    -1,    -1,    -1,    -1,    -1,   145,
     146,    -1,   803,    -1,   628,    -1,   152,   358,    -1,    -1,
     156,   157,    -1,    -1,    -1,  1379,   367,   368,   369,    -1,
    1384,    -1,    -1,    -1,    73,  1389,  1390,  1391,    -1,  1510,
      -1,  1512,    -1,   384,    98,    -1,   503,   388,   102,   103,
     104,   105,   106,   107,   108,   917,    -1,    -1,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
      -1,   412,    -1,  1544,    98,    -1,   938,    -1,   102,   103,
     104,   105,   106,   107,   108,    -1,    -1,   428,    -1,    -1,
      -1,   432,   146,     9,    -1,   436,    -1,   438,  1483,   556,
      -1,    -1,    -1,    -1,  1489,    -1,  1491,   146,    -1,    -1,
      98,  1726,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   145,   146,   985,   986,   466,   917,    -1,    -1,  1483,
      -1,   170,    -1,    -1,   475,  1489,    -1,  1491,    -1,   596,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,    -1,   495,    -1,   497,   498,   146,    -1,
     501,    -1,   503,    -1,    -1,    94,    -1,    -1,   156,    -1,
      -1,    -1,    -1,    -1,  1645,    -1,   105,    -1,   107,  1794,
     109,    -1,    98,  1798,   100,   101,   102,   103,   104,   105,
     106,   107,   108,    -1,   985,   986,  1667,    -1,    -1,    -1,
     541,    -1,   659,   660,    -1,    -1,    -1,    -1,    -1,  1824,
    1564,   552,  1566,   554,    -1,   556,    -1,    -1,   147,    -1,
     149,   150,    -1,    -1,    -1,    -1,   683,    -1,    -1,   570,
     146,   572,   573,    -1,   575,    -1,    -1,   578,    -1,   696,
     581,   582,   583,    -1,   858,    -1,    -1,  1632,    -1,   863,
    1635,    -1,    -1,    -1,    -1,   596,    -1,    -1,  1873,  1874,
     874,    -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1741,    -1,    -1,    -1,  1745,    -1,    -1,    -1,  1632,   736,
      -1,  1635,    -1,    -1,    -1,  1756,    -1,    -1,    -1,    -1,
      -1,    -1,  1907,    -1,    -1,    -1,    -1,    -1,  1769,  1684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   648,   649,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   657,    -1,   659,   660,
      -1,   662,    -1,    -1,   253,    -1,   255,   256,    -1,   670,
    1684,    -1,    -1,   674,    -1,    -1,    12,    -1,    -1,    -1,
      -1,    -1,   683,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1735,    -1,   693,    -1,   283,   696,    -1,    -1,    -1,    -1,
     289,    -1,    -1,   704,    -1,    -1,  1837,    -1,  1839,  1840,
      -1,  1233,  1234,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1735,    -1,    -1,    -1,  1247,  1248,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   736,    -1,    -1,   739,    -1,
     741,    -1,    -1,    -1,    -1,   862,    -1,    83,    -1,    -1,
      -1,    -1,    -1,  1884,    -1,    -1,    -1,   758,   759,  1281,
    1282,    -1,    98,  1894,   100,   101,   102,   103,   104,   105,
     106,   107,   108,    -1,   775,    -1,   777,    -1,    -1,  1824,
     369,    -1,  1233,  1234,    -1,    -1,   375,    -1,   377,  1920,
      -1,  1922,    -1,    -1,    -1,   796,  1247,  1248,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1937,    -1,    -1,    -1,
    1824,  1942,  1826,  1087,    -1,    -1,   933,    -1,    -1,   936,
      -1,    -1,    -1,   412,    -1,  1956,    -1,    -1,  1873,  1874,
    1281,  1282,    -1,   834,    -1,    -1,    -1,    67,    -1,    -1,
     841,  1190,    -1,    -1,    -1,   846,   847,    -1,    -1,   438,
      -1,   440,   441,    -1,    -1,    -1,    -1,    -1,    -1,  1873,
    1874,   862,  1907,    -1,    -1,    -1,    -1,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   475,    -1,    -1,    -1,
    1164,  1165,  1166,  1907,    -1,   125,    -1,  1171,  1172,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   495,    -1,    -1,    -1,
      -1,   500,    -1,   502,    -1,   145,   146,    -1,    -1,   149,
     921,    17,    -1,    -1,    -1,    78,   156,   157,    -1,    -1,
      -1,  1205,   933,   522,   935,   524,   525,    -1,    -1,    -1,
      -1,    -1,    -1,  1465,  1466,   946,  1960,    -1,    -1,    -1,
      -1,    -1,    -1,   542,    -1,    -1,  1970,    -1,    -1,    -1,
      56,    57,    58,    59,    -1,   554,    -1,    -1,  1317,    -1,
      -1,    -1,    12,    13,    14,    15,    16,  1326,    -1,  1501,
      -1,  1330,    -1,  1332,    -1,    -1,    -1,    -1,   989,    -1,
     143,    -1,    -1,    -1,    -1,   996,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,   157,   158,   102,   103,   104,   105,
     106,   107,   108,   109,  1465,  1466,    -1,    -1,    -1,    -1,
      -1,    -1,   175,    -1,    -1,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,  1039,    -1,
    1157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1571,
     146,    -1,    -1,   149,    -1,  1577,    -1,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,    -1,
      -1,  1593,  1594,   662,    -1,   664,   665,    -1,  1079,    12,
      13,    14,    15,    16,    -1,   125,  1087,   240,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   686,   687,    -1,
      -1,    -1,    -1,    -1,   693,   145,   146,    -1,    -1,   149,
      -1,    -1,    -1,    -1,    -1,  1116,   156,   157,    -1,    -1,
    1571,    -1,  1471,    -1,    -1,    -1,  1577,    -1,    -1,    -1,
      -1,    98,  1133,  1134,    67,   102,   103,   104,   105,   106,
     107,   108,  1593,  1594,   297,    -1,  1147,    -1,    -1,    -1,
      -1,    -1,  1153,    -1,    -1,    -1,  1157,    -1,   125,    -1,
    1161,    -1,  1511,    -1,  1513,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,    -1,    -1,   145,   146,
      -1,    -1,   149,    -1,    -1,    -1,    -1,  1536,   341,   156,
     157,    -1,   125,    -1,    -1,    -1,    -1,    -1,    60,    61,
      62,    63,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   145,   146,   367,    -1,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    67,    12,    13,    14,    15,
      16,    -1,    -1,    -1,    -1,    -1,    98,   826,   100,   101,
     102,   103,   104,   105,   106,   107,   108,    -1,   837,    -1,
      -1,  1252,  1253,    -1,    -1,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,  1541,    -1,    -1,
       1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,   432,
      -1,    67,    -1,    -1,   146,    -1,  1403,   149,    -1,    -1,
    1291,  1292,  1293,  1294,  1295,  1296,    -1,    -1,  1647,    -1,
    1301,  1302,  1651,   145,   146,   167,    -1,    -1,    -1,  1658,
      -1,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,   107,   108,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   921,    -1,    -1,    -1,    -1,    -1,    -1,   125,
      -1,    -1,    -1,    -1,   497,   498,   935,    78,    -1,  1350,
     503,    -1,    -1,    -1,    -1,    -1,    -1,   946,    -1,   145,
     146,    -1,    -1,    94,    -1,    -1,   955,    98,    -1,  1891,
     156,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,    -1,    -1,    -1,
      -1,    -1,  1403,   556,   135,  1754,  1755,   996,  1757,    -1,
     141,    -1,   143,    -1,    -1,    -1,   147,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1426,   157,   158,   159,    -1,
      -1,    -1,    -1,    -1,   146,    67,    -1,    -1,    -1,    -1,
    1891,    -1,    -1,   596,   175,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   187,   188,   170,   142,
     191,    -1,    -1,    -1,  1813,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,    -1,  1479,    -1,
      -1,    -1,  1483,    -1,  1485,    -1,   169,    -1,  1489,    -1,
    1491,    -1,    -1,   125,    -1,    -1,    -1,  1771,    -1,    12,
      13,    14,    15,    16,   657,    -1,   237,   660,    -1,   240,
    1511,    -1,  1513,   145,   146,    -1,  1865,    -1,  1867,    -1,
      -1,    -1,   253,    -1,   156,   157,    -1,    -1,    -1,    -1,
     683,    -1,    -1,    -1,    -1,  1536,    -1,    -1,  1887,   270,
      -1,    -1,    -1,    -1,  1133,    -1,  1547,   278,    -1,  1550,
      -1,   704,   283,    -1,    67,  1556,    -1,    -1,   289,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   297,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1923,  1924,  1925,    -1,    -1,    -1,
      -1,    -1,    -1,   736,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   327,  1598,  1599,    -1,
      -1,    -1,    -1,    -1,    -1,   758,   759,    -1,    -1,    -1,
     341,    -1,   125,    -1,    -1,    -1,  1617,  1618,    -1,    -1,
    1209,    -1,   775,    -1,    -1,    -1,  1215,    -1,    -1,    -1,
      -1,  1632,   145,   146,  1635,    -1,   367,    67,   369,   370,
      -1,    -1,    -1,   156,   157,    -1,    -1,  1764,    -1,    -1,
      -1,    -1,    -1,   384,  1655,  1656,    98,   388,   100,   101,
     102,   103,   104,   105,   106,   107,   108,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,    -1,
      -1,    -1,    -1,  1684,    -1,    -1,    -1,    -1,   841,    -1,
      -1,    -1,    -1,    -1,   847,   125,    -1,   428,    -1,    -1,
      -1,   432,    -1,    -1,   146,  1822,    -1,   438,    -1,   862,
      -1,    -1,    -1,  1302,    -1,   145,   146,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1726,   156,   157,   170,    -1,
      -1,    -1,    -1,    -1,  1735,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    -1,    -1,    98,  1747,   100,   101,   102,
     103,   104,   105,   106,   107,   108,  1757,    -1,    -1,    -1,
      -1,  1350,    -1,  1764,    -1,    -1,   497,   498,    -1,    98,
      -1,   502,   503,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,   113,    -1,   115,    -1,  1789,    -1,
      -1,    67,    -1,   146,    -1,   148,    -1,  1798,    -1,    -1,
     153,    -1,    -1,   534,  1805,    -1,    -1,    -1,   539,    -1,
      -1,   542,   543,    -1,   545,    -1,    -1,   146,    -1,    -1,
    1821,  1822,    98,  1824,  1825,   556,   102,   103,   104,   105,
     106,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,   570,
      -1,   572,   573,    -1,   575,    -1,    -1,   578,    -1,   125,
     581,   582,   583,    -1,    67,    -1,  1857,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   596,    -1,    -1,    -1,   145,
     146,    -1,  1873,  1874,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   157,    67,    -1,    -1,    98,  1039,   100,   101,   102,
     103,   104,   105,   106,   107,   108,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,    -1,    -1,  1907,    -1,    -1,    -1,
      -1,    -1,   125,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,    -1,    -1,   657,   658,   659,   660,
      -1,   662,   145,   146,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,   156,   157,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   683,    55,    -1,   686,    -1,    -1,    -1,   690,
     145,   146,   693,    -1,    -1,   696,    -1,   698,    -1,    -1,
      -1,   156,   157,   704,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,    -1,    -1,    98,    -1,    -1,    -1,
    1153,    -1,    -1,    -1,  1157,   736,    -1,    -1,    -1,    -1,
     125,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,    -1,    -1,    -1,    -1,    -1,   758,   759,    -1,
     145,   146,    -1,   135,   149,    -1,    -1,    -1,   125,   141,
      -1,   156,   157,    -1,   775,    -1,  1635,    -1,    -1,    -1,
      -1,    -1,  1205,    -1,   169,    -1,   158,    -1,   145,   146,
      -1,    -1,    -1,    -1,    -1,   152,  1655,    -1,    -1,   156,
     157,    -1,   803,    98,   176,   100,   101,   102,   103,   104,
     105,   106,   107,   108,    -1,   187,   188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,  1252,
    1253,    -1,    -1,   834,    -1,    -1,   837,    -1,    -1,    -1,
     841,    -1,    -1,   215,    -1,   846,   847,    -1,    -1,    -1,
      -1,   146,    -1,    -1,    -1,    -1,    -1,    -1,   153,   231,
      -1,   862,    -1,    -1,   236,   237,    -1,  1726,   240,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1747,    -1,
     262,    -1,    -1,   265,    -1,   267,    -1,   125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,   278,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   917,   145,   146,   291,
      -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,
      -1,    -1,   933,    -1,   935,   936,    -1,   938,    -1,    -1,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,    -1,   146,    -1,    98,   149,   100,   101,   102,
     103,   104,   105,   106,   107,   108,  1825,    98,   125,   100,
     101,   102,   103,   104,   105,   106,   107,   108,    -1,    -1,
    1403,    -1,   125,    -1,   985,   986,    -1,    -1,   145,   146,
      -1,    -1,    -1,    -1,    -1,   367,    -1,    -1,   370,   156,
     157,    -1,   145,   146,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   384,   156,   157,   146,   388,    -1,    -1,    96,
      -1,    98,   153,   100,   101,   102,   103,   104,   105,   106,
     107,   108,    -1,    -1,    -1,    -1,    98,    -1,  1039,    -1,
     102,   103,   104,   105,   106,   107,   108,   109,  1907,    -1,
      -1,   113,    -1,   115,    -1,    -1,   428,    -1,    -1,    -1,
     432,    -1,  1485,    -1,    -1,    -1,    -1,    -1,   145,    -1,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,  1079,    -1,
      -1,    -1,    -1,    -1,   146,    45,  1087,   149,    -1,    49,
      -1,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,
      -1,    -1,    -1,    -1,    -1,  1116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1547,    -1,    -1,    -1,    -1,    -1,
      -1,   503,  1133,  1134,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,  1153,   113,   114,   115,  1157,   117,   118,    -1,
    1161,    -1,   534,    -1,    -1,   125,    -1,   539,    -1,    -1,
      -1,   543,    -1,   545,    -1,  1598,  1599,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   556,   145,    -1,    -1,   148,   149,
      -1,    -1,    -1,    -1,    -1,   155,   156,   157,   158,   159,
     160,   161,    -1,    -1,  1205,    -1,    -1,    -1,  1209,   169,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1221,    -1,    -1,    -1,   596,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1233,  1234,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,    -1,  1247,  1248,    -1,    98,
      -1,  1252,  1253,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,   113,    98,   115,   100,   101,   102,
     103,   104,   105,   106,   107,   108,    -1,   649,    -1,    -1,
    1281,  1282,   146,    -1,    -1,   149,    -1,   659,   660,    -1,
    1291,  1292,  1293,  1294,  1295,  1296,    -1,   146,    -1,    -1,
     149,  1302,   674,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,   683,    -1,   146,    -1,   148,   688,    -1,    -1,    -1,
      -1,    -1,    -1,    98,   696,    -1,    -1,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,   113,    -1,
     115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1350,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    -1,    -1,    -1,   736,    -1,  1789,    -1,    -1,   741,
      -1,   146,    -1,     1,   149,    98,     4,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   758,   759,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,    -1,
      -1,    98,  1403,   775,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   145,   146,    -1,  1426,    -1,    55,    -1,    -1,
      -1,   803,    -1,    -1,    -1,    -1,   146,    -1,   148,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
      78,   145,   146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   834,    -1,  1465,  1466,    -1,    -1,    -1,   841,
      98,    -1,    -1,    98,   846,   100,   101,   102,   103,   104,
     105,   106,   107,   108,  1485,    -1,    -1,    -1,    -1,    -1,
     862,   188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1501,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
    1511,    -1,  1513,   141,    -1,   143,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,    -1,    -1,    -1,  1536,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   917,  1547,   175,    -1,  1550,
      -1,    -1,    -1,    -1,    -1,  1556,    -1,    -1,    -1,   187,
      -1,   933,    -1,    -1,   936,   262,   938,    -1,    45,    -1,
    1571,   943,    49,    -1,    51,    -1,  1577,    -1,    -1,    -1,
      -1,   278,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,  1593,  1594,    -1,    -1,    -1,  1598,  1599,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   237,
      -1,    -1,   240,   985,   986,    -1,    -1,   245,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,    -1,   113,   114,   115,    -1,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
     278,    -1,    -1,    -1,  1655,  1656,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1039,   145,   297,
     367,   148,   149,   370,    -1,    -1,   153,    -1,   155,   156,
     157,   158,   159,   160,   161,    -1,    -1,   384,    -1,    -1,
      -1,   388,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   327,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1079,    -1,    -1,
      -1,    -1,    -1,   341,    -1,  1087,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,  1116,    -1,  1747,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1757,    -1,    -1,    -1,
      -1,    -1,  1134,  1764,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1157,    -1,    -1,  1789,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
     428,    -1,    -1,    -1,  1805,    -1,   503,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1821,  1822,    -1,    -1,  1825,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1205,    -1,    -1,    -1,   534,    -1,    -1,
      98,    -1,  1214,    -1,    -1,    -1,   543,    -1,   545,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1857,    -1,    -1,   556,
      -1,  1233,  1234,    -1,    -1,    -1,    -1,    -1,    -1,   497,
     498,    -1,    -1,    -1,    -1,  1247,  1248,    -1,    -1,    -1,
    1252,  1253,    -1,   141,    -1,    -1,    -1,    -1,    -1,    -1,
    1891,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   596,
     158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1281,
    1282,   539,    -1,    -1,    -1,   543,    -1,   545,    -1,  1291,
    1292,  1293,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   570,    -1,   572,   573,    -1,   575,    -1,    -1,
     578,    -1,    -1,   581,   582,   583,    -1,    -1,    -1,    -1,
      -1,    -1,   659,   660,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   683,    45,    -1,    -1,
      -1,    49,    -1,    51,    -1,    -1,    -1,    -1,    -1,   696,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     278,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   657,
      -1,  1403,    -1,    -1,    -1,    -1,    -1,    95,    96,   736,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,  1426,   113,   114,   115,    -1,   117,
     118,   758,   759,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   704,    -1,   775,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,   146,    -1,
     148,   149,    -1,  1465,  1466,   153,    -1,   155,   156,   157,
     158,   159,   160,   161,    -1,    -1,   803,    -1,    -1,   367,
      -1,    -1,   370,    -1,    -1,    -1,    -1,    -1,    -1,  1491,
      -1,    -1,    -1,    -1,    -1,    -1,   384,    -1,    -1,  1501,
     388,    -1,    -1,    -1,    -1,    -1,    -1,   834,    -1,  1511,
      -1,  1513,    -1,    -1,   841,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1536,   862,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   432,  1547,    -1,    -1,  1550,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1571,
      -1,    -1,    -1,    -1,    -1,  1577,   834,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,   847,
     917,  1593,  1594,    -1,    -1,    -1,  1598,  1599,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   933,    -1,    -1,   936,
      -1,   938,    -1,    -1,    -1,   503,  1618,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   534,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1656,   543,    -1,   545,   985,   986,
       1,    -1,    -1,     4,    -1,    -1,    45,    -1,   556,    -1,
      49,    -1,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   596,    -1,
      -1,    -1,  1039,    -1,    55,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,    -1,   113,   114,   115,    78,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1757,    -1,    98,    -1,    -1,
    1087,    -1,  1764,    -1,    -1,    -1,   145,    -1,    -1,   148,
     149,   659,   660,    -1,    -1,    -1,   155,   156,   157,   158,
     159,   160,   161,    -1,    -1,    -1,    -1,  1789,    -1,  1116,
      -1,    -1,    -1,    -1,   135,   683,    -1,    -1,    -1,    -1,
     141,    -1,   143,  1805,    -1,    -1,    -1,  1134,   696,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,    -1,  1821,
    1822,  1079,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1087,
    1157,    -1,    -1,    -1,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,   736,    -1,
      -1,    -1,    -1,    -1,    -1,  1857,    -1,    -1,  1116,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     758,   759,  1874,    -1,    -1,    -1,  1134,    -1,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   775,    -1,  1891,
      -1,    -1,    -1,    -1,    -1,  1153,   237,    -1,    -1,   240,
      -1,    -1,    -1,  1161,   245,    -1,  1233,  1234,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   803,    -1,    -1,    -1,    -1,
    1247,  1248,    -1,    -1,    -1,  1252,  1253,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   278,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   834,    -1,    -1,    -1,
      -1,    -1,    -1,   841,  1281,  1282,   297,    -1,    -1,    -1,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   327,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     341,    45,    -1,    -1,    -1,    49,    -1,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,   917,
      -1,    -1,    -1,  1291,  1292,  1293,  1294,  1295,  1296,    -1,
      -1,    -1,    -1,    -1,    -1,   933,    -1,    -1,   936,    -1,
     938,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,    -1,   113,
     114,   115,    -1,   117,   118,    -1,  1403,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    -1,    -1,    -1,   428,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,   985,   986,     4,
      -1,   145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,
      -1,   155,   156,   157,   158,   159,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1465,  1466,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,  1039,    -1,    -1,    -1,    -1,   497,   498,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1426,    -1,
      -1,    -1,    -1,    78,  1501,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1511,    -1,  1513,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,   539,  1087,
      -1,    -1,   543,    -1,   545,    -1,    -1,    -1,    -1,  1536,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1547,    -1,    -1,    -1,    -1,    -1,    -1,  1485,  1116,   570,
     135,   572,   573,    -1,   575,    -1,   141,   578,   143,    -1,
     581,   582,   583,    -1,  1571,    -1,  1134,    -1,    -1,    -1,
    1577,    -1,   157,  1511,    -1,  1513,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1593,  1594,    -1,  1157,
     175,  1598,  1599,    -1,    -1,    -1,    -1,    -1,  1536,    -1,
      -1,    -1,   187,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1550,    -1,    -1,    -1,    -1,    -1,  1556,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   657,  1205,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1656,
      -1,    -1,   237,    -1,    -1,   240,    -1,    -1,    -1,    -1,
     245,    -1,    -1,    -1,    -1,  1233,  1234,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1247,
    1248,    -1,    -1,   704,  1252,  1253,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   278,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      52,    53,   297,  1281,  1282,    -1,    -1,    -1,  1656,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   327,    -1,    86,    -1,    -1,    -1,    -1,    -1,
    1757,    12,    13,    14,    15,    16,   341,  1764,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,  1789,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    -1,    -1,    -1,   136,    -1,    -1,   139,  1805,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
     152,    -1,    -1,   834,  1821,  1822,    -1,    -1,    -1,  1757,
      -1,    -1,    -1,    -1,    -1,   846,   847,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1403,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,    -1,    -1,
    1857,    -1,    -1,     0,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,   205,    -1,    -1,    -1,  1805,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1821,  1891,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1465,  1466,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   250,    -1,
      -1,    -1,   497,   498,    -1,    -1,    -1,    -1,   260,  1857,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,   271,
      -1,    -1,    -1,  1501,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1511,    -1,  1513,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   539,    -1,   298,    -1,   543,    -1,
     545,    -1,   304,   305,    -1,    -1,    -1,   309,  1536,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1547,
      -1,    -1,    -1,    -1,   131,   570,    -1,   572,   573,    -1,
     575,    -1,    -1,   578,    -1,    -1,   581,   582,   583,    -1,
      -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,  1577,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,  1593,  1594,    -1,    -1,    -1,
    1598,  1599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   393,    -1,    -1,    -1,   122,   398,    -1,  1079,   401,
      -1,    -1,    -1,    -1,    -1,    -1,  1087,    -1,    -1,    -1,
      -1,    -1,   657,    -1,    -1,    -1,    -1,   224,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1656,    -1,
      -1,    -1,   239,   435,    -1,  1116,    -1,    -1,    -1,    -1,
      -1,    -1,   249,    -1,    -1,   447,   448,    -1,    -1,    -1,
      -1,    -1,   259,  1134,    -1,    -1,    -1,    -1,    -1,   704,
      -1,    -1,    -1,    -1,    -1,    -1,   273,   274,    -1,    -1,
      -1,    -1,  1153,   280,   281,    -1,    -1,    -1,    -1,    -1,
    1161,    -1,    -1,    -1,    12,    13,    14,    15,    16,   296,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    47,
      48,   328,    50,    -1,    -1,    -1,    -1,    -1,    -1,  1757,
      -1,    -1,    -1,    -1,    -1,    -1,  1764,    -1,    -1,    67,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   366,
     287,  1789,    -1,    -1,    -1,   567,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,    -1,    -1,  1805,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   834,
      -1,    -1,    -1,  1821,  1822,    -1,    -1,   404,    -1,    -1,
      -1,   846,   847,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1291,  1292,  1293,  1294,  1295,  1296,    -1,    -1,    -1,   426,
     148,    -1,    -1,   430,    -1,    -1,    -1,    -1,    -1,  1857,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   449,    -1,    -1,    -1,   453,   454,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   462,   463,   464,   465,    -1,
      -1,    -1,    -1,  1891,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   481,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     702,    -1,    -1,    -1,   431,    -1,   433,    -1,    -1,    -1,
     517,    -1,    -1,    -1,    -1,   442,   443,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   548,    -1,    -1,    -1,  1426,    -1,    -1,   555,    -1,
      -1,   753,    -1,   560,    -1,    -1,    -1,    -1,   760,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,   584,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  1485,    46,    47,    48,    -1,    50,
      -1,    -1,    -1,   540,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
    1511,   833,  1513,    -1,  1079,    -1,    -1,    -1,    -1,    -1,
      -1,   648,  1087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1536,    -1,    -1,    -1,   100,
     101,    -1,    -1,   670,   671,    -1,    -1,    -1,    -1,  1550,
      -1,  1116,    -1,    -1,    -1,  1556,    -1,    -1,    45,    -1,
      -1,    -1,    49,    -1,    51,    -1,    -1,    -1,    -1,  1134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    -1,    -1,    -1,    -1,    -1,   148,  1153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1161,    -1,    -1,   726,
      -1,    -1,    -1,   730,    -1,    -1,    -1,    -1,    95,    96,
      -1,    98,   739,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,    -1,   113,   114,   115,    -1,
     117,   118,    -1,    -1,    -1,    -1,   763,    -1,   125,   961,
      -1,    -1,    -1,    -1,   966,    -1,    -1,   774,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1656,    -1,    -1,   145,   146,
      -1,   148,   149,    -1,    -1,    -1,   153,    -1,   155,   156,
     157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,
      -1,   818,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   844,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   852,  1291,  1292,  1293,  1294,
    1295,  1296,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1757,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1092,  1093,  1094,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     827,   828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   838,   839,   840,    -1,    -1,   843,    -1,    -1,    -1,
    1122,    -1,    -1,    -1,  1805,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,    -1,
    1821,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   978,    -1,    -1,    -1,   982,  1857,    -1,    -1,    -1,
      -1,  1426,   989,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,   999,    -1,    -1,    -1,    -1,    -1,    -1,  1006,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,  1015,    -1,
    1017,    -1,    70,    -1,    -1,    -1,  1218,    -1,    -1,  1026,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1034,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1485,    -1,    -1,  1050,    -1,    -1,    -1,  1054,   975,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,  1068,    -1,    -1,  1071,    -1,  1511,    -1,  1513,    -1,
     128,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1012,    -1,    -1,    -1,    -1,
      -1,  1536,    -1,    -1,  1021,  1022,  1023,  1024,    -1,    -1,
      -1,    -1,   160,  1305,    -1,  1550,  1308,    -1,    44,    -1,
      -1,  1556,    -1,  1040,    -1,    -1,    -1,    -1,    -1,   177,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1061,    -1,  1063,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,    -1,    -1,   315,   316,
      -1,    -1,    -1,    -1,    -1,    -1,   214,    -1,   325,   326,
     218,    -1,    -1,   221,   222,    -1,    -1,   225,    -1,    -1,
     228,   229,    -1,    -1,   341,   342,    -1,    -1,    -1,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1116,
      -1,    -1,   128,    -1,   130,    -1,  1203,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1656,    -1,    -1,    -1,  1142,    -1,    -1,    -1,    -1,
    1227,  1148,    -1,  1150,  1151,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   290,    -1,    -1,   293,    -1,    -1,    -1,   406,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   432,  1273,    -1,  1195,    -1,
    1277,    -1,    -1,    -1,    -1,    -1,    -1,  1204,    -1,  1206,
      -1,  1208,    -1,  1210,    -1,   221,   222,    -1,  1215,   225,
      -1,    -1,   228,   229,    -1,    -1,  1498,    -1,    -1,    -1,
    1307,    -1,    -1,    -1,    -1,    -1,    -1,   365,  1235,  1236,
      -1,    -1,  1757,  1320,  1321,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   380,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1262,  1263,    -1,    -1,    -1,
      -1,    -1,    -1,  1270,    -1,    -1,    -1,    -1,    -1,    -1,
    1357,    -1,    -1,  1360,    -1,    -1,    -1,    -1,    -1,    -1,
    1805,    -1,    -1,   421,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1300,    -1,    -1,  1821,    -1,    -1,    -1,
      -1,    -1,    -1,  1585,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1398,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1409,    -1,    -1,    -1,    -1,    -1,   466,    -1,
      -1,    -1,  1857,  1340,    -1,    -1,    -1,    -1,    -1,    -1,
     478,    -1,    -1,    -1,    -1,    -1,    -1,  1434,    -1,   365,
      -1,  1438,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   380,  1452,  1453,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   628,   629,   630,   631,   632,   633,   634,   635,   636,
     637,   638,   639,   640,   641,   642,   643,   644,   645,   646,
    1407,  1683,    -1,    -1,    -1,   421,    -1,    -1,  1415,    -1,
    1417,    -1,    -1,    -1,   552,    -1,    -1,    -1,    -1,    -1,
    1702,    45,  1429,  1430,    -1,    49,    -1,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1444,  1445,    -1,
    1447,    -1,    -1,    -1,    68,    -1,    -1,    -1,  1455,    -1,
      -1,    -1,    -1,    -1,  1461,    -1,    -1,    -1,    -1,    -1,
    1467,  1468,   478,    -1,  1551,  1552,    -1,    -1,    -1,    -1,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,    -1,   113,
     114,   115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   652,   653,    -1,    -1,    -1,    -1,
      -1,   145,   146,    -1,   148,   149,    -1,    -1,    -1,    -1,
      -1,   155,   156,   157,   158,   159,   160,   161,    -1,    -1,
     678,   679,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   691,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1578,  1579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1588,    -1,   721,    -1,  1672,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,    -1,    -1,
      -1,  1608,  1609,    -1,   742,    -1,    -1,   745,   746,    -1,
     748,   858,   750,   751,    -1,    -1,   863,  1704,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1712,    -1,   874,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   652,   653,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   787,
      -1,    -1,  1739,   791,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   678,   679,    -1,    -1,    -1,    -1,   915,    -1,
      -1,    -1,    -1,  1760,    -1,   691,  1763,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    45,  1694,    -1,  1696,
      49,    -1,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1707,  1708,    -1,    -1,    -1,   721,    -1,  1714,    -1,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   742,    -1,    -1,   745,
     746,    -1,   748,    -1,   750,   751,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,    -1,   113,   114,   115,    -1,   117,   118,
      -1,  1848,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,   787,    -1,    -1,    -1,   791,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   145,    -1,    -1,   148,
     149,    -1,    -1,    -1,   153,    -1,   155,   156,   157,   158,
     159,   160,   161,  1810,    -1,    -1,   944,    -1,    -1,    -1,
    1817,    -1,    -1,    -1,    -1,    -1,  1823,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   995,    -1,    -1,
      -1,    -1,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1895,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1903,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1044,   187,   188,  1047,
      -1,  1918,    -1,    -1,    -1,    -1,    -1,  1164,  1165,  1166,
      -1,    -1,    -1,    -1,  1171,  1172,    -1,    -1,   944,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   218,    -1,
      -1,    -1,    -1,    -1,    -1,   225,    -1,  1194,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,     9,    -1,   995,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,   293,    -1,    47,    48,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,  1044,    -1,
      -1,  1047,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1196,    -1,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,   100,   101,
      -1,    -1,    -1,  1211,    -1,    -1,    -1,    -1,   358,    -1,
      -1,  1219,  1220,    -1,    -1,    -1,    -1,   367,   368,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
    1238,    -1,    -1,  1241,    -1,  1243,  1244,    -1,   388,    -1,
      -1,    -1,   144,    -1,    -1,    -1,   148,   149,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   421,   422,    -1,   424,   425,  1284,    -1,    -1,    -1,
      -1,    -1,   432,    -1,    -1,    -1,   436,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1196,    -1,    -1,    -1,    -1,    -1,    -1,   467,    -1,    -1,
      -1,   471,    -1,    -1,    -1,  1211,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1219,  1220,    -1,    -1,    -1,    -1,  1456,
    1457,    -1,    -1,  1351,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1238,   503,    -1,  1241,    -1,  1243,  1244,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    -1,    -1,    49,    -1,    51,    -1,    -1,   175,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1284,    -1,
      -1,   188,    68,   553,    -1,    -1,   556,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1541,  1433,    -1,    -1,    -1,    95,
      96,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   596,   113,   114,   115,
      -1,   117,   118,    -1,    -1,  1463,    -1,    -1,    -1,   125,
      -1,    -1,    -1,    -1,    -1,  1351,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,
     146,  1489,   148,   149,    -1,    -1,    -1,  1495,    -1,   155,
     156,   157,   158,   159,   160,   161,    -1,    -1,   648,   649,
      -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   659,
     660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     670,    -1,    -1,    -1,   674,    -1,    -1,    -1,    -1,    -1,
      -1,   681,    -1,   683,    -1,    -1,    -1,    -1,  1546,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1433,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   721,   722,    -1,   724,   725,    -1,  1463,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   736,    -1,    -1,   739,
      -1,   741,   742,    -1,    -1,    -1,    -1,    -1,   748,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   758,   759,
      -1,    -1,    -1,    -1,    78,    -1,  1624,  1625,    -1,    -1,
    1737,    -1,    -1,    -1,  1632,   775,    -1,    -1,  1636,   779,
      94,    -1,    -1,   783,    -1,    -1,    -1,   787,   788,    -1,
      -1,   791,   792,    -1,    -1,    -1,    -1,    -1,    -1,   799,
      -1,    -1,    -1,    -1,  1771,    -1,    -1,    -1,    -1,    -1,
    1546,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   456,
      -1,    -1,   459,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      -1,    -1,    -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   841,   842,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   175,   862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1728,    -1,    -1,    -1,   188,    -1,    -1,   191,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1853,  1854,  1624,  1625,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1636,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   546,
      -1,    -1,    -1,  1880,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   921,    -1,    -1,    -1,    -1,   240,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1794,    -1,  1905,   253,
     577,    -1,    -1,    -1,    -1,    -1,   946,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   595,   596,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,
      -1,   608,   609,    -1,    -1,   289,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   297,    -1,  1952,   623,    -1,    -1,   989,
     990,    -1,  1728,    -1,    -1,    -1,   996,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   327,    -1,  1873,    -1,    -1,    -1,    -1,
     657,    -1,    -1,   660,    -1,    -1,    -1,   341,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   673,    -1,    -1,  1039,
      -1,    -1,    -1,    -1,  1044,  1045,    -1,  1047,  1048,    -1,
      -1,    -1,    -1,   367,    -1,   369,    -1,    -1,  1794,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   716,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   733,    -1,    -1,   736,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   432,    -1,
      -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,
      -1,   768,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1157,    -1,    -1,
      -1,    -1,    -1,   800,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   497,   498,    -1,    -1,    -1,    -1,   503,
      -1,    -1,    -1,    -1,    -1,    -1,  1196,  1197,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,
     847,  1211,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   862,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   870,    -1,    -1,    -1,    -1,  1238,  1239,
     877,  1241,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1252,  1253,    -1,    -1,   570,   894,   572,   573,
      -1,   575,    -1,    -1,   578,    -1,    -1,   581,   582,   583,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   935,   936,
      -1,    -1,    -1,    -1,    -1,    -1,   943,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,    49,
      -1,    51,    -1,    -1,    -1,    -1,    -1,   964,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   973,    -1,    68,    -1,
      -1,    -1,    -1,   657,    -1,    -1,   660,    -1,   662,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,   683,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,    -1,   113,   114,   115,    -1,   117,   118,    -1,
     704,  1028,    -1,  1030,    -1,   125,    -1,    -1,    -1,    -1,
      -1,  1038,    -1,  1403,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   145,   146,    -1,   148,   149,
      -1,    -1,   736,    -1,    -1,   155,   156,   157,   158,   159,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   758,   759,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   775,    -1,    -1,    -1,    -1,    -1,  1104,  1105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,  1479,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,  1495,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    1157,    46,    47,    48,    -1,    50,    -1,   841,    -1,    -1,
      -1,    -1,    -1,   847,    -1,    -1,    -1,  1174,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,  1547,   862,    -1,
    1187,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1209,    -1,    -1,   100,   101,    -1,  1215,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1598,  1599,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1617,  1618,    -1,
      -1,   935,    -1,   148,   149,    -1,    -1,    -1,  1265,    -1,
    1267,   156,   157,  1633,    -1,  1272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1310,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,  1328,    -1,    19,  1331,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    47,    48,    -1,    50,  1039,  1726,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1734,    -1,    -1,    -1,    -1,    -1,
      -1,    67,  1379,    -1,  1381,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1079,  1403,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,  1413,  1414,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1789,
      -1,    -1,    -1,    -1,  1794,  1795,    -1,    -1,  1798,   125,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1133,
      -1,    -1,   148,   149,  1824,    -1,    -1,    -1,    -1,    -1,
     156,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1153,
      -1,    -1,    -1,  1157,    -1,    -1,    -1,  1161,  1485,    -1,
      -1,    -1,    -1,  1490,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1873,  1874,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      45,  1205,    -1,    -1,    49,  1532,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1907,    -1,    -1,
      -1,    -1,    -1,    68,    -1,    -1,    -1,  1554,    -1,    -1,
    1557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1252,  1253,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,  1291,  1292,  1293,
      -1,  1295,  1296,    -1,    -1,    -1,    -1,    -1,  1302,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,   152,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1350,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    45,  1403,
      47,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    -1,    64,    -1,    66,
      67,    68,    69,    -1,    71,    -1,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,  1772,    93,    -1,    95,    96,
      97,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    45,
     117,   118,    -1,    49,    -1,    51,    -1,    -1,   125,    -1,
      -1,  1485,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,    -1,    -1,    -1,   144,   145,   146,
      -1,   148,   149,    -1,    -1,    -1,   153,    -1,   155,   156,
     157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,    95,
      96,    -1,    98,   170,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,    -1,   113,   114,   115,
      -1,   117,   118,  1547,    -1,    -1,    -1,    -1,    -1,   125,
      -1,    -1,  1556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,
      -1,    -1,   148,   149,    -1,    -1,    -1,   153,    -1,   155,
     156,   157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1598,  1599,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,  1655,    -1,    45,    -1,    47,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    -1,    64,    -1,    66,    67,    68,    69,    -1,    71,
      -1,    -1,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    -1,    95,    96,    97,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1747,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   145,   146,    -1,   148,   149,    -1,    -1,
    1764,   153,    -1,   155,   156,   157,   158,   159,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,  1789,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1822,    -1,
      -1,  1825,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    45,    -1,    47,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      -1,    -1,    -1,    64,    -1,    -1,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    95,    96,    97,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,    -1,   113,   114,   115,    -1,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   145,    -1,    -1,   148,   149,    -1,
      -1,    -1,    -1,    -1,   155,   156,   157,   158,   159,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,   170,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    45,    -1,    47,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    -1,    -1,
      -1,    64,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    95,    96,    97,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,    -1,
     113,   114,   115,    -1,   117,   118,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   145,    -1,    -1,   148,   149,    -1,    -1,    -1,
      -1,    -1,   155,   156,   157,   158,   159,   160,   161,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    45,    -1,
      47,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,    -1,   113,   114,   115,    -1,
     117,   118,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,    -1,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,   155,   156,
     157,   158,   159,   160,   161,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    45,    -1,    47,    48,
      49,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,    -1,   113,   114,   115,    -1,   117,   118,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   145,    -1,    -1,   148,
     149,    -1,    -1,    -1,    -1,    -1,   155,   156,   157,   158,
     159,   160,   161,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    45,    -1,    47,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,    -1,   113,   114,   115,    -1,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   145,    -1,    -1,   148,   149,    -1,
      -1,    -1,    -1,    -1,   155,   156,   157,   158,   159,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    -1,    64,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    93,    -1,    -1,    -1,    97,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,    -1,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   145,   146,    -1,   148,   149,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      47,    48,    -1,    50,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    19,
      67,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    47,    48,    -1,
      50,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,   146,
      -1,   148,   149,    -1,    -1,    -1,   153,    -1,    -1,   156,
     157,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    -1,
      -1,    -1,    64,    -1,    -1,    67,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,   100,   101,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,    -1,    -1,    -1,   148,   149,    -1,    -1,
      -1,    -1,    -1,    -1,   156,   157,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    -1,    64,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      97,    -1,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    47,    48,    -1,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,    67,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    47,    48,    -1,    50,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   145,   146,    -1,   148,   149,    -1,    -1,
       3,    -1,    -1,    -1,   156,   157,     9,    -1,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    97,    -1,    -1,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,    -1,    -1,    -1,   148,   149,    -1,    -1,     3,
      -1,    -1,    -1,   156,   157,     9,    -1,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,   100,   101,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,   148,   149,    -1,    -1,     3,    -1,
      -1,    -1,   156,   157,     9,    -1,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    97,    -1,    -1,   100,   101,    -1,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,
      -1,    49,    -1,    51,    -1,    -1,    -1,    -1,    -1,   144,
      -1,    -1,    -1,   148,   149,    -1,    -1,    -1,    66,    -1,
      68,   156,   157,    71,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    -1,    93,    -1,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,   146,    -1,
     148,   149,    -1,    -1,    -1,   153,    -1,   155,   156,   157,
     158,   159,   160,   161,    45,    -1,    -1,    -1,    49,    -1,
      51,    -1,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    66,    -1,    68,    69,    -1,
      71,    -1,    -1,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    -1,    93,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   145,   146,    -1,   148,   149,    -1,
      -1,    -1,   153,    -1,   155,   156,   157,   158,   159,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    -1,    -1,    49,    -1,    51,
      -1,   145,   146,    -1,   148,   149,    -1,    -1,    -1,   153,
      -1,    -1,   156,   157,    66,    -1,    68,    -1,    -1,    71,
      -1,    -1,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    -1,    95,    96,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,    -1,    -1,    -1,
      -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   145,   146,    -1,   148,   149,    -1,    -1,
      -1,   153,    -1,   155,   156,   157,   158,   159,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,   146,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
      -1,   156,   157,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,   127,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   147,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
     127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,   127,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
      -1,   156,   157,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    47,    48,    -1,    50,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,   153,    -1,
     155,   156,   157,   158,   159,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,   147,   148,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,   153,    -1,
     155,   156,   157,   158,   159,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      45,    -1,    47,    48,    49,    -1,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    -1,   117,   118,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,   146,    -1,   148,   149,    -1,    -1,    -1,   153,    -1,
      -1,   156,   157,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   145,   146,    -1,   148,
     149,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   146,    -1,   148,   149,    -1,    12,    13,
      14,    15,    16,   156,   157,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   145,   146,    -1,   148,   149,    -1,    -1,    -1,    -1,
      -1,    -1,   156,   157,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,   100,   101,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   147,   148,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
      -1,   156,   157,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,    -1,   156,
     157,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,   156,   157,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
      -1,   156,   157,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,    -1,    -1,    -1,    -1,   156,   157,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   156,   157,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    12,    13,    14,    15,    16,    -1,
      67,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    47,
      48,    -1,    50,   100,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   100,   101,    -1,    -1,    -1,    -1,    -1,   156,
     157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,   149,    -1,    12,    13,    14,    15,    16,   156,   157,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    -1,    46,    47,    48,
      -1,    50,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    -1,    67,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    -1,    46,    47,    48,    -1,
      50,   100,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     100,   101,    -1,    -1,    -1,    -1,    -1,   156,   157,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,   148,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   147,   148,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   127,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,   148,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,   100,   101,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,
      -1,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,   107,   108,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
     146,    -1,   148,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,   100,   101,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    45,    -1,    -1,
      -1,    49,    -1,    51,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,    -1,   113,   114,   115,    45,   117,
     118,    -1,    49,    -1,    51,    -1,    -1,   125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,    -1,
      -1,    68,    -1,    -1,    -1,    -1,    -1,   145,    -1,   147,
     148,   149,    -1,    -1,    -1,    -1,    -1,   155,   156,   157,
     158,   159,   160,   161,    -1,    -1,    -1,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,    -1,   113,   114,   115,    45,
     117,   118,    -1,    49,    -1,    51,    -1,    -1,   125,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,   145,   146,
      -1,   148,   149,    -1,    -1,    -1,    -1,    -1,   155,   156,
     157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,    95,
      96,    -1,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,    -1,   113,   114,   115,
      45,   117,   118,    -1,    49,    -1,    51,    -1,    -1,   125,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,   145,
      -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,   155,
     156,   157,   158,   159,   160,   161,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,    -1,   113,   114,
     115,    45,   117,   118,    -1,    49,    -1,    51,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,
     145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,   157,   158,   159,   160,   161,    -1,    -1,    -1,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,    -1,   113,
     114,   115,    45,   117,   118,    -1,    49,    -1,    51,    -1,
      -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
      -1,   145,    -1,    -1,   148,   149,    -1,    -1,    -1,    -1,
      -1,   155,   156,   157,   158,   159,   160,   161,    -1,    -1,
      -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,    -1,
     113,   114,   115,    45,   117,   118,    -1,    49,    -1,    51,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,
      -1,    -1,   145,    -1,    -1,   148,   149,    -1,    -1,    -1,
      -1,    -1,   155,   156,   157,   158,   159,   160,   161,    -1,
      -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
      -1,   113,   114,   115,    45,   117,   118,    -1,    49,    -1,
      51,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,
      -1,    -1,    -1,   145,    -1,    -1,   148,   149,    -1,    -1,
      -1,    -1,    -1,   155,   156,   157,   158,   159,   160,   161,
      -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,    -1,   113,   114,   115,    -1,   117,   118,    -1,    -1,
      -1,    -1,    -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   145,    -1,    -1,   148,   149,    -1,
      -1,    -1,    -1,    -1,   155,   156,   157,   158,   159,   160,
     161
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   172,   379,   380,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    47,    48,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    64,    67,    68,    93,    97,    98,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   112,   125,
     145,   146,   148,   149,   156,   157,   175,   176,   190,   271,
     272,   273,   274,   275,   276,   277,   278,   279,   280,   281,
     282,   284,   286,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   298,   300,   301,   302,   304,   305,   309,   310,
     311,   312,   313,   315,   321,   322,   323,   324,   335,   338,
     371,   374,   384,   389,   391,   397,   401,   406,   407,   408,
     409,   410,   411,   412,   413,   433,   450,   451,   452,   453,
       0,   172,   176,   190,   275,   277,   286,   289,   301,   305,
     310,   111,   145,    53,    56,    57,    59,   145,   145,   395,
     396,   397,   297,   298,   100,   101,   176,   351,   372,   373,
     351,   145,   384,   145,   145,   145,   190,   396,   401,   407,
     408,   409,   411,   412,   413,   100,   312,   150,   172,   278,
     286,   289,   406,   410,   449,   450,   453,   454,   170,   173,
     142,   169,   211,   354,    84,   151,   390,   351,   173,   173,
     173,   170,   100,   101,   145,   190,   283,   392,   401,   402,
     403,   404,   405,   406,   410,   414,   415,   416,   417,   418,
     424,     3,    43,    44,    46,    50,   303,     3,     4,   149,
     190,   277,   290,   294,   296,   306,   311,   386,   406,   410,
     453,   275,   277,   289,   301,   305,   310,   387,   406,   410,
      60,   295,   295,   290,   296,   295,   290,   295,   290,   148,
     395,   151,   173,   145,   153,   219,   395,   395,   172,   266,
     267,   149,   286,   289,   451,   351,   351,   384,   169,   289,
     145,   190,   392,   401,   406,   415,   149,   190,   453,   385,
      60,    61,    62,    63,   149,   167,   351,   360,   362,   366,
     368,   369,    45,    49,    51,    68,    95,    96,    98,   109,
     110,   111,   113,   114,   115,   117,   118,   145,   149,   155,
     158,   159,   160,   161,   174,   175,   177,   178,   179,   182,
     189,   190,   191,   192,   195,   196,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   208,   213,   286,   311,
     352,   353,   370,   449,   454,    52,   149,   190,   285,   289,
     293,   294,   300,   301,   307,   308,   309,   310,   314,   321,
     322,   338,   347,   349,   433,   445,   446,   447,   448,   453,
     454,   100,   101,   153,   176,   311,   424,   397,   145,   367,
     368,   145,   145,   177,   149,   189,   190,   206,   207,   311,
     147,   370,   289,   407,   408,   409,   411,   412,   413,   147,
     147,   147,   147,   147,   147,   147,   149,   286,   433,   451,
     149,   156,   190,   208,   277,   278,   285,   287,   289,   301,
     308,   310,   342,   343,   346,   347,   348,   445,   453,   145,
     406,   410,   453,   145,   151,    21,   153,   208,   355,   145,
     351,   219,   145,   151,   151,   151,   396,   401,   403,   404,
     405,   414,   416,   417,   418,   289,   402,   415,   151,    93,
     394,   149,   395,   432,   433,   395,   395,   390,   266,   145,
     395,   432,   390,   395,   395,   289,   392,   145,   145,   288,
     289,   286,   289,   172,   286,   449,   454,   313,   153,   390,
     266,   351,   354,   277,   294,   388,   406,   410,   153,   390,
     266,   372,   289,   301,   289,   289,   100,   312,   100,   101,
     176,   311,   316,   372,   172,   176,   350,   144,   172,     3,
     282,   284,   289,   293,   219,   172,   172,   394,   145,   394,
     173,   208,   396,   401,   289,   145,   172,   351,   153,   351,
     153,   351,   127,   156,   157,   365,   147,   151,   351,   369,
     145,   189,   145,   145,   192,   145,   189,   145,   145,   189,
     189,    18,    20,    80,   149,   158,   159,   193,   194,   208,
     215,   219,   324,   352,   453,   151,   172,   145,   179,   154,
     154,   111,   114,   116,   117,   118,   145,   148,   149,   153,
     154,   128,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   169,   210,   211,   212,   192,   192,   162,   156,
     163,   164,   158,   159,   119,   120,   121,   122,   165,   166,
     123,   124,   157,   155,   167,   125,   126,   168,   145,   190,
     428,   429,   430,   431,   432,   148,   147,   151,   395,   150,
     172,   287,   289,   301,   308,   310,   444,   445,   453,   454,
     145,   149,   157,   169,   190,   433,   434,   435,   436,   437,
     438,   439,   456,   190,   314,   453,   289,   308,   295,   290,
     395,   147,   287,   289,   446,   287,   433,   446,     9,   339,
     351,   336,   153,   360,   169,   360,    12,    83,    98,   100,
     101,   175,   398,   399,   400,   147,   172,   147,   151,   147,
     147,   147,   147,   147,   147,   147,   145,   395,   432,   433,
     145,   432,   433,   172,   286,   451,   172,   173,   173,   145,
     157,   190,   401,   419,   420,   421,   422,   423,   424,   425,
     426,   427,   127,   453,   173,   173,   351,   351,   172,   172,
     172,    98,   148,   149,   175,   176,   355,   356,   357,   358,
     359,   146,   208,   214,   145,   172,   172,   172,   172,   401,
     403,   404,   405,   414,   416,   417,   418,   147,   147,   147,
     147,   147,   147,   147,   402,   415,   395,   145,   354,   150,
     172,   219,   390,   172,   219,   392,   215,   353,   215,   353,
     392,   382,   219,   390,   394,   153,   390,   266,   382,   219,
     390,   318,   319,   317,   153,   127,   289,   344,   345,   348,
     349,   147,   151,    65,   268,   269,   173,   289,   282,   156,
     208,   172,   401,   343,   382,   150,   172,   145,   364,   362,
     363,   352,   149,   352,   352,   352,   208,   352,   147,   352,
     352,   352,   172,   147,   158,   159,   194,    17,   291,   147,
     151,   147,   156,   157,   147,   214,   208,   153,   176,   176,
     109,   149,   176,   146,   183,   184,   208,   109,   149,   176,
     324,   208,   183,   176,   153,   208,   192,   195,   195,   195,
     196,   196,   197,   197,   198,   198,   198,   198,   199,   199,
     200,   201,   202,   203,   204,   152,   215,   172,   429,   430,
     431,   289,   428,   395,   395,   149,   176,    73,   299,   208,
     353,   176,   287,   433,   446,   289,   293,   453,   172,   435,
     436,   437,   150,   172,    17,   208,   289,   434,   456,   395,
     395,   433,   287,   444,   454,   289,   176,   395,   287,   446,
     311,   151,   455,   169,   211,   340,   153,   339,   147,   353,
     147,   147,   151,   145,   170,   208,   170,   177,   145,   395,
     432,   433,   145,   432,   433,   172,   172,   150,   150,   145,
     401,   420,   421,   422,   425,    17,   289,   419,   423,   145,
     395,   438,   456,   395,   395,   456,   145,   395,   438,   395,
     395,   173,   207,   351,   150,   151,   150,   151,   456,   456,
     127,   341,   342,   343,   341,   351,   149,   176,   172,   152,
     151,   455,   355,   148,   149,   152,   359,   147,   151,   172,
     341,   176,   392,   176,   147,   147,   147,   147,   147,   147,
     145,   395,   432,   433,   145,   395,   432,   433,   392,   177,
     433,   208,   219,   344,   147,   147,   147,   147,   380,   381,
     219,   382,   219,   390,   381,   219,   153,   153,   153,   325,
     173,   173,   176,   270,   351,    17,    66,    68,    71,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    87,    88,    89,    90,    91,    93,   100,   101,   112,
     172,   215,   216,   217,   218,   219,   220,   221,   223,   224,
     234,   238,   239,   240,   241,   242,   243,   248,   249,   255,
     256,   257,   271,   289,   293,   351,   391,    65,   170,   173,
     173,   173,   341,   173,   381,   275,   277,   286,   375,   376,
     377,   378,   370,   169,   361,   361,   147,   172,   151,   147,
     147,   151,   147,   192,   147,   147,   147,   192,    17,   291,
     208,   147,   147,   146,   153,   192,   150,   173,   183,   109,
     113,   115,   176,   185,   186,   187,   147,   151,   185,   150,
     151,   144,   356,   206,   152,   344,   147,   147,   147,   428,
     185,   287,   446,   149,   156,   190,   208,   311,   208,   289,
     344,   147,   147,   147,     5,   289,   395,   434,   153,   176,
     424,     9,   351,   144,   355,   339,   455,   153,   147,   399,
     183,   173,   147,   172,   172,   344,   344,   425,   147,   147,
     147,   147,   145,   401,   424,   419,   423,   172,   172,   150,
     173,   456,   172,   172,   173,   173,   173,   173,   354,   172,
     206,   207,   208,   393,   355,   357,   144,   172,   146,   208,
     341,   173,   169,   145,   395,   432,   433,   145,   395,   432,
     433,   172,   172,   394,   147,   173,   173,   383,   381,   219,
     383,   325,   325,   325,     3,     9,    68,   144,   272,   279,
     280,   286,   289,   326,   331,   449,   147,   151,   151,   170,
     145,    56,    57,   170,   219,   271,   391,   145,    17,   217,
     145,   145,   170,   351,   170,   351,   156,   351,   153,   216,
     145,   145,   145,   219,   208,   209,   209,    13,   258,    69,
     225,   170,   173,   221,    73,   170,   351,    86,   244,   350,
     289,   152,   270,   170,   150,   150,   173,   151,   383,   392,
     173,   170,   173,   170,   173,   147,   353,   367,   367,   176,
      73,   180,   181,   352,   192,   192,   192,   192,   192,   153,
     356,   151,   144,   188,   149,   186,   188,   188,   150,   151,
     116,   148,   184,   150,   215,   455,   206,   173,   145,   395,
     432,   433,   150,   172,   173,   173,   173,   208,   173,   145,
     395,   438,   433,   288,     5,   156,   173,   208,   339,   395,
     395,   311,   340,   455,   144,   144,   172,   147,   170,   344,
     344,   173,   173,   147,   145,   395,   432,   433,   145,   395,
     438,   401,   395,   395,   344,   344,   150,   343,   346,   346,
     347,   147,   151,   151,   147,   185,   127,   161,   173,   173,
     355,   208,   173,   147,   208,   172,   172,   344,   344,   354,
     395,   151,   147,   144,   383,   144,   144,   144,   144,   286,
     324,   332,   449,   286,   331,   145,   320,   170,   170,   145,
     152,   190,   327,   328,   334,   401,   402,   415,   151,   170,
     351,   172,   351,   183,   170,   219,   170,   219,   215,    75,
     147,   172,   147,   172,   170,   170,   215,   170,   356,   170,
     215,   214,   215,   104,   105,   106,   107,   108,   250,   252,
     253,   170,    92,   170,    79,   145,   145,   173,   144,   170,
     170,   145,   217,   219,   395,   170,   147,   172,   144,   144,
     172,   151,   151,   147,   152,   147,   151,   152,   356,   455,
     214,   114,   185,   186,   149,   186,   149,   186,   150,   144,
     147,   172,   150,   150,   150,   173,   147,   172,   208,   208,
     173,   150,   173,   455,   337,   153,   340,   144,   375,   173,
     173,   147,   147,   172,   172,   173,   173,   173,   172,   172,
     173,   207,   207,   150,   150,   173,   147,   395,   344,   344,
     173,   173,   215,   144,   320,   320,   320,   145,   190,   329,
     330,   432,   440,   441,   442,   443,   170,   151,   170,   327,
     170,   370,   396,   401,   208,   289,   151,   170,   333,   334,
     333,   351,   127,   348,   349,   147,   147,   145,   217,   215,
     226,   271,   273,   276,   282,   289,   293,   217,   169,   170,
     215,   235,   236,   271,   170,   455,   147,   147,   147,   219,
     252,   253,   145,   208,   145,   177,   226,   192,   245,   103,
     217,   395,   376,   172,   172,   208,   181,   208,   455,   144,
     150,   150,   185,   185,   344,   150,   344,   173,   173,   150,
     150,   144,   153,   339,   173,   147,   147,   344,   344,   147,
     147,   150,   151,   127,   343,   127,   150,   173,   173,   173,
     147,   147,   150,   441,   442,   443,   289,   440,   151,   170,
     395,   395,   170,   147,   401,   395,   217,    72,    73,   153,
     229,   230,   231,   147,   215,   147,   215,   289,   215,   216,
     139,   140,   141,   161,   170,   237,   147,   152,   216,   144,
     153,   231,   217,   145,   172,   170,   177,   147,   152,   147,
     147,   151,   152,   243,   247,   351,   392,   144,   150,   150,
     173,   173,   150,   150,   339,   455,   144,   173,   173,   172,
     173,   150,   147,   147,   147,   147,   147,   440,   395,   328,
     207,   227,   228,   393,   152,   172,   217,   229,   170,   147,
     217,   170,   100,   169,   215,   216,   215,   217,   236,   170,
     170,   172,   172,   254,   287,   289,   449,   152,   170,   149,
     177,   259,   260,   261,   217,   192,   183,    70,   102,   244,
     246,   147,   147,   455,   144,   147,   147,   346,   145,   395,
     432,   433,   330,   127,   151,   152,   264,   265,   271,   170,
     173,   216,   215,   140,   161,   237,   170,   161,   173,   216,
     264,   254,   173,   145,   190,   392,   440,   176,   152,    98,
     145,   147,   152,   151,    70,   147,   217,   145,   217,   217,
     144,   172,   207,   227,   230,   232,   233,   271,   146,   146,
     215,   216,   215,   232,   173,   170,   251,   289,   259,   150,
     207,   170,   259,   261,   217,   215,   103,   103,   344,   217,
     222,   173,   230,   161,   161,   161,   173,   251,   206,   147,
     152,   177,   147,   147,   152,   147,   247,    70,   242,   173,
     217,   144,   222,   215,   146,   215,   215,   144,   147,   219,
     177,   262,   145,   170,   262,   217,    70,   147,   219,   151,
     152,   207,   147,   217,   177,   176,   263,   147,   170,   147,
     151,   170,   176
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   171,   172,   173,   174,   174,   174,   174,   174,   175,
     175,   175,   175,   175,   175,   175,   176,   176,   176,   177,
     178,   178,   179,   179,   179,   179,   179,   179,   179,   179,
     180,   180,   181,   181,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   183,   183,   183,   184,   184,
     185,   185,   186,   186,   186,   186,   186,   186,   186,   187,
     187,   187,   188,   188,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   190,   190,
     190,   191,   191,   191,   191,   192,   192,   192,   192,   192,
     192,   192,   192,   192,   193,   193,   193,   193,   194,   194,
     195,   195,   196,   196,   196,   196,   197,   197,   197,   198,
     198,   198,   199,   199,   199,   199,   199,   200,   200,   200,
     201,   201,   202,   202,   203,   203,   204,   204,   205,   205,
     206,   206,   206,   207,   208,   208,   208,   209,   209,   210,
     210,   211,   211,   212,   212,   212,   212,   212,   212,   212,
     212,   212,   212,   212,   213,   213,   214,   214,   214,   214,
     215,   215,   216,   216,   217,   217,   217,   217,   217,   217,
     217,   217,   217,   217,   217,   217,   217,   218,   219,   219,
     220,   220,   221,   221,   221,   221,   221,   222,   222,   223,
     224,   224,   224,   224,   224,   225,   225,   226,   226,   226,
     226,   227,   227,   227,   228,   228,   229,   229,   230,   230,
     231,   232,   232,   233,   233,   234,   234,   234,   234,   234,
     234,   235,   235,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   237,   237,   237,   237,   238,   238,   238,   238,   238,
     238,   238,   238,   238,   238,   238,   238,   238,   238,   238,
     238,   238,   238,   238,   238,   239,   239,   240,   241,   242,
     243,   243,   244,   244,   245,   245,   246,   247,   247,   247,
     247,   247,   247,   248,   248,   249,   249,   249,   250,   250,
     251,   251,   252,   252,   252,   252,   253,   254,   254,   254,
     254,   254,   255,   256,   256,   257,   257,   257,   257,   257,
     258,   258,   259,   259,   260,   260,   261,   261,   262,   262,
     262,   263,   263,   264,   264,   265,   265,   266,   266,   267,
     267,   268,   268,   269,   269,   270,   270,   271,   271,   271,
     272,   272,   273,   273,   273,   273,   273,   274,   274,   274,
     275,   275,   275,   276,   276,   276,   276,   276,   277,   277,
     278,   278,   279,   279,   279,   280,   280,   280,   280,   280,
     281,   281,   282,   282,   282,   282,   283,   283,   284,   284,
     284,   285,   285,   285,   286,   286,   286,   287,   287,   287,
     288,   288,   289,   289,   290,   290,   291,   291,   291,   291,
     291,   292,   293,   293,   293,   294,   294,   295,   295,   295,
     295,   295,   295,   295,   295,   296,   296,   296,   296,   296,
     296,   296,   296,   296,   296,   296,   296,   296,   296,   296,
     296,   296,   296,   296,   296,   296,   296,   296,   296,   296,
     297,   297,   298,   299,   299,   300,   300,   300,   300,   300,
     301,   301,   302,   302,   302,   302,   303,   303,   303,   303,
     303,   303,   304,   304,   304,   304,   305,   306,   305,   305,
     307,   307,   307,   307,   308,   308,   308,   309,   309,   309,
     309,   310,   310,   310,   311,   311,   311,   311,   311,   311,
     312,   312,   312,   313,   313,   314,   314,   316,   315,   317,
     315,   318,   315,   319,   315,   315,   320,   320,   321,   321,
     322,   322,   323,   323,   323,   324,   324,   324,   324,   324,
     324,   324,   324,   325,   325,   326,   326,   326,   326,   326,
     326,   326,   326,   326,   326,   327,   327,   327,   328,   328,
     328,   329,   329,   329,   330,   331,   331,   332,   332,   333,
     333,   334,   335,   336,   335,   335,   335,   337,   335,   335,
     335,   338,   338,   339,   339,   339,   339,   340,   340,   341,
     341,   341,   341,   341,   341,   341,   342,   342,   342,   342,
     343,   343,   344,   344,   344,   344,   345,   345,   345,   345,
     346,   346,   346,   346,   346,   347,   347,   347,   347,   347,
     348,   348,   349,   349,   350,   350,   351,   351,   351,   352,
     352,   352,   353,   353,   354,   354,   354,   355,   355,   356,
     356,   356,   356,   356,   357,   357,   358,   358,   359,   359,
     359,   359,   359,   360,   360,   361,   361,   363,   362,   364,
     362,   362,   362,   365,   365,   365,   365,   366,   366,   366,
     366,   367,   367,   368,   368,   369,   369,   370,   370,   370,
     370,   371,   371,   371,   372,   372,   373,   373,   374,   374,
     375,   375,   376,   376,   377,   377,   377,   378,   378,   379,
     379,   380,   380,   381,   381,   382,   383,   384,   384,   384,
     384,   384,   385,   384,   386,   384,   387,   384,   388,   384,
     389,   389,   389,   390,   390,   391,   391,   391,   391,   391,
     391,   391,   391,   391,   391,   392,   392,   392,   393,   394,
     394,   395,   395,   396,   396,   397,   398,   398,   399,   399,
     399,   400,   400,   400,   400,   400,   400,   401,   401,   402,
     402,   402,   402,   403,   403,   403,   403,   404,   404,   404,
     404,   404,   404,   404,   405,   405,   405,   405,   406,   406,
     406,   407,   407,   407,   407,   407,   408,   408,   408,   408,
     409,   409,   409,   409,   409,   409,   410,   410,   410,   411,
     411,   411,   411,   411,   412,   412,   412,   412,   413,   413,
     413,   413,   413,   413,   414,   414,   415,   415,   415,   415,
     416,   416,   416,   416,   417,   417,   417,   417,   417,   417,
     417,   418,   418,   418,   418,   418,   419,   419,   419,   419,
     419,   420,   420,   420,   421,   421,   421,   421,   422,   422,
     422,   423,   423,   423,   423,   423,   424,   424,   425,   425,
     425,   426,   426,   427,   427,   428,   428,   428,   429,   429,
     429,   429,   429,   430,   430,   430,   430,   431,   431,   431,
     432,   432,   432,   432,   433,   433,   433,   433,   434,   434,
     434,   434,   435,   435,   435,   435,   435,   436,   436,   436,
     436,   437,   437,   437,   438,   438,   438,   439,   439,   439,
     439,   439,   439,   440,   440,   440,   441,   441,   441,   441,
     441,   442,   442,   442,   442,   443,   443,   444,   444,   444,
     445,   445,   446,   446,   446,   446,   446,   446,   447,   447,
     447,   447,   447,   447,   447,   447,   447,   447,   448,   448,
     448,   448,   449,   449,   449,   450,   450,   451,   451,   451,
     451,   451,   451,   452,   452,   452,   452,   452,   452,   453,
     453,   453,   454,   454,   455,   455,   456,   456
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     3,     3,     3,     5,     6,
       1,     3,     3,     3,     1,     6,     4,     4,     4,     3,
       3,     3,     3,     3,     2,     5,     3,     3,     3,     5,
       2,     2,     7,     8,     5,     0,     1,     3,     1,     1,
       1,     3,     1,     2,     4,     3,     5,     3,     5,     2,
       2,     2,     0,     2,     1,     1,     1,     2,     2,     2,
       2,     2,     2,     4,     2,     4,     6,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     5,     5,     4,
       5,     5,     5,     4,     2,     2,     3,     3,     1,     1,
       1,     3,     1,     3,     3,     3,     1,     3,     3,     1,
       3,     3,     1,     3,     3,     3,     3,     1,     3,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     5,     4,     1,     1,     3,     6,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     7,     1,     1,     3,     3,
       1,     3,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     2,     6,
       1,     2,     1,     2,     1,     2,     1,     1,     2,     2,
       3,     5,    10,     5,    10,     5,     7,     1,     1,     1,
       2,     1,     3,     1,     1,     3,     3,     2,     1,     2,
       2,     0,     1,     2,     3,     7,     4,     7,     6,     7,
       4,     1,     3,     4,     5,     4,     1,     2,     3,     5,
       2,     3,     4,     5,     7,     3,     5,     5,     7,     7,
       7,     1,     1,     1,     1,     3,     4,     2,     3,     3,
       2,     3,     2,     3,     3,     6,     2,     2,     3,     3,
       3,     3,     3,     3,     5,     1,     1,     5,     5,     4,
       0,     1,     4,     6,     1,     3,     4,     3,     5,     3,
       3,     6,     7,     3,     5,     3,     3,     4,     8,     9,
       0,     2,     1,     1,     1,     1,     2,     1,     2,     2,
       2,     1,     3,     1,     1,     6,     8,    10,    12,    14,
       0,     1,     0,     1,     1,     3,     4,     7,     0,     1,
       3,     1,     3,     0,     1,     1,     2,     0,     1,     4,
       5,     0,     1,     3,     4,     1,     3,     2,     2,     1,
       7,     5,     1,     1,     1,     1,     1,     2,     3,     6,
       3,     3,     4,     1,     2,     2,     3,     8,     8,     8,
       5,     9,     2,     2,     5,     3,     5,     4,     3,     4,
       4,     7,     2,     1,     1,     1,     3,     6,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     4,     1,     2,     3,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     5,     0,     1,     1,     2,     2,     3,     3,
       1,     3,     1,     2,     2,     2,     4,     4,     4,     4,
       1,     1,     1,     2,     2,     3,     1,     0,     3,     2,
       1,     2,     2,     3,     1,     2,     2,     1,     2,     2,
       3,     1,     2,     2,     1,     2,     3,     1,     2,     3,
       1,     3,     4,     1,     1,     1,     1,     0,     7,     0,
       8,     0,     8,     0,     8,     1,     0,     3,     3,     3,
       1,     1,     2,     1,     1,     1,     2,     1,     2,     1,
       2,     1,     2,     0,     2,     3,     4,     4,     3,     2,
       2,     3,     3,     2,     1,     0,     1,     4,     1,     2,
       2,     0,     1,     4,     1,     2,     3,     1,     2,     0,
       1,     2,     6,     0,     8,     7,     9,     0,    12,    11,
       1,     3,     3,     2,     2,     4,     5,     0,     2,     0,
       1,     1,     1,     5,     5,     5,     1,     5,     5,     9,
       1,     5,     0,     1,     1,     5,     1,     1,     5,     5,
       1,     3,     3,     4,     1,     1,     1,     1,     2,     1,
       3,     3,     2,     3,     1,     3,     1,     1,     1,     1,
       1,     2,     1,     1,     0,     2,     2,     1,     4,     0,
       1,     2,     3,     4,     2,     2,     1,     2,     2,     5,
       5,     7,     6,     1,     3,     0,     2,     0,     5,     0,
       5,     3,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     6,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,    10,
       1,     4,     2,     2,     1,     1,     5,     2,     5,     0,
       1,     3,     4,     0,     1,     0,     0,     1,     1,     1,
       2,     5,     0,     8,     0,     7,     0,     7,     0,     8,
       1,     2,     3,     0,     4,     3,     4,     4,     4,     4,
       5,     5,     5,     5,     6,     1,     1,     1,     3,     0,
       5,     0,     1,     1,     2,     6,     1,     3,     0,     1,
       4,     1,     1,     1,     1,     1,     1,     1,     3,     2,
       1,     2,     2,     2,     3,     4,     5,     2,     4,     5,
       4,     5,     3,     4,     8,     9,     3,     4,     2,     1,
       2,     6,     8,     9,     3,     4,     2,     3,     4,     5,
       4,     5,     4,     5,     3,     4,     1,     1,     1,     4,
       8,     9,     3,     4,     2,     3,     3,     4,     4,     5,
       4,     5,     3,     4,     1,     3,     2,     1,     2,     2,
       2,     3,     4,     5,     2,     4,     5,     4,     5,     3,
       4,     6,     8,     9,     3,     4,     2,     4,     1,     2,
       2,     2,     3,     4,     2,     4,     4,     3,     6,     8,
       3,     2,     4,     1,     2,     2,     1,     1,     2,     3,
       4,     2,     4,     6,     8,     1,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     5,     8,     3,
       2,     3,     7,     1,     5,     5,     6,     6,     1,     3,
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
#line 532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 6769 "Parser/parser.cc"
    break;

  case 3:
#line 536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6775 "Parser/parser.cc"
    break;

  case 4:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6781 "Parser/parser.cc"
    break;

  case 5:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6787 "Parser/parser.cc"
    break;

  case 6:
#line 545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6793 "Parser/parser.cc"
    break;

  case 7:
#line 546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6799 "Parser/parser.cc"
    break;

  case 8:
#line 547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6805 "Parser/parser.cc"
    break;

  case 18:
#line 564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6811 "Parser/parser.cc"
    break;

  case 19:
#line 568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6817 "Parser/parser.cc"
    break;

  case 20:
#line 572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6823 "Parser/parser.cc"
    break;

  case 21:
#line 574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6833 "Parser/parser.cc"
    break;

  case 22:
#line 585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6839 "Parser/parser.cc"
    break;

  case 23:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6845 "Parser/parser.cc"
    break;

  case 25:
#line 590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6851 "Parser/parser.cc"
    break;

  case 26:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6857 "Parser/parser.cc"
    break;

  case 27:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6863 "Parser/parser.cc"
    break;

  case 28:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6869 "Parser/parser.cc"
    break;

  case 29:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6879 "Parser/parser.cc"
    break;

  case 31:
#line 612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6890 "Parser/parser.cc"
    break;

  case 32:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6899 "Parser/parser.cc"
    break;

  case 33:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6905 "Parser/parser.cc"
    break;

  case 35:
#line 634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "New array subscript is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6911 "Parser/parser.cc"
    break;

  case 36:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6917 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6927 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6933 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6939 "Parser/parser.cc"
    break;

  case 40:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6945 "Parser/parser.cc"
    break;

  case 41:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6951 "Parser/parser.cc"
    break;

  case 42:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6957 "Parser/parser.cc"
    break;

  case 43:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6963 "Parser/parser.cc"
    break;

  case 44:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 6969 "Parser/parser.cc"
    break;

  case 45:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6975 "Parser/parser.cc"
    break;

  case 46:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 6981 "Parser/parser.cc"
    break;

  case 47:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6987 "Parser/parser.cc"
    break;

  case 48:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6993 "Parser/parser.cc"
    break;

  case 49:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6999 "Parser/parser.cc"
    break;

  case 50:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7005 "Parser/parser.cc"
    break;

  case 51:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7011 "Parser/parser.cc"
    break;

  case 52:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7017 "Parser/parser.cc"
    break;

  case 53:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7023 "Parser/parser.cc"
    break;

  case 54:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7033 "Parser/parser.cc"
    break;

  case 55:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7039 "Parser/parser.cc"
    break;

  case 57:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7045 "Parser/parser.cc"
    break;

  case 58:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7051 "Parser/parser.cc"
    break;

  case 61:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7057 "Parser/parser.cc"
    break;

  case 63:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7063 "Parser/parser.cc"
    break;

  case 64:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7069 "Parser/parser.cc"
    break;

  case 65:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7075 "Parser/parser.cc"
    break;

  case 66:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7081 "Parser/parser.cc"
    break;

  case 67:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7087 "Parser/parser.cc"
    break;

  case 68:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7093 "Parser/parser.cc"
    break;

  case 69:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7099 "Parser/parser.cc"
    break;

  case 70:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7105 "Parser/parser.cc"
    break;

  case 71:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7113 "Parser/parser.cc"
    break;

  case 72:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7119 "Parser/parser.cc"
    break;

  case 73:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7128 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7134 "Parser/parser.cc"
    break;

  case 77:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7140 "Parser/parser.cc"
    break;

  case 78:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7160 "Parser/parser.cc"
    break;

  case 79:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7166 "Parser/parser.cc"
    break;

  case 80:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7172 "Parser/parser.cc"
    break;

  case 81:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7178 "Parser/parser.cc"
    break;

  case 82:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7184 "Parser/parser.cc"
    break;

  case 83:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7190 "Parser/parser.cc"
    break;

  case 84:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7196 "Parser/parser.cc"
    break;

  case 85:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7202 "Parser/parser.cc"
    break;

  case 86:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7208 "Parser/parser.cc"
    break;

  case 87:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7217 "Parser/parser.cc"
    break;

  case 88:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7223 "Parser/parser.cc"
    break;

  case 89:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7229 "Parser/parser.cc"
    break;

  case 90:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7235 "Parser/parser.cc"
    break;

  case 91:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7241 "Parser/parser.cc"
    break;

  case 92:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7247 "Parser/parser.cc"
    break;

  case 93:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7253 "Parser/parser.cc"
    break;

  case 94:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7259 "Parser/parser.cc"
    break;

  case 96:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7265 "Parser/parser.cc"
    break;

  case 97:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7271 "Parser/parser.cc"
    break;

  case 98:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7277 "Parser/parser.cc"
    break;

  case 99:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7283 "Parser/parser.cc"
    break;

  case 100:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7289 "Parser/parser.cc"
    break;

  case 101:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7295 "Parser/parser.cc"
    break;

  case 102:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7301 "Parser/parser.cc"
    break;

  case 103:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7307 "Parser/parser.cc"
    break;

  case 111:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7313 "Parser/parser.cc"
    break;

  case 113:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7319 "Parser/parser.cc"
    break;

  case 114:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7325 "Parser/parser.cc"
    break;

  case 115:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7331 "Parser/parser.cc"
    break;

  case 117:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7337 "Parser/parser.cc"
    break;

  case 118:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7343 "Parser/parser.cc"
    break;

  case 120:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7349 "Parser/parser.cc"
    break;

  case 121:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7355 "Parser/parser.cc"
    break;

  case 123:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7361 "Parser/parser.cc"
    break;

  case 124:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7367 "Parser/parser.cc"
    break;

  case 125:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7373 "Parser/parser.cc"
    break;

  case 126:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7379 "Parser/parser.cc"
    break;

  case 128:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7385 "Parser/parser.cc"
    break;

  case 129:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7391 "Parser/parser.cc"
    break;

  case 131:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7397 "Parser/parser.cc"
    break;

  case 133:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7403 "Parser/parser.cc"
    break;

  case 135:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7409 "Parser/parser.cc"
    break;

  case 137:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7415 "Parser/parser.cc"
    break;

  case 139:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7421 "Parser/parser.cc"
    break;

  case 141:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7427 "Parser/parser.cc"
    break;

  case 142:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7433 "Parser/parser.cc"
    break;

  case 145:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7445 "Parser/parser.cc"
    break;

  case 146:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7451 "Parser/parser.cc"
    break;

  case 147:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7457 "Parser/parser.cc"
    break;

  case 151:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7463 "Parser/parser.cc"
    break;

  case 152:
#line 966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7469 "Parser/parser.cc"
    break;

  case 153:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7475 "Parser/parser.cc"
    break;

  case 154:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7481 "Parser/parser.cc"
    break;

  case 155:
#line 972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7487 "Parser/parser.cc"
    break;

  case 156:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7493 "Parser/parser.cc"
    break;

  case 157:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7499 "Parser/parser.cc"
    break;

  case 158:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7505 "Parser/parser.cc"
    break;

  case 159:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7511 "Parser/parser.cc"
    break;

  case 160:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7517 "Parser/parser.cc"
    break;

  case 161:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7523 "Parser/parser.cc"
    break;

  case 162:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7529 "Parser/parser.cc"
    break;

  case 163:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7535 "Parser/parser.cc"
    break;

  case 164:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7541 "Parser/parser.cc"
    break;

  case 165:
#line 993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7547 "Parser/parser.cc"
    break;

  case 167:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7553 "Parser/parser.cc"
    break;

  case 168:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7559 "Parser/parser.cc"
    break;

  case 169:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7565 "Parser/parser.cc"
    break;

  case 171:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7571 "Parser/parser.cc"
    break;

  case 172:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7577 "Parser/parser.cc"
    break;

  case 184:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7583 "Parser/parser.cc"
    break;

  case 186:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7589 "Parser/parser.cc"
    break;

  case 187:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7595 "Parser/parser.cc"
    break;

  case 188:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7601 "Parser/parser.cc"
    break;

  case 189:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7607 "Parser/parser.cc"
    break;

  case 191:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7613 "Parser/parser.cc"
    break;

  case 192:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7619 "Parser/parser.cc"
    break;

  case 193:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7625 "Parser/parser.cc"
    break;

  case 194:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7631 "Parser/parser.cc"
    break;

  case 195:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7637 "Parser/parser.cc"
    break;

  case 198:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7643 "Parser/parser.cc"
    break;

  case 199:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7649 "Parser/parser.cc"
    break;

  case 200:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7655 "Parser/parser.cc"
    break;

  case 201:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7661 "Parser/parser.cc"
    break;

  case 202:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7675 "Parser/parser.cc"
    break;

  case 203:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7681 "Parser/parser.cc"
    break;

  case 204:
#line 1103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7690 "Parser/parser.cc"
    break;

  case 205:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7696 "Parser/parser.cc"
    break;

  case 206:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7702 "Parser/parser.cc"
    break;

  case 207:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7708 "Parser/parser.cc"
    break;

  case 208:
#line 1121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7714 "Parser/parser.cc"
    break;

  case 209:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7720 "Parser/parser.cc"
    break;

  case 210:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7726 "Parser/parser.cc"
    break;

  case 211:
#line 1132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7732 "Parser/parser.cc"
    break;

  case 212:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7738 "Parser/parser.cc"
    break;

  case 214:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7744 "Parser/parser.cc"
    break;

  case 215:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7750 "Parser/parser.cc"
    break;

  case 216:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7756 "Parser/parser.cc"
    break;

  case 217:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7762 "Parser/parser.cc"
    break;

  case 219:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7768 "Parser/parser.cc"
    break;

  case 220:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7774 "Parser/parser.cc"
    break;

  case 221:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7780 "Parser/parser.cc"
    break;

  case 223:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7786 "Parser/parser.cc"
    break;

  case 224:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7792 "Parser/parser.cc"
    break;

  case 225:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7798 "Parser/parser.cc"
    break;

  case 226:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7804 "Parser/parser.cc"
    break;

  case 227:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7810 "Parser/parser.cc"
    break;

  case 228:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7816 "Parser/parser.cc"
    break;

  case 229:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7822 "Parser/parser.cc"
    break;

  case 230:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7828 "Parser/parser.cc"
    break;

  case 232:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7847 "Parser/parser.cc"
    break;

  case 233:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7853 "Parser/parser.cc"
    break;

  case 234:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7859 "Parser/parser.cc"
    break;

  case 235:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7865 "Parser/parser.cc"
    break;

  case 236:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7872 "Parser/parser.cc"
    break;

  case 237:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7879 "Parser/parser.cc"
    break;

  case 238:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7885 "Parser/parser.cc"
    break;

  case 239:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7891 "Parser/parser.cc"
    break;

  case 240:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7897 "Parser/parser.cc"
    break;

  case 241:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7904 "Parser/parser.cc"
    break;

  case 242:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7911 "Parser/parser.cc"
    break;

  case 243:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7917 "Parser/parser.cc"
    break;

  case 244:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7923 "Parser/parser.cc"
    break;

  case 245:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7932 "Parser/parser.cc"
    break;

  case 246:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7938 "Parser/parser.cc"
    break;

  case 247:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 248:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 249:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 250:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 7962 "Parser/parser.cc"
    break;

  case 251:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 7968 "Parser/parser.cc"
    break;

  case 252:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 7974 "Parser/parser.cc"
    break;

  case 253:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 7980 "Parser/parser.cc"
    break;

  case 254:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 7986 "Parser/parser.cc"
    break;

  case 255:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 7992 "Parser/parser.cc"
    break;

  case 256:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 7998 "Parser/parser.cc"
    break;

  case 257:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8004 "Parser/parser.cc"
    break;

  case 258:
#line 1288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8010 "Parser/parser.cc"
    break;

  case 259:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8016 "Parser/parser.cc"
    break;

  case 260:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 261:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8028 "Parser/parser.cc"
    break;

  case 262:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8034 "Parser/parser.cc"
    break;

  case 263:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 264:
#line 1306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8046 "Parser/parser.cc"
    break;

  case 265:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8052 "Parser/parser.cc"
    break;

  case 266:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8058 "Parser/parser.cc"
    break;

  case 267:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8064 "Parser/parser.cc"
    break;

  case 268:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8070 "Parser/parser.cc"
    break;

  case 269:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8076 "Parser/parser.cc"
    break;

  case 270:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8082 "Parser/parser.cc"
    break;

  case 271:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8088 "Parser/parser.cc"
    break;

  case 272:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8094 "Parser/parser.cc"
    break;

  case 273:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8100 "Parser/parser.cc"
    break;

  case 274:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8106 "Parser/parser.cc"
    break;

  case 277:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 8114 "Parser/parser.cc"
    break;

  case 278:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Mutex statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8120 "Parser/parser.cc"
    break;

  case 279:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8126 "Parser/parser.cc"
    break;

  case 280:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8132 "Parser/parser.cc"
    break;

  case 282:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8138 "Parser/parser.cc"
    break;

  case 283:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8144 "Parser/parser.cc"
    break;

  case 285:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8150 "Parser/parser.cc"
    break;

  case 286:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8156 "Parser/parser.cc"
    break;

  case 287:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8162 "Parser/parser.cc"
    break;

  case 288:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8168 "Parser/parser.cc"
    break;

  case 289:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8174 "Parser/parser.cc"
    break;

  case 290:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8180 "Parser/parser.cc"
    break;

  case 291:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8186 "Parser/parser.cc"
    break;

  case 292:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8192 "Parser/parser.cc"
    break;

  case 293:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8198 "Parser/parser.cc"
    break;

  case 294:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8204 "Parser/parser.cc"
    break;

  case 295:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8210 "Parser/parser.cc"
    break;

  case 296:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8216 "Parser/parser.cc"
    break;

  case 297:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8222 "Parser/parser.cc"
    break;

  case 298:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8228 "Parser/parser.cc"
    break;

  case 299:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8234 "Parser/parser.cc"
    break;

  case 300:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8240 "Parser/parser.cc"
    break;

  case 301:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8246 "Parser/parser.cc"
    break;

  case 302:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8252 "Parser/parser.cc"
    break;

  case 303:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8258 "Parser/parser.cc"
    break;

  case 304:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8264 "Parser/parser.cc"
    break;

  case 305:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8270 "Parser/parser.cc"
    break;

  case 306:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8276 "Parser/parser.cc"
    break;

  case 308:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8282 "Parser/parser.cc"
    break;

  case 309:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8288 "Parser/parser.cc"
    break;

  case 310:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8294 "Parser/parser.cc"
    break;

  case 315:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8300 "Parser/parser.cc"
    break;

  case 316:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 317:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 318:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 319:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 320:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8330 "Parser/parser.cc"
    break;

  case 321:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8336 "Parser/parser.cc"
    break;

  case 322:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8342 "Parser/parser.cc"
    break;

  case 325:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8348 "Parser/parser.cc"
    break;

  case 326:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 327:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 328:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8366 "Parser/parser.cc"
    break;

  case 329:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 330:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8378 "Parser/parser.cc"
    break;

  case 331:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8387 "Parser/parser.cc"
    break;

  case 332:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8396 "Parser/parser.cc"
    break;

  case 333:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8402 "Parser/parser.cc"
    break;

  case 336:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8408 "Parser/parser.cc"
    break;

  case 337:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8414 "Parser/parser.cc"
    break;

  case 339:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8420 "Parser/parser.cc"
    break;

  case 340:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8426 "Parser/parser.cc"
    break;

  case 350:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 351:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8438 "Parser/parser.cc"
    break;

  case 355:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8444 "Parser/parser.cc"
    break;

  case 357:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8450 "Parser/parser.cc"
    break;

  case 358:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8456 "Parser/parser.cc"
    break;

  case 359:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8462 "Parser/parser.cc"
    break;

  case 360:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8468 "Parser/parser.cc"
    break;

  case 361:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 362:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8480 "Parser/parser.cc"
    break;

  case 364:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8486 "Parser/parser.cc"
    break;

  case 365:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8492 "Parser/parser.cc"
    break;

  case 366:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 367:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8509 "Parser/parser.cc"
    break;

  case 368:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8515 "Parser/parser.cc"
    break;

  case 369:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8521 "Parser/parser.cc"
    break;

  case 370:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8527 "Parser/parser.cc"
    break;

  case 371:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8533 "Parser/parser.cc"
    break;

  case 372:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8542 "Parser/parser.cc"
    break;

  case 373:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8551 "Parser/parser.cc"
    break;

  case 374:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8560 "Parser/parser.cc"
    break;

  case 375:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8569 "Parser/parser.cc"
    break;

  case 376:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8578 "Parser/parser.cc"
    break;

  case 377:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8587 "Parser/parser.cc"
    break;

  case 378:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8596 "Parser/parser.cc"
    break;

  case 379:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8605 "Parser/parser.cc"
    break;

  case 380:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8613 "Parser/parser.cc"
    break;

  case 381:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8621 "Parser/parser.cc"
    break;

  case 382:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8627 "Parser/parser.cc"
    break;

  case 386:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8633 "Parser/parser.cc"
    break;

  case 387:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8639 "Parser/parser.cc"
    break;

  case 400:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8645 "Parser/parser.cc"
    break;

  case 403:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8651 "Parser/parser.cc"
    break;

  case 406:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8657 "Parser/parser.cc"
    break;

  case 407:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8663 "Parser/parser.cc"
    break;

  case 408:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8669 "Parser/parser.cc"
    break;

  case 409:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8675 "Parser/parser.cc"
    break;

  case 411:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8681 "Parser/parser.cc"
    break;

  case 413:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8687 "Parser/parser.cc"
    break;

  case 414:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8693 "Parser/parser.cc"
    break;

  case 416:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 417:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8705 "Parser/parser.cc"
    break;

  case 418:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8711 "Parser/parser.cc"
    break;

  case 419:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8717 "Parser/parser.cc"
    break;

  case 420:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8723 "Parser/parser.cc"
    break;

  case 421:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8729 "Parser/parser.cc"
    break;

  case 422:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8735 "Parser/parser.cc"
    break;

  case 423:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8741 "Parser/parser.cc"
    break;

  case 424:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8747 "Parser/parser.cc"
    break;

  case 425:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8753 "Parser/parser.cc"
    break;

  case 426:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8759 "Parser/parser.cc"
    break;

  case 427:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8765 "Parser/parser.cc"
    break;

  case 428:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8771 "Parser/parser.cc"
    break;

  case 429:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8777 "Parser/parser.cc"
    break;

  case 430:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8783 "Parser/parser.cc"
    break;

  case 431:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8789 "Parser/parser.cc"
    break;

  case 432:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8795 "Parser/parser.cc"
    break;

  case 433:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8801 "Parser/parser.cc"
    break;

  case 434:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8807 "Parser/parser.cc"
    break;

  case 435:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8813 "Parser/parser.cc"
    break;

  case 436:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8819 "Parser/parser.cc"
    break;

  case 437:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8825 "Parser/parser.cc"
    break;

  case 438:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8831 "Parser/parser.cc"
    break;

  case 439:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8837 "Parser/parser.cc"
    break;

  case 440:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8843 "Parser/parser.cc"
    break;

  case 441:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8849 "Parser/parser.cc"
    break;

  case 442:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8855 "Parser/parser.cc"
    break;

  case 443:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8861 "Parser/parser.cc"
    break;

  case 444:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8867 "Parser/parser.cc"
    break;

  case 445:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8873 "Parser/parser.cc"
    break;

  case 446:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8879 "Parser/parser.cc"
    break;

  case 447:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8885 "Parser/parser.cc"
    break;

  case 448:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8891 "Parser/parser.cc"
    break;

  case 450:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8897 "Parser/parser.cc"
    break;

  case 452:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8903 "Parser/parser.cc"
    break;

  case 453:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8909 "Parser/parser.cc"
    break;

  case 454:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8915 "Parser/parser.cc"
    break;

  case 456:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8921 "Parser/parser.cc"
    break;

  case 457:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8927 "Parser/parser.cc"
    break;

  case 458:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8933 "Parser/parser.cc"
    break;

  case 459:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 8939 "Parser/parser.cc"
    break;

  case 461:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8945 "Parser/parser.cc"
    break;

  case 463:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8951 "Parser/parser.cc"
    break;

  case 464:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8957 "Parser/parser.cc"
    break;

  case 465:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 8963 "Parser/parser.cc"
    break;

  case 466:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 8969 "Parser/parser.cc"
    break;

  case 467:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 8975 "Parser/parser.cc"
    break;

  case 468:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 8981 "Parser/parser.cc"
    break;

  case 469:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 8987 "Parser/parser.cc"
    break;

  case 470:
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 8993 "Parser/parser.cc"
    break;

  case 471:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 8999 "Parser/parser.cc"
    break;

  case 473:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9005 "Parser/parser.cc"
    break;

  case 474:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9011 "Parser/parser.cc"
    break;

  case 475:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9017 "Parser/parser.cc"
    break;

  case 477:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9023 "Parser/parser.cc"
    break;

  case 478:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9029 "Parser/parser.cc"
    break;

  case 479:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9038 "Parser/parser.cc"
    break;

  case 481:
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9044 "Parser/parser.cc"
    break;

  case 482:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9050 "Parser/parser.cc"
    break;

  case 483:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9056 "Parser/parser.cc"
    break;

  case 485:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9062 "Parser/parser.cc"
    break;

  case 486:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9068 "Parser/parser.cc"
    break;

  case 488:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9074 "Parser/parser.cc"
    break;

  case 489:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9080 "Parser/parser.cc"
    break;

  case 490:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9086 "Parser/parser.cc"
    break;

  case 492:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9092 "Parser/parser.cc"
    break;

  case 493:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9098 "Parser/parser.cc"
    break;

  case 494:
#line 2032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9104 "Parser/parser.cc"
    break;

  case 495:
#line 2034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9110 "Parser/parser.cc"
    break;

  case 496:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9116 "Parser/parser.cc"
    break;

  case 498:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 499:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9128 "Parser/parser.cc"
    break;

  case 500:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9134 "Parser/parser.cc"
    break;

  case 501:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9140 "Parser/parser.cc"
    break;

  case 502:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9146 "Parser/parser.cc"
    break;

  case 507:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9152 "Parser/parser.cc"
    break;

  case 508:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 509:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9167 "Parser/parser.cc"
    break;

  case 510:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 511:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9182 "Parser/parser.cc"
    break;

  case 512:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9191 "Parser/parser.cc"
    break;

  case 513:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9200 "Parser/parser.cc"
    break;

  case 514:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9209 "Parser/parser.cc"
    break;

  case 516:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9215 "Parser/parser.cc"
    break;

  case 517:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9221 "Parser/parser.cc"
    break;

  case 518:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9231 "Parser/parser.cc"
    break;

  case 519:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9246 "Parser/parser.cc"
    break;

  case 522:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9252 "Parser/parser.cc"
    break;

  case 523:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9258 "Parser/parser.cc"
    break;

  case 524:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9264 "Parser/parser.cc"
    break;

  case 525:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9270 "Parser/parser.cc"
    break;

  case 526:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9276 "Parser/parser.cc"
    break;

  case 527:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9282 "Parser/parser.cc"
    break;

  case 528:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9288 "Parser/parser.cc"
    break;

  case 529:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9294 "Parser/parser.cc"
    break;

  case 530:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9300 "Parser/parser.cc"
    break;

  case 531:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9306 "Parser/parser.cc"
    break;

  case 532:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9312 "Parser/parser.cc"
    break;

  case 533:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9318 "Parser/parser.cc"
    break;

  case 534:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9324 "Parser/parser.cc"
    break;

  case 535:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9330 "Parser/parser.cc"
    break;

  case 536:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9336 "Parser/parser.cc"
    break;

  case 537:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9349 "Parser/parser.cc"
    break;

  case 538:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9355 "Parser/parser.cc"
    break;

  case 541:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9361 "Parser/parser.cc"
    break;

  case 542:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9367 "Parser/parser.cc"
    break;

  case 545:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9373 "Parser/parser.cc"
    break;

  case 547:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9379 "Parser/parser.cc"
    break;

  case 548:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 549:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 550:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9397 "Parser/parser.cc"
    break;

  case 551:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9403 "Parser/parser.cc"
    break;

  case 553:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 555:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 556:
#line 2229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 558:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 559:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9433 "Parser/parser.cc"
    break;

  case 561:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9439 "Parser/parser.cc"
    break;

  case 562:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 563:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 564:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 565:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 566:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9472 "Parser/parser.cc"
    break;

  case 567:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9481 "Parser/parser.cc"
    break;

  case 568:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9489 "Parser/parser.cc"
    break;

  case 569:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9499 "Parser/parser.cc"
    break;

  case 571:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 572:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9511 "Parser/parser.cc"
    break;

  case 573:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9517 "Parser/parser.cc"
    break;

  case 574:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9523 "Parser/parser.cc"
    break;

  case 575:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9529 "Parser/parser.cc"
    break;

  case 576:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9535 "Parser/parser.cc"
    break;

  case 577:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9541 "Parser/parser.cc"
    break;

  case 578:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9547 "Parser/parser.cc"
    break;

  case 579:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9553 "Parser/parser.cc"
    break;

  case 580:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9559 "Parser/parser.cc"
    break;

  case 583:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9565 "Parser/parser.cc"
    break;

  case 584:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9571 "Parser/parser.cc"
    break;

  case 585:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9577 "Parser/parser.cc"
    break;

  case 587:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9583 "Parser/parser.cc"
    break;

  case 588:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9589 "Parser/parser.cc"
    break;

  case 589:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9595 "Parser/parser.cc"
    break;

  case 591:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 592:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9607 "Parser/parser.cc"
    break;

  case 593:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9613 "Parser/parser.cc"
    break;

  case 595:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9619 "Parser/parser.cc"
    break;

  case 598:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 599:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 601:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 602:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 603:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9649 "Parser/parser.cc"
    break;

  case 608:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 610:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9661 "Parser/parser.cc"
    break;

  case 611:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9667 "Parser/parser.cc"
    break;

  case 612:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9673 "Parser/parser.cc"
    break;

  case 613:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9679 "Parser/parser.cc"
    break;

  case 614:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 615:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 621:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 624:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9703 "Parser/parser.cc"
    break;

  case 625:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9709 "Parser/parser.cc"
    break;

  case 626:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9715 "Parser/parser.cc"
    break;

  case 627:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 628:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9727 "Parser/parser.cc"
    break;

  case 629:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9733 "Parser/parser.cc"
    break;

  case 631:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 632:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 633:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9751 "Parser/parser.cc"
    break;

  case 635:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9757 "Parser/parser.cc"
    break;

  case 637:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9763 "Parser/parser.cc"
    break;

  case 638:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9769 "Parser/parser.cc"
    break;

  case 639:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9775 "Parser/parser.cc"
    break;

  case 640:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9781 "Parser/parser.cc"
    break;

  case 641:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9787 "Parser/parser.cc"
    break;

  case 642:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9793 "Parser/parser.cc"
    break;

  case 644:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9799 "Parser/parser.cc"
    break;

  case 645:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9805 "Parser/parser.cc"
    break;

  case 646:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9811 "Parser/parser.cc"
    break;

  case 647:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9822 "Parser/parser.cc"
    break;

  case 648:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9828 "Parser/parser.cc"
    break;

  case 649:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9834 "Parser/parser.cc"
    break;

  case 650:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9840 "Parser/parser.cc"
    break;

  case 651:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::ALtype, (yyvsp[-1].tok) );
		}
#line 9849 "Parser/parser.cc"
    break;

  case 652:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9855 "Parser/parser.cc"
    break;

  case 653:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9861 "Parser/parser.cc"
    break;

  case 654:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9867 "Parser/parser.cc"
    break;

  case 655:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9873 "Parser/parser.cc"
    break;

  case 656:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9879 "Parser/parser.cc"
    break;

  case 657:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9885 "Parser/parser.cc"
    break;

  case 658:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9891 "Parser/parser.cc"
    break;

  case 659:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9897 "Parser/parser.cc"
    break;

  case 660:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9903 "Parser/parser.cc"
    break;

  case 661:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9909 "Parser/parser.cc"
    break;

  case 664:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9915 "Parser/parser.cc"
    break;

  case 665:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9921 "Parser/parser.cc"
    break;

  case 666:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9927 "Parser/parser.cc"
    break;

  case 667:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 9933 "Parser/parser.cc"
    break;

  case 668:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, toString("Expression generic parameters are currently unimplemented: ", (yyvsp[0].en)->build()) ); (yyval.en) = nullptr; }
#line 9939 "Parser/parser.cc"
    break;

  case 669:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 9945 "Parser/parser.cc"
    break;

  case 670:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, toString("Expression generic parameters are currently unimplemented: ", (yyvsp[0].en)->build()) ); (yyval.en) = nullptr; }
#line 9951 "Parser/parser.cc"
    break;

  case 671:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9957 "Parser/parser.cc"
    break;

  case 672:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9963 "Parser/parser.cc"
    break;

  case 673:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 9969 "Parser/parser.cc"
    break;

  case 674:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 9975 "Parser/parser.cc"
    break;

  case 675:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 9981 "Parser/parser.cc"
    break;

  case 676:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 9990 "Parser/parser.cc"
    break;

  case 677:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 9999 "Parser/parser.cc"
    break;

  case 678:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10005 "Parser/parser.cc"
    break;

  case 679:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10011 "Parser/parser.cc"
    break;

  case 681:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10017 "Parser/parser.cc"
    break;

  case 686:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10023 "Parser/parser.cc"
    break;

  case 687:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10029 "Parser/parser.cc"
    break;

  case 688:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10035 "Parser/parser.cc"
    break;

  case 690:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10041 "Parser/parser.cc"
    break;

  case 691:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10047 "Parser/parser.cc"
    break;

  case 692:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10053 "Parser/parser.cc"
    break;

  case 693:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10059 "Parser/parser.cc"
    break;

  case 695:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10065 "Parser/parser.cc"
    break;

  case 696:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10071 "Parser/parser.cc"
    break;

  case 697:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 700:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10086 "Parser/parser.cc"
    break;

  case 701:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10092 "Parser/parser.cc"
    break;

  case 702:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10101 "Parser/parser.cc"
    break;

  case 703:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10111 "Parser/parser.cc"
    break;

  case 704:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10120 "Parser/parser.cc"
    break;

  case 705:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10130 "Parser/parser.cc"
    break;

  case 706:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10139 "Parser/parser.cc"
    break;

  case 707:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10149 "Parser/parser.cc"
    break;

  case 708:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10158 "Parser/parser.cc"
    break;

  case 709:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10168 "Parser/parser.cc"
    break;

  case 711:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10174 "Parser/parser.cc"
    break;

  case 712:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10180 "Parser/parser.cc"
    break;

  case 713:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10186 "Parser/parser.cc"
    break;

  case 714:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10192 "Parser/parser.cc"
    break;

  case 715:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10203 "Parser/parser.cc"
    break;

  case 716:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10212 "Parser/parser.cc"
    break;

  case 717:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10221 "Parser/parser.cc"
    break;

  case 718:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 719:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10233 "Parser/parser.cc"
    break;

  case 720:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 721:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10248 "Parser/parser.cc"
    break;

  case 722:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10254 "Parser/parser.cc"
    break;

  case 723:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10260 "Parser/parser.cc"
    break;

  case 724:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10266 "Parser/parser.cc"
    break;

  case 728:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10272 "Parser/parser.cc"
    break;

  case 729:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10278 "Parser/parser.cc"
    break;

  case 730:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10288 "Parser/parser.cc"
    break;

  case 731:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10294 "Parser/parser.cc"
    break;

  case 734:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10300 "Parser/parser.cc"
    break;

  case 735:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10306 "Parser/parser.cc"
    break;

  case 737:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10312 "Parser/parser.cc"
    break;

  case 738:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10318 "Parser/parser.cc"
    break;

  case 739:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 740:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 745:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10336 "Parser/parser.cc"
    break;

  case 746:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10342 "Parser/parser.cc"
    break;

  case 747:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 748:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10354 "Parser/parser.cc"
    break;

  case 749:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 751:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 752:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 753:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 754:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10384 "Parser/parser.cc"
    break;

  case 755:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 756:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10396 "Parser/parser.cc"
    break;

  case 757:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10402 "Parser/parser.cc"
    break;

  case 758:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 759:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10414 "Parser/parser.cc"
    break;

  case 760:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 761:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 762:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10432 "Parser/parser.cc"
    break;

  case 763:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 764:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10444 "Parser/parser.cc"
    break;

  case 765:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 766:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10456 "Parser/parser.cc"
    break;

  case 767:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 768:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 770:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 771:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 772:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10486 "Parser/parser.cc"
    break;

  case 773:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 774:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10498 "Parser/parser.cc"
    break;

  case 775:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 776:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 777:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 778:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 779:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 780:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 781:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10540 "Parser/parser.cc"
    break;

  case 782:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10546 "Parser/parser.cc"
    break;

  case 783:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10552 "Parser/parser.cc"
    break;

  case 784:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10558 "Parser/parser.cc"
    break;

  case 785:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10564 "Parser/parser.cc"
    break;

  case 789:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 790:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 791:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 792:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10588 "Parser/parser.cc"
    break;

  case 793:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 794:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10600 "Parser/parser.cc"
    break;

  case 795:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10606 "Parser/parser.cc"
    break;

  case 796:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10612 "Parser/parser.cc"
    break;

  case 797:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 798:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 799:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10630 "Parser/parser.cc"
    break;

  case 800:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 801:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10642 "Parser/parser.cc"
    break;

  case 802:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10648 "Parser/parser.cc"
    break;

  case 803:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 804:
#line 3079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10663 "Parser/parser.cc"
    break;

  case 805:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10669 "Parser/parser.cc"
    break;

  case 806:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10675 "Parser/parser.cc"
    break;

  case 808:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10681 "Parser/parser.cc"
    break;

  case 809:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10687 "Parser/parser.cc"
    break;

  case 810:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10693 "Parser/parser.cc"
    break;

  case 811:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10699 "Parser/parser.cc"
    break;

  case 812:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10705 "Parser/parser.cc"
    break;

  case 813:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10711 "Parser/parser.cc"
    break;

  case 814:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10717 "Parser/parser.cc"
    break;

  case 815:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 816:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 817:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 818:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 819:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10747 "Parser/parser.cc"
    break;

  case 820:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10753 "Parser/parser.cc"
    break;

  case 821:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10759 "Parser/parser.cc"
    break;

  case 822:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10765 "Parser/parser.cc"
    break;

  case 823:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10771 "Parser/parser.cc"
    break;

  case 824:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10777 "Parser/parser.cc"
    break;

  case 825:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10783 "Parser/parser.cc"
    break;

  case 826:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10789 "Parser/parser.cc"
    break;

  case 827:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10795 "Parser/parser.cc"
    break;

  case 829:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10801 "Parser/parser.cc"
    break;

  case 830:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10807 "Parser/parser.cc"
    break;

  case 831:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10813 "Parser/parser.cc"
    break;

  case 832:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10819 "Parser/parser.cc"
    break;

  case 833:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10825 "Parser/parser.cc"
    break;

  case 834:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10831 "Parser/parser.cc"
    break;

  case 835:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10837 "Parser/parser.cc"
    break;

  case 836:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 837:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10849 "Parser/parser.cc"
    break;

  case 838:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 839:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10861 "Parser/parser.cc"
    break;

  case 840:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10867 "Parser/parser.cc"
    break;

  case 841:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 842:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 844:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10885 "Parser/parser.cc"
    break;

  case 845:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10891 "Parser/parser.cc"
    break;

  case 846:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10897 "Parser/parser.cc"
    break;

  case 847:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10903 "Parser/parser.cc"
    break;

  case 848:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10909 "Parser/parser.cc"
    break;

  case 849:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10915 "Parser/parser.cc"
    break;

  case 850:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10921 "Parser/parser.cc"
    break;

  case 851:
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10927 "Parser/parser.cc"
    break;

  case 852:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10933 "Parser/parser.cc"
    break;

  case 853:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10939 "Parser/parser.cc"
    break;

  case 854:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10945 "Parser/parser.cc"
    break;

  case 856:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10951 "Parser/parser.cc"
    break;

  case 857:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10957 "Parser/parser.cc"
    break;

  case 858:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 10963 "Parser/parser.cc"
    break;

  case 859:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 860:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10975 "Parser/parser.cc"
    break;

  case 861:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 862:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 864:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10993 "Parser/parser.cc"
    break;

  case 865:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10999 "Parser/parser.cc"
    break;

  case 866:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11005 "Parser/parser.cc"
    break;

  case 867:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11011 "Parser/parser.cc"
    break;

  case 868:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11017 "Parser/parser.cc"
    break;

  case 869:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11023 "Parser/parser.cc"
    break;

  case 870:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11029 "Parser/parser.cc"
    break;

  case 871:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11035 "Parser/parser.cc"
    break;

  case 872:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11041 "Parser/parser.cc"
    break;

  case 874:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11047 "Parser/parser.cc"
    break;

  case 875:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11053 "Parser/parser.cc"
    break;

  case 876:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 877:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11065 "Parser/parser.cc"
    break;

  case 879:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11071 "Parser/parser.cc"
    break;

  case 880:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11077 "Parser/parser.cc"
    break;

  case 881:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11083 "Parser/parser.cc"
    break;

  case 882:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11089 "Parser/parser.cc"
    break;

  case 883:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11095 "Parser/parser.cc"
    break;

  case 884:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 885:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 886:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 888:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 889:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 890:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11131 "Parser/parser.cc"
    break;

  case 891:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11137 "Parser/parser.cc"
    break;

  case 892:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11143 "Parser/parser.cc"
    break;

  case 893:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11149 "Parser/parser.cc"
    break;

  case 895:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 897:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11161 "Parser/parser.cc"
    break;

  case 898:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 899:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11173 "Parser/parser.cc"
    break;

  case 900:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11179 "Parser/parser.cc"
    break;

  case 901:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11185 "Parser/parser.cc"
    break;

  case 902:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11191 "Parser/parser.cc"
    break;

  case 904:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 905:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 906:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 907:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11215 "Parser/parser.cc"
    break;

  case 908:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11221 "Parser/parser.cc"
    break;

  case 909:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 910:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 912:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 913:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 914:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11251 "Parser/parser.cc"
    break;

  case 915:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 916:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11263 "Parser/parser.cc"
    break;

  case 919:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 922:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 923:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 924:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 925:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11293 "Parser/parser.cc"
    break;

  case 926:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11299 "Parser/parser.cc"
    break;

  case 927:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11305 "Parser/parser.cc"
    break;

  case 928:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 929:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 930:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 931:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 932:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 933:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 934:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11347 "Parser/parser.cc"
    break;

  case 935:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 936:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 937:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 938:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 939:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11377 "Parser/parser.cc"
    break;

  case 940:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11383 "Parser/parser.cc"
    break;

  case 941:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11389 "Parser/parser.cc"
    break;

  case 943:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 947:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 948:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 949:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 950:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 951:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 952:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11431 "Parser/parser.cc"
    break;

  case 953:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 954:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 955:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 956:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 957:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 958:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 959:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 960:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11479 "Parser/parser.cc"
    break;

  case 961:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11485 "Parser/parser.cc"
    break;

  case 962:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11491 "Parser/parser.cc"
    break;

  case 963:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11497 "Parser/parser.cc"
    break;

  case 966:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11503 "Parser/parser.cc"
    break;

  case 967:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11509 "Parser/parser.cc"
    break;


#line 11513 "Parser/parser.cc"

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
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
