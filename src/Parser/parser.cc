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

#line 626 "Parser/parser.cc"

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
#define YYFINAL  143
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   18298

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  286
/* YYNRULES -- Number of rules.  */
#define YYNRULES  971
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1977

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
       0,   533,   533,   537,   544,   545,   546,   547,   548,   552,
     553,   554,   555,   556,   557,   558,   562,   563,   564,   569,
     573,   574,   585,   587,   589,   593,   594,   596,   598,   600,
     602,   615,   616,   626,   631,   636,   637,   643,   649,   655,
     657,   659,   661,   663,   665,   667,   669,   671,   673,   675,
     677,   679,   681,   683,   685,   687,   697,   698,   699,   704,
     707,   711,   712,   716,   717,   719,   721,   723,   725,   727,
     732,   734,   736,   744,   745,   753,   756,   757,   759,   764,
     780,   782,   784,   786,   788,   790,   792,   794,   796,   804,
     805,   807,   811,   812,   813,   814,   818,   819,   821,   823,
     825,   827,   829,   831,   833,   840,   841,   842,   843,   847,
     848,   852,   853,   858,   859,   861,   863,   868,   869,   871,
     876,   877,   879,   884,   885,   887,   889,   891,   896,   897,
     899,   904,   905,   910,   911,   916,   917,   922,   923,   928,
     929,   934,   935,   938,   943,   948,   949,   957,   963,   964,
     968,   969,   973,   974,   978,   979,   980,   981,   982,   983,
     984,   985,   986,   987,   988,   998,  1000,  1005,  1006,  1008,
    1010,  1015,  1016,  1022,  1023,  1029,  1030,  1031,  1032,  1033,
    1034,  1035,  1036,  1037,  1038,  1039,  1041,  1042,  1048,  1053,
    1055,  1063,  1064,  1069,  1071,  1073,  1075,  1077,  1081,  1082,
    1087,  1094,  1096,  1098,  1108,  1110,  1118,  1121,  1126,  1128,
    1130,  1132,  1140,  1141,  1143,  1147,  1149,  1153,  1154,  1165,
    1166,  1170,  1175,  1176,  1180,  1182,  1187,  1189,  1191,  1193,
    1195,  1197,  1202,  1203,  1225,  1227,  1229,  1232,  1235,  1238,
    1240,  1242,  1244,  1247,  1250,  1252,  1255,  1262,  1264,  1266,
    1268,  1270,  1275,  1277,  1279,  1281,  1286,  1288,  1293,  1295,
    1297,  1299,  1302,  1306,  1309,  1313,  1315,  1317,  1319,  1321,
    1323,  1325,  1327,  1329,  1331,  1333,  1338,  1339,  1343,  1351,
    1356,  1361,  1362,  1366,  1370,  1375,  1376,  1382,  1386,  1388,
    1390,  1392,  1395,  1397,  1402,  1404,  1409,  1411,  1413,  1418,
    1420,  1426,  1427,  1431,  1432,  1433,  1434,  1438,  1443,  1444,
    1446,  1448,  1450,  1454,  1458,  1459,  1463,  1465,  1467,  1469,
    1471,  1477,  1478,  1484,  1485,  1489,  1490,  1495,  1497,  1503,
    1504,  1506,  1511,  1516,  1527,  1528,  1532,  1533,  1539,  1540,
    1544,  1546,  1550,  1552,  1556,  1557,  1561,  1562,  1566,  1567,
    1568,  1572,  1574,  1589,  1590,  1591,  1592,  1594,  1598,  1600,
    1604,  1611,  1613,  1615,  1620,  1621,  1623,  1625,  1627,  1659,
    1662,  1667,  1669,  1675,  1680,  1685,  1696,  1701,  1706,  1711,
    1716,  1725,  1729,  1736,  1738,  1739,  1740,  1746,  1748,  1753,
    1754,  1755,  1764,  1765,  1766,  1770,  1771,  1772,  1781,  1782,
    1783,  1788,  1789,  1798,  1799,  1804,  1805,  1809,  1811,  1813,
    1815,  1817,  1821,  1826,  1827,  1829,  1839,  1840,  1845,  1847,
    1849,  1851,  1853,  1856,  1858,  1860,  1865,  1867,  1869,  1871,
    1873,  1875,  1877,  1879,  1881,  1883,  1885,  1887,  1889,  1891,
    1893,  1895,  1897,  1899,  1901,  1903,  1905,  1907,  1909,  1911,
    1913,  1915,  1917,  1919,  1924,  1925,  1929,  1935,  1936,  1942,
    1943,  1945,  1947,  1949,  1954,  1956,  1961,  1962,  1964,  1966,
    1971,  1973,  1975,  1977,  1979,  1981,  1986,  1987,  1989,  1991,
    1996,  1998,  1997,  2001,  2009,  2010,  2012,  2014,  2019,  2020,
    2022,  2027,  2028,  2030,  2032,  2037,  2038,  2040,  2045,  2047,
    2049,  2051,  2052,  2054,  2059,  2061,  2063,  2068,  2069,  2073,
    2074,  2079,  2078,  2083,  2082,  2090,  2089,  2100,  2099,  2109,
    2114,  2115,  2120,  2126,  2140,  2141,  2145,  2147,  2149,  2155,
    2157,  2159,  2161,  2163,  2165,  2167,  2169,  2175,  2176,  2181,
    2183,  2185,  2194,  2196,  2197,  2198,  2200,  2202,  2203,  2208,
    2209,  2210,  2215,  2217,  2220,  2227,  2228,  2229,  2235,  2240,
    2242,  2248,  2249,  2255,  2256,  2260,  2265,  2268,  2267,  2271,
    2274,  2280,  2279,  2288,  2294,  2298,  2300,  2305,  2307,  2309,
    2311,  2317,  2320,  2326,  2327,  2329,  2330,  2331,  2333,  2335,
    2342,  2343,  2345,  2347,  2352,  2353,  2359,  2360,  2362,  2363,
    2368,  2369,  2370,  2372,  2380,  2381,  2383,  2386,  2388,  2392,
    2393,  2394,  2396,  2398,  2403,  2405,  2410,  2412,  2421,  2423,
    2428,  2429,  2430,  2434,  2435,  2436,  2441,  2442,  2447,  2448,
    2449,  2453,  2454,  2459,  2460,  2461,  2462,  2463,  2478,  2479,
    2484,  2485,  2491,  2493,  2496,  2498,  2500,  2523,  2524,  2530,
    2531,  2537,  2536,  2546,  2545,  2549,  2555,  2561,  2562,  2564,
    2566,  2571,  2573,  2575,  2577,  2583,  2584,  2588,  2589,  2594,
    2596,  2603,  2605,  2606,  2608,  2613,  2615,  2617,  2622,  2624,
    2629,  2634,  2642,  2644,  2649,  2650,  2655,  2656,  2660,  2661,
    2662,  2667,  2669,  2675,  2677,  2682,  2684,  2690,  2691,  2695,
    2699,  2703,  2705,  2706,  2707,  2712,  2715,  2714,  2726,  2725,
    2737,  2736,  2748,  2747,  2761,  2767,  2769,  2775,  2776,  2781,
    2788,  2793,  2799,  2802,  2805,  2809,  2815,  2818,  2821,  2826,
    2827,  2828,  2832,  2838,  2839,  2849,  2850,  2854,  2855,  2860,
    2865,  2866,  2872,  2873,  2875,  2880,  2881,  2882,  2883,  2884,
    2886,  2921,  2923,  2928,  2930,  2931,  2933,  2938,  2940,  2942,
    2944,  2949,  2951,  2953,  2955,  2957,  2959,  2961,  2966,  2968,
    2970,  2972,  2981,  2983,  2984,  2989,  2991,  2993,  2995,  2997,
    3002,  3004,  3006,  3008,  3013,  3015,  3017,  3019,  3021,  3023,
    3035,  3036,  3037,  3041,  3043,  3045,  3047,  3049,  3054,  3056,
    3058,  3060,  3065,  3067,  3069,  3071,  3073,  3075,  3090,  3095,
    3100,  3102,  3103,  3105,  3110,  3112,  3114,  3116,  3121,  3123,
    3125,  3127,  3129,  3131,  3133,  3138,  3140,  3142,  3144,  3146,
    3156,  3158,  3160,  3161,  3163,  3168,  3170,  3172,  3177,  3179,
    3181,  3183,  3188,  3190,  3192,  3206,  3208,  3210,  3211,  3213,
    3218,  3220,  3225,  3227,  3229,  3234,  3236,  3241,  3243,  3260,
    3261,  3263,  3268,  3270,  3272,  3274,  3276,  3281,  3282,  3284,
    3286,  3291,  3293,  3295,  3301,  3303,  3305,  3308,  3312,  3314,
    3316,  3318,  3352,  3353,  3355,  3357,  3362,  3364,  3366,  3368,
    3370,  3375,  3376,  3378,  3380,  3385,  3387,  3389,  3395,  3396,
    3398,  3407,  3410,  3412,  3415,  3417,  3419,  3433,  3434,  3436,
    3441,  3443,  3445,  3447,  3449,  3454,  3455,  3457,  3459,  3464,
    3466,  3474,  3475,  3476,  3481,  3482,  3487,  3489,  3491,  3493,
    3495,  3497,  3504,  3506,  3508,  3510,  3512,  3515,  3517,  3519,
    3521,  3523,  3528,  3530,  3532,  3537,  3563,  3564,  3566,  3570,
    3571,  3575,  3577,  3579,  3581,  3583,  3585,  3592,  3594,  3596,
    3598,  3600,  3602,  3607,  3609,  3611,  3618,  3620,  3638,  3640,
    3645,  3646
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
     395,   396,   397,   398,   399,   400,   401,   402,   125,    40,
      64,    41,    46,    91,    93,    44,    58,   123,    96,    94,
      42,    38,    43,    45,    33,   126,    92,    47,    37,    60,
      62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1632)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-852)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      84, 11075,   120,   238, 15300,    49, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632,   132,   783,   173,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632,    82,   395, -1632,
   -1632, -1632, -1632, -1632, -1632,  5124,  5124,   210, 11075,   389,
     440, -1632, -1632,   471, -1632, -1632, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632,  3458, -1632,   455,   480, -1632, -1632, -1632,
   -1632, 15150, -1632, -1632,   494,   524,    -7,   315, -1632,  5124,
     524,   524,   524,   525,  4836,   705,   772, 11234, -1632, -1632,
   -1632, 15000,   946, -1632, -1632, -1632,  1529,   708, 11286,   822,
     657,  1529,   700,   569, -1632, -1632, -1632, -1632,   669, -1632,
   -1632, -1632, -1632,   609, -1632, -1632, -1632, -1632, -1632,   575,
     592,   669, -1632,   669,   631, -1632, -1632, -1632, 15652,  5124,
   -1632, -1632,  5124, -1632, 11075,   612, 15704, -1632, -1632,  4898,
    9464, -1632,   628,   628, -1632,  2386, -1632, -1632, -1632, -1632,
   14538, 13610,  3573,   669, -1632, -1632, -1632, -1632, -1632, -1632,
     641, -1632,   627,   656,   665, -1632,   735, 17773, 14230,  4317,
    3458,    25,   715,   744,   750,   757,   768,   770, -1632, -1632,
   15854, 10426,   706, -1632,  9062, -1632, -1632, -1632, -1632,   775,
   -1632, -1632,   771, -1632,   921, 17197, -1632,   798,  5124,   592,
     812,   813,   815,   840, -1632, -1632, -1632,  3141,  3796,   845,
     932,     4, -1632, -1632,   669,   669,    38,   110,   116,    38,
   -1632,   669,   669, -1632,  3916, -1632, -1632,   881,   901,   628,
   13188, -1632, -1632, 15150, -1632, -1632,  1529, -1632,  1015,   569,
     922,  1000,   110,  5124,    -7, -1632, 12481, -1632,   628,   628,
     928,  1000,   110,  5124, -1632,  9805, -1632, -1632,   628, -1632,
     628, -1632,   654,  4945,  5124, -1632,  1967,   890, -1632, -1632,
   -1632,  7465,   592,   223, -1632, -1632, 16664, -1632,   932,   340,
   -1632, 17773,  9464,  3188,  3916, -1632,   198, -1632, -1632, -1632,
   15704,  5124,   933, -1632, -1632, -1632, -1632,  5124,  3671,   436,
     443, -1632,  5124,   627, -1632, 17845,   944,   952, 17773, 17917,
     955, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, 17989,
   17989, 14076,   865,  3987, -1632, -1632, -1632, -1632,   969, -1632,
     971,  1009, -1632,   531,  4348, 14538, 17773, -1632,   973,   587,
     814,   924,   459,   938,   983,  1008,  1002,  1046,   344, -1632,
   -1632, -1632,   608,  1030, -1632, -1632,   511, -1632, -1632,   669,
    1032, 15906,   580, 13768, 13346,  1529,  1529, -1632,  1529,   628,
    1529,   628, -1632, -1632,   669, -1632,  1037, -1632, 16056, -1632,
   -1632, -1632, 16108,   775, -1632,  1035,   130,  2221,  1071,   569,
    1077, -1632,  2386,  1021,   627,  2386,  2072,  1048,  1052, -1632,
   17773, -1632,   621,  1030, -1632,   689,  4317,  1066,  1088,  1093,
    1095,  1099,  1105, -1632, -1632,   235,  1123, -1632,   718,  1123,
   -1632, -1632, 15652, -1632,  1005,  1126, 14692, -1632, -1632,  4977,
    4623,  1152, 13768,  1161,   155,   508, -1632, -1632, -1632, -1632,
   -1632,  5124,  5024, -1632, -1632, -1632, -1632, -1632, -1632, -1632,
    6122, -1632, -1632,  7787,  1141, -1632, -1632, -1632, -1632, -1632,
    3141,   662,  1156,  1158,  1166,   671,  1169,  1187,  1193,  3796,
   -1632, -1632,   669,  1163,    -7,  1142, -1632, -1632,  1173, -1632,
   -1632,   592,  1000, -1632, -1632, -1632,   592, -1632, -1632,  3916,
   -1632, 14538, 14538, -1632,   628,  4898,  9251, 13610, -1632, -1632,
   -1632, -1632, -1632,   592,  1000,   340, -1632, -1632,  1529,  1190,
    1000,   110, -1632,   592,  1000, -1632, 12373, -1632,   628,   628,
   -1632, -1632,  1195,   499,  1197,   569,  1199, -1632, 16716, -1632,
     755, -1632,  1302, 17182, -1632,  4898, 16267, 13188, -1632,  7465,
   18061, -1632, -1632, -1632, -1632, -1632,  3188,   739,  3916, -1632,
   13610,   932, -1632,  1219, -1632,  1230, -1632, -1632, -1632, -1632,
   -1632,  2386, -1632, -1632, 14384, -1632, 16319, 16319, -1632, 14384,
   -1632, 17773, 14384, -1632, -1632, 15450, 16319, 16319,   865,   861,
     999,   414,  1189, -1632,   760,  1238,  1004,  1239, -1632,  7787,
   10426, 17269,  1224,  1967,  1967, -1632, -1632,  1160, -1632, -1632,
   17341,  2666, 17773, 17341,  1967, -1632, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632,  1231, 17773, -1632, -1632,
   -1632, -1632, 17773, 17773, 17773, 17773, 17773, 17773, 17773, 17773,
   17773, 17773, 17773, 17773, 17773, 17773, 17773, 17773, 17773, 17773,
   17773, 17413,   608,   788, -1632, -1632,   669,   669, -1632,  3963,
    1320, 14538,  4959, 16108, 10426, -1632, 16371, -1632,   628,   628,
   -1632, -1632,   775, -1632,   633,  1243,  1385, 17773,  1350,  1173,
    1232, -1632,   669,   669, -1632,  1123, -1632, 15906, -1632, -1632,
   16997,   628,   628, -1632,  4959,   669, -1632,  8635, -1632, -1632,
   16056, -1632,   356,  1248,   189,  1249,  2221,   765, 15704,   778,
   -1632, -1632, -1632, -1632, -1632, -1632,   781, -1632,  1266,  1244,
   17773,  1245,   735, -1632, -1632,   274,  1123, -1632,   786,  1123,
   -1632, -1632, -1632,  1173, -1632, -1632,  1173, 18133, -1632, -1632,
   10426,  1267,  1268,  2402,  1407,  3391,   276,  1232, -1632,   669,
     669,  1232,   284, -1632,   669,   669, 17773,  5124,  1025,  1036,
    1232,     9, 13136, 13136,  5124,  1269,  4728,  1052,  1270,  1275,
   -1632,  1278, 17197,   704, -1632, -1632, -1632,   789, -1632, 13136,
    1967,  4898,  1967,   809,  1292,  1294,  1295,   810,  1297,  1301,
    1303,   328,  1123, -1632, -1632,   337,  1123, -1632, -1632, -1632,
    4898,   735, -1632,  1123, 18133, -1632,   592, 16716, -1632, -1632,
     816,  1304,   832,  1307, -1632,  1305, -1632,   592, -1632, -1632,
     592,  1000,  1305, -1632,   592,  1306,  1309,  1312, -1632, -1632,
   16997, -1632,  1308, -1632, -1632, -1632,  1967,  5124,  9875,  1394,
    1290, 17084, -1632,  1126, -1632, 13136,   835, -1632,  1305, -1632,
   15704, 14538,  1298, -1632,  1298,  1321,   480,  1318,  1325,  1329,
    1327,  1333, 17773,  1337,  1338,  1339, 10426, 17773, -1632, -1632,
    1424, -1632, -1632, -1632, 17773, -1632,  1340,  1342,  8836,  1042,
   -1632, 17341, -1632, -1632, -1632,  3504, -1632, -1632,   871, -1632,
   -1632, -1632,  3504, -1632, -1632,  1055,   468, -1632,  6122, -1632,
   -1632,   973,   973,   973,   587,   587,   814,   814,   924,   924,
     924,   924,   459,   459,   938,   983,  1008,  1002,  1046, 17773,
    1060, 16716,  1345,  1346,  1349,   788, -1632, -1632, -1632,  3504,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632, 16056, -1632, 10588,
   14846, -1632, 16716,  1351,  1354,  1355, -1632,  5505,   669, -1632,
    1350, -1632, -1632, -1632, -1632,  1173, -1632, -1632, -1632,   628,
   -1632,  3897, -1632, -1632,   569,  2366,  1359, -1632, 17197, -1632,
    2221,  1248, -1632, -1632,  1352,  1357,  2072, 17341, -1632, -1632,
   -1632,  1361, -1632, -1632, -1632,  1173, -1632, -1632,  1173, 16716,
   16716, -1632, -1632,  2402,   745,  1365,  1366,  1368,  1371,  2861,
    3391, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632,  1370, -1632,  1232, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632,  1374,  1375, -1632,    -7,
   -1632, -1632, 17773, -1632,  6122,  1393, -1632,   950, -1632, -1632,
   -1632, -1632, 17485, 13136, -1632, -1632, -1632,  1369,   352,  1123,
   -1632,   365,  1123, -1632, -1632, -1632, -1632,  1173, -1632, -1632,
   -1632,  1173,   932,  1395,  1173, -1632, -1632, -1632, -1632, -1632,
   -1632, -1632,  1396, -1632, -1632,  1305, -1632,   592, -1632, -1632,
   -1632, -1632, -1632, 11705,  1397,  1390, -1632,   294, -1632,   386,
     103, 10264,  1401, 12965,  1402,  1403,  2252,  3281,  3540, 17557,
    1404, -1632, -1632,  1406,  1410, -1632, -1632,   592, 17773, 17773,
    1547,  1405,   476, -1632,  1490,  1408,  1391, -1632, -1632, -1632,
    9626, -1632, -1632, -1632, -1632, -1632,  2469, -1632, -1632, -1632,
    1475, -1632, -1632, -1632,  1967, -1632, -1632, 11552, 15150,  1412,
   -1632,  5124, -1632,  1398,  1416,  1419, -1632,  1065, -1632, -1632,
   -1632,  4898, -1632, -1632,  1400,  1409,   880, 15704,   627,   627,
     500, 10426,  1967, -1632,   500, 15502,   500, -1632, 17773, 17773,
   17773, -1632, -1632, -1632, -1632, 17773, 17773,  1418,  6122, -1632,
   -1632,  1070,   549, -1632,  2130, -1632, -1632,  1072, -1632,   174,
   -1632, 17341,  1083, -1632,  7787, -1632,  1278, -1632, 17773, -1632,
     394,  1123, -1632, -1632,  1087, -1632, -1632,  1052,  1126, 14692,
   -1632,  1030, -1632, 10750, -1632,   401,  1123, -1632,   628,  6566,
   -1632, -1632,  2221,   669,   669,   356,   189, -1632, -1632,  1248,
    1429,  1432, -1632, -1632,   886,  1426,  1411, 16716, 16716, -1632,
   -1632,  1431,   422,  1123, -1632,   430,  2331,   669,   669, -1632,
   -1632, 16716, 16716, -1632,  1433, -1632, 13610, 13610,  1438,  1436,
    1437,  1442, -1632,  3504,  1094,   275, -1632, -1632, -1632, 17197,
   -1632, 17773, -1632, -1632, -1632,  1444, 17773, -1632, -1632, -1632,
    1173, -1632, -1632, -1632,  1173, 16716, 16716,    -7,   669,  1100,
    1445,  1449, -1632, -1632,  1450, 11858, 12011, 12164, 15704, 16319,
   16319,  1451, -1632,  1425,  1427,  2591,  5944, -1632,   372,  5124,
   -1632, -1632,  5124, -1632, 17341,   229,   355, -1632, -1632, -1632,
   -1632, 17773,  1454,  1526,  1455,  1458, -1632,  1440, -1632,  1446,
   17773,  1447,  6122,  1452, 17773,  7787, 17773,   895, -1632,  1456,
      85, -1632,   192,  1467, -1632, -1632,  1457, -1632,  1462, -1632,
    1463,  1469, 12965,    48, 12762,   669,   427, -1632, -1632, -1632,
    1468, -1632,  1476, -1632,  1483, -1632,  1477, -1632,  1484, -1632,
   -1632, -1632, -1632,  1472,  1485,   888, -1632,  1487, -1632, -1632,
   -1632, -1632, -1632,  6122,  1278,  7787, -1632,  1522,  3504, -1632,
    1522,  1522, -1632,  3504,  4201,  4559, -1632, -1632,  1104,  1501,
   -1632,  1499, -1632, -1632, -1632,  1173, -1632, 10912,  1497,  1498,
    1502, -1632,  1504, -1632, -1632, -1632,  1173, 17773, 17773,  1126,
    1503, -1632,  1248, -1632,  1496,   435, -1632,  1510, -1632, -1632,
   15704, -1632, -1632, -1632, -1632,  1509,  1511,   669, -1632, -1632,
   -1632,  1173, -1632, -1632, -1632,  1513, -1632, -1632, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632,  1520,
   17773, 17773,  1120,  1524, -1632, -1632,  1528,   669, -1632, 16716,
   16716, -1632, -1632, -1632, -1632, 17773, -1632, -1632,  1532, -1632,
    1451,  1451,  1451,   785,  1508,   444, -1632,  4652,   481, 14538,
   -1632, -1632, -1632,  3810, 17773,  4111,   551, -1632, -1632,    35,
    1527,  1527,  5124, -1632, -1632, 16865, -1632,   889, -1632, -1632,
   -1632, -1632,   898,  1540, 12965, 10264, 12965, 10092, -1632, -1632,
     562, -1632,  1278, -1632,   913,   951,   956, -1632, -1632, -1632,
   -1632,   592,   895,  1543, -1632, -1632, 17773, -1632,  1546,   735,
   10264, -1632, -1632, -1632, -1632, 17773,  1586, -1632, 12965, -1632,
     669, 13610, -1632, -1632, 15704, -1632, -1632, -1632, 17773, -1632,
   15502, 17773,  1278,  1548,  1124, -1632,  1131, -1632,  3504, -1632,
    3504, -1632, -1632, -1632, -1632, 16716, -1632, -1632, -1632,  1550,
   -1632, 16716, -1632, -1632,  1555, -1632,  1558,  1552,  1541,  2221,
   -1632, -1632, -1632,  1556,  1563, -1632, -1632, 16716, 16716,  1564,
    1566,  1140, 13294, 13452,  1569, -1632, -1632, -1632, -1632,  1573,
   -1632, -1632, -1632, -1632,  1577,  1579,  1145, -1632, -1632, -1632,
   -1632,   785,  1516,   564, -1632, -1632, -1632, -1632,   669,   669,
   -1632, -1632, -1632,   577, -1632,   958,  3810,   624, -1632,  4111,
     669, -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632, 12965,
     298, 17629, -1632,  1408,  1581, 17773,   494,  1565,   525, 12323,
   15704, -1632, 17773, 17773,   520,   526, -1632, 17773, -1632,  1585,
     404, 12965, -1632, -1632,  1587, -1632, -1632,  1560,   735,   535,
    1584,  1588,  1147,  1649, -1632, -1632, -1632,  5124,  4898, -1632,
   -1632, -1632,  1593, -1632, -1632, -1632,  1175,  1179, -1632, -1632,
   -1632,  1595,  1596, -1632, -1632, -1632,  2221,  1248,  1599, -1632,
   -1632, -1632, -1632, -1632, -1632, -1632, -1632, -1632,  1600, -1632,
   -1632,  1602,  1603,  1606, -1632, -1632, -1632,  1609,  1610,  1611,
    1516, -1632,   669, -1632, -1632, -1632, -1632, -1632,  1615,  4652,
   -1632, 17773,  1613, -1632, -1632, 12591, -1632,  1597,   961, 12965,
    1408, 13926,  1408,  1601, -1632, -1632, -1632, -1632,  4461, 17773,
   12965, 10092,  1604,  1607, -1632, -1632, -1632, -1632, 16521, -1632,
    1614,  1608,   350, 12965, -1632, 17773, 17341,   467, -1632, -1632,
   -1632, -1632, -1632, -1632,  1622,  1623, -1632, -1632,  1248,  1628,
   -1632,  1626,  1632, 13610,  1629, -1632, -1632, -1632,   458,  1123,
   -1632, -1632,   785, -1632,   332, -1632,  1159, -1632, -1632,  8062,
   -1632, -1632, -1632,  1612, -1632, 17773,  1636, 17773,   836,  1616,
     407, -1632, -1632, 17773, -1632,  8062, 16521, -1632,  4809, 16371,
    1967,  1633, -1632,  1691,  1646,   594,  1644, -1632,  1727, -1632,
     966, 12965,  1655, 12965, 12965, -1632, -1632,  1657, -1632, -1632,
   -1632, -1632, -1632, -1632, -1632,  1173, -1632, 17773, 17773, -1632,
    1260, 11393, -1632, -1632, -1632, -1632,  1408,  1658,  1659, 17773,
   17773, 17773, -1632, -1632,  1260, -1632,  1637,  2716,  3560, -1632,
   -1632, -1632,   350,  1656, 17773,  1640,   350,   350, 12965, -1632,
   -1632, 17773,  1714,  1716, -1632, 16716, -1632, -1632, 12591, -1632,
    1260, -1632,  1661,  1665,   457, -1632,  1408, -1632,  1637, 17773,
    1680,  3560,  1676,   735,  1684, -1632,   615, -1632, -1632,   975,
    1649,   438, -1632, -1632, 12847,  1689, 12591, 17773, 17701, 17773,
    1690,  1688, -1632,   592,   735,  1692, -1632,  1671,   735, -1632,
   -1632, 12965,  1773,  1696, -1632, -1632, 12847,  1408, -1632,  1408,
    1408, -1632,   592, -1632, -1632,  1186, 17773, -1632,   990, -1632,
   12965, -1632, -1632,   735,  1967,  1697,  1677, -1632, -1632, -1632,
     998, -1632, -1632,  1678,  1967, -1632, -1632
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   401,     0,     2,   401,   418,   419,   420,   421,   422,
     423,   424,   425,   407,   409,   408,   410,     0,     0,     0,
     426,   428,   449,   429,   450,   432,   433,   447,   448,   427,
     445,   446,   430,   431,   434,   435,   436,   437,   438,   439,
     440,   441,   442,   443,   444,   451,   452,   735,   454,   527,
     528,   531,   533,   529,   535,     0,     0,     0,   401,     0,
       0,    16,   498,   504,     9,    10,    11,    12,    13,    14,
      15,   701,    91,     0,    18,     0,     2,    89,    90,    17,
     751,   401,   702,   350,     0,   353,   628,   355,   364,     0,
     354,   384,   385,     0,     0,     0,     0,   481,   403,   405,
     411,   401,   413,   416,   466,   453,   389,   459,   464,   390,
     476,   391,   491,   495,   501,   480,   507,   519,   735,   524,
     525,   508,   574,   356,   357,     3,   703,   714,   406,     0,
       0,   735,   773,   735,     2,   790,   791,   792,   401,     0,
     949,   950,     0,     1,   401,     0,   401,   373,   374,     0,
     481,   395,   396,   397,   706,     0,   530,   532,   534,   536,
     401,   401,     0,   736,   737,   526,   455,   621,   622,   620,
     680,   675,   665,     0,     0,   704,     0,     0,   401,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   499,   502,
     401,   401,     0,   951,   481,   780,   798,   955,   948,   946,
     953,   349,     0,   153,   152,     0,   358,     0,     0,     0,
       0,     0,     0,     0,   348,   850,   851,     0,     0,   383,
     733,   735,   729,   754,   735,   735,   731,     2,   735,   730,
     811,   735,   735,   808,     0,   474,   475,     0,     0,   401,
     401,   418,     2,   401,   365,   404,   414,   467,     0,   496,
       0,   717,     2,     0,   628,   366,   481,   460,   477,   492,
       0,   717,     2,     0,   417,   461,   468,   469,   478,   483,
     493,   497,     0,   511,     0,   695,     2,     2,   715,   772,
     774,   401,     0,     2,     2,   959,   481,   962,   733,   733,
       3,     0,   481,     0,     0,   376,   735,   731,   730,     2,
     401,     0,     0,   661,   663,   662,   664,     0,     0,   657,
       0,   647,     0,   656,   667,     0,     0,     0,     0,     0,
       0,    22,    24,     4,     8,    20,     5,     6,     7,     0,
       0,   401,     2,     0,    92,    93,    94,    95,    76,    23,
      77,    19,    35,    75,    96,   401,     0,   111,   113,   117,
     120,   123,   128,   131,   133,   135,   137,   139,   141,   145,
     672,    25,   624,   495,   626,   671,     0,   623,   627,   735,
       2,   401,   970,   402,   401,   413,   392,   459,   393,   484,
     394,   491,   488,   509,   735,   510,     0,   609,   401,   610,
     924,   925,   401,   611,   613,   498,   504,     0,   575,   576,
       0,   738,     0,   678,   666,     0,   742,     0,     2,    96,
       0,   144,     0,     0,   505,     0,     0,     0,     0,     0,
       0,     0,     0,   752,   778,   735,   788,   796,   800,   806,
       2,   957,   401,   960,     2,    89,   401,     3,   608,     0,
     970,     0,   402,   459,   484,   491,     3,     3,   590,   594,
     604,   610,   611,     2,   781,   799,   947,     2,     2,   630,
     633,   631,   629,     0,     0,   719,     2,     2,     2,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     757,   814,   735,     0,   628,     2,   753,   761,   877,   755,
     756,     0,   717,     2,   810,   818,     0,   812,   813,     0,
     379,   401,   401,   465,   402,     0,   481,   401,   952,   956,
     954,   482,   699,     0,   717,   733,   359,   367,   415,     0,
     717,     2,   699,     0,   717,   676,   462,   463,   479,   494,
     500,   503,   498,   504,   522,   523,     0,   677,   401,   618,
       0,   189,   342,   401,     3,     0,   481,   401,   716,   401,
       0,   361,     2,   362,   696,   381,     0,     0,     0,     2,
     401,   733,   699,     0,     2,     0,   660,   659,   658,   653,
     412,     0,   651,   668,   401,    83,   401,   401,    78,   401,
      85,     0,   401,    81,    82,   401,   401,   401,     2,    92,
      93,     0,     0,   171,     0,     0,   525,     0,   946,     0,
     401,     0,     0,     0,     0,    21,    45,     0,    51,    52,
      56,     0,     0,    56,     0,   154,   155,   156,   157,   158,
     159,   160,   161,   162,   163,   164,   152,     0,   150,   151,
      79,    80,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     2,   862,   625,   859,   735,   735,   867,     0,
     457,   401,     0,   401,   401,   926,   402,   398,   399,   400,
     930,   921,   922,   928,     2,     2,    90,     0,   886,   900,
     970,   882,   735,   735,   891,   898,   616,   401,   489,   612,
     402,   485,   486,   490,     0,   735,   936,   402,   941,   933,
     401,   938,     0,   968,   581,     0,     0,     0,   401,     0,
     750,   749,   745,   747,   748,   746,     0,   740,   743,     0,
       0,     0,     0,   506,   779,   735,   789,   797,   801,   807,
       2,   782,   784,   786,     2,   802,   804,     0,   958,   961,
     401,     0,     0,     2,    90,   886,   735,   970,   832,   735,
     735,   970,   735,   847,   735,   735,     3,   612,     0,     0,
     970,   970,   401,   401,     0,    22,     0,     2,    23,     0,
     634,   968,     0,     0,   640,   168,   167,     0,     2,   401,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   735,   766,   770,   809,   735,   823,   828,   758,   815,
       0,     0,   387,   874,     0,   720,     0,   401,   721,   380,
       0,     0,     0,     0,   378,     2,   722,     0,   363,   699,
       0,   717,     2,   723,     0,     0,     0,     0,   537,   597,
     402,     3,     3,   601,   600,   793,     0,     0,   401,   343,
       0,   481,     3,    89,     3,   401,     0,     3,     2,   655,
     401,   401,   649,   648,   649,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   401,     0,   110,   109,
       0,   106,   105,    26,     0,    27,     0,     0,     0,     0,
       3,    56,    41,    42,    49,     0,    48,    59,     0,    57,
      60,    44,     0,    43,    47,     0,     0,    40,   633,   146,
     112,   114,   115,   116,   118,   119,   121,   122,   126,   127,
     124,   125,   129,   130,   132,   134,   136,   138,   140,     0,
       0,   401,     0,     0,     0,   863,   864,   860,   861,     0,
      28,   458,   456,   674,   673,   575,   932,   401,   937,   402,
     401,   923,   401,     0,     0,     0,   901,     0,   735,   971,
     887,   888,   617,   884,   885,   899,   927,   931,   929,   487,
     522,     0,   935,   940,   578,   969,     0,   152,     0,   577,
       0,   968,   681,   679,     0,     0,   742,    56,   705,     3,
     352,     0,     2,   783,   785,   787,     2,   803,   805,   401,
     401,   879,   878,     2,     0,     0,     0,     0,     0,   735,
     887,   835,   852,     2,   830,   838,   614,   833,   834,   615,
       2,   845,   855,   848,   849,     0,     3,   970,   371,     2,
     963,     2,   605,   606,   584,     3,     3,     3,     3,   628,
       2,   642,     0,   639,   969,     0,   635,     0,     2,   638,
     641,   718,     0,   401,     3,   375,   377,     0,   735,   767,
     771,   735,   824,   829,     2,   759,   762,   764,     2,   816,
     819,   821,   733,     0,   875,     3,   725,     3,   471,   470,
     473,   472,     2,   700,   726,     2,   724,     0,   700,   727,
     537,   537,   537,   401,     0,     0,   619,     0,   346,     0,
       0,   401,     0,     2,     0,     0,     0,     0,     0,   173,
       0,   276,   277,     0,     0,   315,   314,     0,   148,   148,
     321,   498,   504,   187,     0,   174,     0,   197,   175,   176,
     401,   191,   177,   178,   179,   180,     0,   181,   182,   282,
       0,   183,   184,   185,     0,   186,   193,   481,   401,     0,
     195,     0,   340,     0,     0,     0,     3,     0,   700,   688,
     689,     0,     3,   684,     3,     3,     0,   401,   665,   665,
      84,   401,     0,    88,    86,   401,     0,   100,     0,     0,
       0,   104,   108,   107,   172,     0,     0,     0,   633,    97,
     165,     0,     0,    73,     0,    73,    73,     0,    61,    63,
      39,     0,     0,    37,     0,    38,   968,   143,     0,     3,
     735,   870,   873,   865,     0,   934,   939,     2,    89,   401,
       3,   496,     3,   402,     3,   735,   894,   897,   401,     3,
     883,   889,     0,   735,   735,     0,   581,   566,   582,   968,
       0,     2,   739,   741,     0,     0,     0,   401,   401,     3,
       3,     0,   735,   841,   844,   735,     0,   735,   735,   836,
     853,   401,   401,   964,     0,   607,   401,   401,     0,     0,
       0,     0,   360,     0,   144,     0,     3,     3,   636,     0,
     632,     0,   170,   169,     3,     0,     0,     2,   760,   763,
     765,     2,   817,   820,   822,   401,   401,   628,   735,     0,
       0,     0,   700,   728,     0,   401,   401,   401,   401,   401,
     401,   520,   548,     3,     3,   549,   481,   538,     0,     0,
     775,     2,     0,   344,    56,     0,     0,   267,   268,   194,
     196,     0,     0,     0,     2,     2,   263,     0,   261,     0,
       0,     0,   633,     0,     0,     0,     0,     0,   149,     0,
       0,   322,     0,     0,     3,   200,     0,   192,     0,   258,
       0,     0,     2,     0,   481,   735,     0,   341,   881,   880,
       0,     2,     0,   691,     2,   686,     0,   687,     0,   669,
     650,   654,   652,     0,     0,     0,    31,     0,   101,   103,
     102,    99,    98,   633,   968,     0,    55,    70,     0,    64,
      71,    72,    50,     0,     0,     0,    58,    46,     0,     0,
     142,     0,     2,   866,   868,   869,    29,   401,     0,     0,
       0,     3,     0,     2,   890,   892,   893,     0,     0,    89,
       0,     3,   968,   571,     0,   581,   579,     0,   569,   682,
     401,   744,   351,     3,     3,     0,     0,   735,     2,   837,
     839,   840,     2,   854,   856,     0,   831,   846,     3,     3,
     965,     3,   592,   591,   595,   967,     2,     2,   966,     3,
       0,     0,     0,     0,   637,     3,     0,   735,   382,   401,
     401,     3,     3,   388,   734,     0,   825,   709,     0,   711,
     520,   520,   520,   555,   525,     0,   561,   549,     0,   401,
     512,   547,   543,     0,     0,     0,     0,   550,   552,   735,
     563,   563,     0,   544,   559,   401,   347,     0,   271,   272,
     269,   270,     0,     0,     2,   401,     2,   401,   264,   262,
       0,   256,   968,   265,     0,     0,     0,   303,   304,   305,
     306,     0,   296,     0,   297,   273,     0,   274,     0,     0,
     401,   201,   190,   260,   259,     0,   294,   313,     2,   345,
     735,   401,   707,   670,   401,     2,     2,    87,     0,    30,
     401,     0,   968,     0,     0,    74,     0,    62,     0,    68,
       0,    66,    36,   147,   871,   401,   942,   943,   944,     0,
     895,   401,     3,     3,     0,   903,     0,     0,     0,     0,
     580,   568,     3,     0,     0,   776,   794,   401,   401,     0,
       0,     0,   401,   401,     0,     3,   732,   643,   644,     0,
     368,   370,     3,     3,     0,     0,     0,   713,   516,   518,
     514,     0,   910,     0,   556,   915,   558,   907,   735,   735,
     542,   562,   546,     0,   545,     0,     0,     0,   565,     0,
     735,   539,   553,   564,   554,   560,   599,   603,   602,     2,
       0,     0,   227,   208,     0,     0,   210,   355,   209,   481,
     401,   231,     0,   173,   237,     0,   232,   173,   257,     0,
       0,     2,   280,   307,     0,   298,     2,     0,     0,     0,
       0,   285,     0,   281,   188,   369,   685,     0,     0,    34,
      32,    33,     0,    53,   166,    65,     0,     0,     3,   945,
       3,     0,     0,   902,   904,   570,     0,   968,     2,   777,
     795,     3,     3,   842,   857,   372,     2,   589,     3,   588,
     646,     0,     0,     0,   768,   826,   876,     0,     0,     0,
     911,   912,   735,   541,   908,   909,   540,   521,     0,     0,
     279,     0,     0,     2,   219,     2,   202,     0,     0,     2,
     211,   481,   238,     0,   253,   254,   255,   252,   241,     0,
       2,   401,     0,     0,     2,   204,   278,     2,   401,   275,
       0,     0,   323,     2,   283,     0,    56,     0,   295,   690,
     692,    54,    69,    67,     0,     0,   905,   906,   968,     0,
     683,     0,     0,   401,     0,   645,   769,   827,   735,   918,
     920,   913,     0,   551,   212,   215,     0,   214,   218,   401,
     221,   220,   229,     0,     3,   173,   246,     0,   242,     0,
     239,     3,   233,   173,   266,   401,   401,     3,   308,   402,
     312,     0,   316,     0,     0,     0,   324,   325,   206,   286,
       0,     2,     0,     2,     2,   872,   896,     0,   573,   843,
     858,   593,     2,   914,   916,   917,   557,     0,     0,   217,
     222,   401,   336,   228,   226,   234,   243,   254,   252,     0,
     173,     0,   230,   236,   222,     3,   301,     0,   910,   309,
     310,   311,   323,     0,     0,     0,   323,     0,     2,   284,
     291,     0,   288,   290,   572,   401,   213,   216,     2,     3,
     223,   337,   248,   247,   244,   235,   240,     3,   301,     0,
       0,   911,     0,     0,     0,   317,     0,   326,   207,     0,
     281,     0,     3,   198,   224,     0,     2,     0,     0,     0,
       0,     0,   302,     0,   329,     0,   327,     0,   329,   287,
     289,     2,     0,     0,   199,   203,   225,   250,   251,   249,
     245,   205,     0,   299,   330,     0,     0,   318,     0,   292,
       2,   919,   300,     0,     0,     0,     0,   293,   331,   332,
       0,   328,   319,     0,     0,   320,   333
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1632,  5885,  5955, -1632,    -1,   629,   185, -1632,  1517, -1632,
     295, -1632,  -583,   667,  -822,  -983, -1632,   164,  2277,  1721,
   -1632,  1788, -1632,  1265,   162,   731,   737,   514,   733,  1214,
    1215,  1213,  1220,  1218, -1632,   -60,  -139,  7851,   763, -1632,
    -339, -1632, -1632,  -568,  1768, -1063,  1386, -1632,    45, -1632,
     753,   -52, -1632, -1632, -1632,   338,    21, -1632, -1595, -1417,
     216,     6, -1632, -1632, -1632,   128,    72, -1632, -1632, -1632,
   -1632,   -30, -1601,   126, -1632, -1632,   -27, -1632, -1632, -1632,
     -14,   374,   375,    78, -1632, -1632, -1632, -1632,  -802, -1632,
      22,   -25, -1632,    83, -1632,   -97, -1632, -1632, -1632,   773,
    -647,  -877,  -966, -1632,    30,     0,    93,  3613,  -865,  -843,
   -1632,  -258, -1632,    75,  -144,   908,  -345,  -221,  3605,  6624,
    -532, -1632,     3,   144,  1237,  2063, -1632,  1871, -1632,   186,
    3986, -1632, -1632, -1632,   127, -1632, -1632,   418,   225,  4606,
    2894,   -38,  1672,  -334, -1632, -1632, -1632, -1632, -1632,  -599,
    1553,  5290, -1632,  -309,    89, -1632,   434,   187, -1632,   122,
     630, -1632,   426,  -117, -1632, -1632, -1632,  5351,  -685, -1143,
    -668,  -549,  -231,  1050, -1632, -1224,  -147,  -125,  1544,   797,
    4200,  -262,  -457,  -245,  -181,  -830,   899, -1632,  1162,  -155,
    1078,  1373, -1632, -1632, -1632, -1632,   227,  -165,  -215,  -170,
   -1632,   184, -1632, -1632,   507,   387, -1632, -1632, -1632,  1942,
    -708,  -503,  -895,    68, -1632, -1632, -1632, -1632, -1632,    -5,
    -763,  -122, -1631,  -210,  6955,   -58,  6330, -1632,   970, -1632,
     929,  -190,  -206,  -204,  -192,     5,   -71,   -61,   -57,   114,
      12,    40,    63,  -175,   -45,  -169,  -166,  -161,  -710,  -723,
    -640,  -627,  -655,  -100,  -608, -1632, -1632,  -589,  1296,  1299,
    1310,  1890,  7573,  -611,  -622,  -621,  -601,  -681, -1632, -1485,
   -1580, -1544, -1540,  -658,    20,  -342, -1632, -1632,   117,   487,
     -98, -1632,  8265,   347,  -495,  -551
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1114,   211,   338,   339,   169,   340,   341,   342,  1375,
    1376,   343,   888,   889,  1187,  1188,  1189,  1387,   344,   410,
     346,   347,   591,   592,   348,   349,   350,   351,   352,   353,
     354,   355,   356,   357,   358,   359,  1265,   593,  1339,   627,
     205,   629,   361,   777,  1115,  1116,  1117,  1118,  1119,  1120,
    1121,  1924,  1122,  1123,  1344,  1654,  1805,  1806,  1744,  1745,
    1746,  1899,  1900,  1124,  1665,  1666,  1759,  1125,  1126,  1127,
    1128,  1129,  1130,  1352,  1682,  1844,  1778,  1131,  1132,  1532,
    1910,  1533,  1534,  1827,  1133,  1134,  1135,  1342,  1835,  1836,
    1837,  1955,  1970,  1860,  1861,   282,   283,   838,   839,  1087,
      82,    83,    84,    85,    86,  1657,   438,    89,    90,    91,
      92,    93,   219,   545,   440,   362,   441,    96,   292,    98,
      99,   100,   374,   375,   103,   104,   165,   105,   932,   376,
     151,   108,   239,   109,   152,   248,   378,   379,   380,   153,
     413,   114,   115,   382,   116,   536,   827,   825,   826,  1490,
     383,   384,   119,   120,  1083,  1307,  1496,  1497,  1623,  1624,
    1308,  1485,  1642,  1498,   121,   705,  1588,   385,   703,   969,
    1025,   446,   447,   831,   832,   448,   449,   833,   387,   540,
    1139,   364,   365,   206,   770,   771,   772,   773,   774,   310,
    1158,   311,   854,   852,   569,   312,   403,   313,   314,   366,
     123,   171,   172,   124,  1152,  1153,  1154,  1155,     2,  1072,
    1073,   815,  1291,   125,   302,   250,   260,   519,   126,   209,
     127,   220,  1267,   818,   486,   163,   128,   716,   717,   718,
     129,   222,   223,   224,   225,   297,   131,   132,   133,   196,
     135,   136,   137,   228,   298,   230,   231,   232,   747,   748,
     749,   750,   751,   233,   753,   754,   755,   654,   655,   656,
     657,   487,   138,   680,   681,   682,   683,   684,   685,  1626,
    1627,  1628,  1629,   670,   451,   390,   391,   392,   367,   198,
     140,   141,   142,   394,   966,   686
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,    87,   182,    79,   101,   628,   130,   404,   415,   516,
     484,   472,   183,   473,   386,   180,   184,   372,   503,   822,
     995,   971,   596,   544,   462,   474,   665,   295,   480,   957,
     896,   879,  1452,  1453,   147,  1001,  1333,   189,   412,   688,
     287,  1727,   475,   696,   811,   813,   698,   699,   476,   229,
     701,   477,   943,   944,    79,    79,   478,    79,    87,   848,
     872,   101,   400,   130,   926,  1005,   450,   951,  1196,   597,
    1192,  1012,    79,   945,    57,  1140,    94,  1728,   551,   553,
      79,  1729,  1777,  1426,  -693,   185,   195,   472,    79,   473,
    1002,   665,   433,    79,    88,  1028,    79,   148,   573,   226,
      79,   474,   251,   996,   480,    57,   261,  1204,   688,   417,
    1807,  1044,   500,   186,  1078,   134,   997,   411,   475,   418,
     143,   943,   944,   419,   476,   207,   175,   477,   110,   952,
     492,   254,   478,    94,   951,   998,   187,  1731,    79,   203,
    1148,    79,   945,    79,    87,   102,   182,   101,    79,   130,
    1811,    88,    57,   276,    79,   514,   183,   485,   552,   470,
     184,    79,  1315,  1316,   154,   524,   204,    13,    14,    15,
      16,    17,   134,   481,   276,   278,   423,  1146,    79,    79,
    1536,   389,   677,  1294,   195,   110,    57,   106,   485,   573,
     244,  1136,   420,    79,   255,  -717,  1006,  -338,   197,   454,
    1009,  1389,   102,   836,   934,   277,  1302,    79,   227,  1022,
    1023,   252,   290,  1026,  1026,   262,    79,    79,  1303,    94,
     421,   491,   182,   195,   496,    57,   111,  1807,   258,   185,
    1026,   161,   183,    79,   531,   556,   184,    88,  -694,   802,
    1304,   246,    79,   422,   106,  1801,   513,   707,   195,   481,
     709,   973,    79,  1362,   465,    79,   523,   186,   134,  1537,
     277,   520,    79,   197,   784,   493,   785,  -338,    57,   485,
     995,   110,    79,    79,  1538,    79,  1035,  1317,   786,   178,
     187,   155,  1590,   111,   547,  1229,  -399,  -851,   102,   798,
    1249,   842,    79,    79,  1394,   787,  1026,  1727,  1182,   195,
      79,   788,   894,  1811,   789,    57,    79,    79,   455,   790,
    -339,    79,   855,  1005,   857,   858,  1075,   859,   936,  1777,
     861,   938,   160,   863,   864,   865,  1395,   548,  1320,   847,
     106,  1811,   688,  1728,   739,   203,  1203,  1729,  1173,  1221,
     752,  1539,   956,  1880,    57,  1250,    57,   559,  1384,   517,
     784,   485,   785,   996,    57,   962,   688,  1140,   963,   174,
     509,   407,   967,   688,   786,   968,   997,  1292,   798,   111,
     521,   943,   944,  1741,  1742,  1274,   595,   809,  1452,  1453,
    -339,   787,   450,   814,   730,  1241,   277,   788,   485,  1221,
     789,   671,   945,  1731,  1234,   790,    79,  1478,    57,   417,
     518,    79,   207,  1508,    79,   715,  1460,    57,   110,   418,
    1567,  1569,  1571,   419,    19,    79,   372,   509,  1302,  1302,
    1302,   454,    57,   982,   821,  1003,  1801,   485,   200,   675,
    1303,  1303,  1303,  1010,   799,    57,   483,   675,    79,    79,
    1461,  1459,   156,  1898,  1319,   157,   158,   525,   159,  1312,
      79,    79,  1304,  1304,  1304,  1743,   671,  1898,   537,   768,
      62,    63,   197,  1857,    57,   325,  1255,   106,  1313,    79,
     208,    57,  -717,  1136,   650,   450,  1230,  1054,    79,  1741,
    1742,   485,   420,  1926,   454,   417,  1058,   806,   139,   552,
     485,   139,    57,   200,  1026,   418,   688,  1461,    79,   419,
      57,  1277,  1522,  1833,    79,   485,   111,   368,    75,   817,
     421,  1941,   277,   799,  1281,   820,   651,   246,   485,   824,
      13,    14,    15,    16,    17,   368,  1104,  1502,    57,  1510,
     455,  1027,  1027,   422,  1415,  1314,   805,  1422,   176,   450,
    1841,   808,    79,  1402,    79,   139,  1503,   485,  1027,  1656,
    1413,   957,   547,  1562,   675,    79,  1351,    79,   816,   188,
      63,  1764,   874,   454,  1444,   867,  1566,   566,   823,  1851,
      79,  1438,  1871,  1842,  1656,   485,   868,   869,    57,  1442,
     389,   203,  1312,   675,   640,   641,   246,   272,   139,   177,
     510,  1036,  1205,   455,   570,  1206,   567,   568,   571,  1631,
    1753,  1549,    79,    79,  1762,   688,    79,  1852,   967,   444,
      79,   485,   874,    79,  1027,   450,  1195,  1016,  1632,   595,
     178,   531,  1929,  1191,   595,   178,  1398,   595,   642,   643,
      80,   139,  -622,   145,   190,   847,  1502,   450,   450,  -400,
      13,    14,    15,    16,    17,   752,   595,   510,   178,   606,
    1177,   607,   608,   609,   450,  1634,  -517,  1178,    79,  1046,
    -396,    79,   660,  1754,  1755,  1756,   661,   940,   201,    13,
      14,    15,    16,    17,   258,   874,   110,  1760,  1062,    -3,
     610,  1156,  1761,   611,   612,  1757,  1771,    80,   613,   614,
     246,  1772,   200,    79,  1758,  1819,   411,  1386,    57,   214,
    1370,  1399,    80,  -397,  1191,    79,  1640,   671,   234,    72,
      80,  -395,    13,    14,    15,    16,    17,   874,   673,  1732,
     450,   272,   372,    80,   276,  1641,    80,    57,   189,   674,
      80,  1507,  1640,   675,  1427,   106,  1668,    72,  1733,    57,
      77,   676,    79,   940,    79,  1885,  1696,   633,  1697,   277,
    1886,  1736,  1865,   677,   634,   635,    79,   652,   530,    63,
    1873,   485,    72,    79,   274,    79,  1937,  1525,    77,    78,
      57,  1938,   721,    80,   111,   423,   722,   485,    80,    79,
      79,    79,   674,   673,  1262,   291,   675,  1228,  -338,   668,
     402,   398,   691,    77,    78,   901,   902,   903,   308,    79,
      13,    14,    15,    16,    17,   405,   668,  1905,    80,    80,
     668,   276,  1027,   423,   406,   485,  1077,  1564,   235,   236,
     493,   237,   794,    80,   485,   238,   241,     6,     7,     8,
       9,    10,    11,    12,   518,    79,    79,   768,    87,   156,
     723,  1138,   157,   158,   661,   159,    80,    80,   368,   368,
     325,  1066,  1287,  1268,   668,   453,  1037,  1038,    57,  1197,
    1039,  1224,  1074,    80,   956,  1076,   424,   734,  1655,  1079,
    1667,   485,    80,  -110,  -110,  -110,  -110,  -110,  -110,   688,
    1149,  1618,  1619,  1620,    79,    80,   389,   968,   559,  1563,
     423,    79,   485,  1655,  1003,   425,   423,   768,   675,  1248,
     752,   426,   534,  1377,  1707,   539,   835,   981,   427,    95,
     836,   873,   149,    94,    72,   874,   972,    72,   450,   428,
     571,   429,    80,    80,   457,   444,   458,  1587,    79,   974,
     847,    88,   975,   571,  1621,   986,   976,   652,   485,   485,
    1041,   485,   459,  1150,  1042,    77,    78,   463,    77,    78,
     241,     6,     7,     8,     9,    10,    11,    12,   276,   493,
      79,   466,   485,   485,    79,   110,    95,  1068,   467,    79,
     468,   874,  1264,  -386,   246,   715,   636,   637,   444,  1754,
    1867,  1756,   102,  1070,   559,   518,  1063,   874,   485,   193,
    1484,   874,    79,   404,   404,   469,  -386,  1417,    79,    79,
     482,  1868,   181,  1527,  1528,  1529,  1530,  1531,   368,   263,
    -174,  -109,  -109,  -109,  -109,  -109,  -109,   372,   444,   190,
     599,  1788,  1190,   221,   106,  1451,  1191,  1669,   483,  1363,
     501,  1369,    18,   768,   958,   661,    79,  1431,   541,  1559,
    1649,  1191,  1473,  1560,  1191,    80,   285,   638,   639,  1650,
     502,    61,    95,   874,   193,   368,    64,    65,    66,    67,
      68,    69,    70,   111,  1670,   644,   645,  1692,   874,    80,
      47,    48,    49,    50,    51,    52,    53,    54,   296,   512,
    1912,   668,   444,   246,  1916,   522,   968,   207,  1464,   769,
     562,    87,   768,   576,  1138,    79,    79,    79,   431,    80,
      74,   577,  1671,  1030,   581,   668,  1042,  1672,    80,  1737,
    1454,   874,  1813,   661,  1667,  1500,   874,  1889,   668,   768,
      87,  1191,  1293,  1138,   605,    79,  1939,   603,    80,   604,
     874,   450,   450,    79,    80,  1318,    79,    79,  1400,   632,
      79,  1966,   251,   261,   646,  1963,   471,   221,   505,  1973,
      79,   508,  1337,  1974,   908,   909,   910,   911,   444,   663,
     599,    79,  1862,   296,   876,   877,    94,   647,   254,  1295,
    1296,  1297,   145,   648,    80,   649,   847,   768,  1862,  1018,
    1019,   389,   659,    79,    88,    80,   663,    80,   695,    95,
    1020,  1021,  -850,  1840,   708,    94,  1180,  1042,   368,   719,
    1309,    13,    14,    15,    16,    17,   871,   599,   508,  1193,
    1194,  1264,  1789,    88,  1901,   874,  1198,   724,   110,  1020,
    1361,    79,   557,   296,   992,  1385,  1392,  1393,  -567,   671,
     244,   255,   882,   883,   706,   102,   886,  1397,  1393,   725,
     893,  1406,  1393,   897,   726,    79,   727,   110,  -145,  -145,
     728,   252,   262,   193,   992,  1475,   729,  1658,  1572,  1042,
    1501,    61,    79,   372,   102,   258,    64,    65,    66,    67,
      68,    69,    70,   884,  1607,  1385,   430,   106,  1694,  1042,
      -3,   246,  1658,   756,   444,  1695,  1393,   472,   930,   473,
     778,   935,  -398,  1847,  1715,  1716,   803,  1500,  1377,  1726,
     874,   474,  1775,  1776,    79,   480,   106,   791,    79,   792,
      74,    79,   801,   885,  1858,  1859,   111,   793,   475,  1635,
     795,  1605,  1606,   960,   476,   139,   284,   477,   147,  1782,
    1393,   768,   478,  1783,  1393,  1741,  1742,   139,   796,   264,
     738,  1963,  1964,   265,   797,   111,   268,   819,   270,  1390,
    1391,   768,  -515,    79,  -513,   668,   828,   518,   691,   520,
    1509,  1511,    13,    14,    15,    16,    17,   904,   905,   746,
     837,  1718,    80,   849,    80,   906,   907,   912,   913,   851,
    1647,   881,   768,  1643,  1643,  1371,  1372,    79,   898,   875,
     878,   148,    79,    79,    79,  1031,   931,   946,  1547,   783,
     411,   411,   948,   965,  1454,   677,   970,   372,   221,  1045,
      80,  1047,  1309,  1309,  1309,   977,  1486,  1309,   978,   980,
      57,   991,   992,  1828,   999,   -16,   -17,   389,   296,    80,
     784,  1033,   785,  1034,   296,  1636,    13,    14,    15,    16,
      17,  1172,  1501,  1048,   786,  1049,  1050,   517,  1051,   798,
     481,   149,  1052,  -697,  1053,  1069,  1454,    95,  1071,  -598,
    1149,   787,  1141,  1080,  1142,  1086,  1081,   788,   521,  1082,
     789,  1157,  1160,  1162,   296,   790,  1163,   450,   450,    72,
    1164,  1828,  1165,   264,  1166,   846,    79,   296,  1168,  1169,
    1170,  1175,    79,  1176,    79,   688,  1200,  1201,   518,   674,
    1202,    79,  1215,   675,   368,  1216,  1217,  1227,  1232,  1231,
      77,   676,  1236,   768,  1186,   768,  1242,  1243,  1660,  1244,
    1660,  1186,  1245,  1150,  1253,  -586,  -585,   769,    13,    14,
      15,    16,    17,   241,     6,     7,     8,     9,    10,    11,
      12,  1270,  1276,  1660,  -698,  1311,  1288,   768,  1310,  1500,
    1321,  1324,  1325,  1334,   117,  1335,   958,   117,  1186,  1336,
    1341,  -621,  1343,   874,  1351,  1345,  1780,    79,  1355,    79,
    1358,   389,  1357,  1359,  1365,  1383,  1673,  1428,   139,   444,
    1429,  1385,  1437,  1367,  1149,  1432,    57,  1450,    79,  1455,
    1223,  1456,  1457,  1458,   799,  1467,  1476,  1477,  1479,  1491,
    1489,  1492,  1804,  1314,  1513,  1542,  1514,   139,    88,  1516,
      88,   117,   264,   265,  1518,   692,  1540,   270,  1545,  1550,
    1519,  1521,    80,  1557,  1552,   139,  1523,   668,    80,    80,
    1535,  1553,  1555,    88,   117,    79,  1543,  1544,    79,  1556,
    1565,  1558,   110,  1561,   110,    72,  1454,  1150,   768,  1573,
    1574,  1576,  1577,  1589,   117,  1580,  1578,  1585,  1591,   102,
    1595,   102,  1596,   769,   423,  1621,  1031,   110,   450,   485,
     768,   472,   994,   473,   746,  1393,    77,    78,  1608,  1610,
    1617,   411,  1630,  1494,   102,   474,    79,    79,   480,  1651,
     254,   117,  1676,  1683,  1501,  1678,  1693,   117,  1706,   117,
    1705,   106,   475,   106,  1699,    79,  1879,  1709,   476,  1703,
     296,   477,  1704,   117,  1710,  1713,   478,  1714,  1896,  1804,
     208,   798,    81,  1720,  1679,   146,   106,  1607,  1724,   296,
    1725,   117,  1749,  1763,  1769,  1773,  1767,  1104,    79,  1774,
     111,  1781,   111,   117,   768,  1914,    95,  1790,   768,  1786,
    1787,  -587,   244,   255,  1796,   264,  1795,  1797,  1151,   768,
    1798,  1799,  1800,   539,  1660,   111,    80,    80,   485,  1808,
    1831,  1812,   768,  1845,  1846,  1815,  1848,  1849,  1823,    81,
      80,  1824,  1832,  1850,  1716,   139,  1863,   258,  -498,  1882,
    1870,  1373,  1883,   117,   179,  1884,   117,   411,   411,  1887,
    1888,   117,    81,   246,  1891,  1894,   182,   769,  1902,  1903,
    1913,  1909,  1660,  1186,  1915,   218,   183,  1965,   243,   556,
     184,  1920,    81,  1921,   411,   444,  1927,    79,  1660,    79,
    1928,  1933,  1934,   481,   117,  1936,   368,  1945,  1951,  1952,
     768,  1956,   768,   768,   517,  1957,  1960,  1961,  1971,  1932,
     602,  1972,  1975,   117,    88,  1690,   870,  1067,  1396,   146,
     914,   916,   915,  1770,  1660,    81,   799,   146,   918,   917,
     294,   300,  1340,  1347,  1946,    80,    79,    79,  1680,  1897,
    1907,   345,   371,   195,   117,  1830,  1765,   768,   110,  1822,
    1869,  1942,  1186,  1940,  1931,   518,   411,   768,   117,   345,
     179,   179,    88,  1843,  1875,   102,  1674,  1675,  1874,  1917,
      79,   146,   436,  1958,  1356,   243,   454,   139,    88,   166,
     511,  1633,   994,   768,  1856,   768,  1803,  1644,  1247,   746,
    1488,  1353,  1159,  1269,    80,  1040,   110,  1592,   218,   218,
     768,  1686,     3,  1830,   853,   768,  1233,   106,   922,     0,
       0,   923,   110,   102,    88,   294,     0,  1834,     0,   768,
       0,   769,   924,    79,    81,     0,     0,     0,     0,   102,
       0,  1199,     0,    79,     0,     0,     0,   243,  1953,     0,
       0,     0,     0,    80,     0,   117,   111,     0,   110,     0,
       0,  1305,  1214,     0,     0,   106,     0,  1962,     0,    95,
       0,     0,   139,     0,   139,   102,     0,   300,     0,     0,
       0,   106,   769,   300,   294,   294,     0,  1186,     0,     0,
       0,   146,  1186,  1186,  1186,     0,     0,   139,    95,     0,
       0,     0,     0,     0,   111,     0,     0,     0,     0,  1239,
    1240,   139,     0,     0,     0,     0,     0,   106,     0,     0,
     111,     0,   345,     0,   117,   117,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   345,  1834,    61,     0,
       0,  1834,  1834,    64,    65,    66,    67,    68,    69,    70,
     296,     0,   834,   653,   710,     0,   111,     0,     0,     0,
       0,     0,   371,   678,   687,     0,   117,     0,  1935,   594,
     117,     0,   117,     0,     0,     0,   578,     0,     0,   371,
       0,     0,     0,   371,     0,     0,    80,    74,   495,  1954,
       0,     0,    80,  1954,    80,     0,     0,   117,     0,   117,
     117,     0,   117,   630,   631,   117,     0,   179,   117,   117,
     117,     0,     0,     0,     0,     0,     0,   139,  1968,     0,
       0,     0,     0,   146,     0,     0,     0,   436,   711,     0,
     247,   745,     0,   687,     0,     0,     0,     0,     0,     0,
       0,   267,     0,   712,     0,  1445,   713,   714,    64,    65,
      66,    67,    68,    69,    70,     0,   668,     0,     0,     0,
       0,   218,     0,     0,     0,     0,     0,  1186,   630,  1186,
     218,     0,     0,  1305,  1305,  1305,   149,  1483,  1487,     0,
       0,     0,     0,   247,   117,     0,     0,     0,     0,     0,
     294,     0,   345,   345,   630,     0,   294,     0,   371,     0,
     702,    61,     0,     0,  1499,     0,    64,    65,    66,    67,
      68,    69,    70,  1183,   668,     0,     0,  1184,   139,  1185,
       0,     0,   658,     0,     0,     0,     0,   247,     0,     0,
       0,   117,     0,     0,     0,    80,   294,     0,    80,   810,
     812,     0,     0,     0,     0,     0,     0,   294,     0,   294,
      74,   371,     0,  1388,     0,     0,     0,  1433,  1434,     0,
       0,     0,     0,     0,     0,   345,   139,   146,   146,     0,
     345,  1448,  1449,   345,     0,     0,   146,   146,   146,     0,
       0,     0,   139,     0,     0,   732,     0,    80,   735,   247,
       0,   436,    61,     0,     0,   167,   168,    64,    65,    66,
      67,    68,    69,    70,     0,  1471,  1472,     0,  1151,     0,
       0,     0,   594,     0,     0,     0,     0,   594,   139,   247,
     594,   834,     0,    61,     0,   247,   167,   168,    64,    65,
      66,    67,    68,    69,    70,   495,     0,     0,    80,   594,
       0,    74,     0,   653,   653,  1225,     0,     0,     0,     0,
       0,     0,   345,     0,   371,   436,     0,   687,     0,     0,
       0,   117,     0,     0,     0,   678,     0,     0,     0,   678,
       0,     0,    74,   117,   117,     0,     0,     0,   371,     0,
       0,     0,     0,     0,     0,     0,  1499,     0,   687,   920,
     900,   371,  1637,    95,  1499,    95,  1326,     0,     0,   146,
       0,     0,    61,     0,     0,     0,   247,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,    95,   303,
     304,   305,   306,     0,   409,     0,     0,    80,     0,  1881,
       0,   436,  1151,     0,   745,   834,   745,    61,     0,     0,
     167,   168,    64,    65,    66,    67,    68,    69,    70,  1323,
    1246,    74,     0,   371,   371,     0,   834,    61,     0,     0,
     167,   168,    64,    65,    66,    67,    68,    69,    70,     0,
     371,     0,   294,    61,     0,   247,    80,    80,    64,    65,
      66,    67,    68,    69,    70,     0,    74,     0,     0,  1612,
    1613,   294,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,   834,   834,     0,    74,     0,     0,   307,
      80,     0,   658,   658,     0,  1348,     0,     0,     0,     0,
       0,   993,    74,     0,     0,   675,     0,   308,     0,   345,
       0,     0,    77,    78,     0,  1738,   371,     0,  1499,   247,
      61,   146,   345,   167,   168,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,   436,     0,   247,
       0,     0,   575,  1969,   234,   409,   580,     0,     0,     0,
       0,     0,     0,  1976,     0,     0,   583,   584,     0,   247,
       0,     0,     0,     0,     0,   984,     0,   296,   987,    74,
       0,     0,   409,   409,     0,  1698,     0,     0,     0,     0,
       0,  1700,     0,     0,     0,     0,   117,     0,     0,     0,
       0,     0,     0,  1349,   117,     0,   653,  1711,  1712,     0,
    1167,     0,     0,     0,     0,  1171,     0,     0,   371,     0,
    1209,     0,     0,     0,     0,     0,  1179,     0,  1499,    95,
       0,   678,     0,   117,     0,     0,     0,   495,     0,     0,
       0,  1056,     0,    18,     0,  1060,     0,   409,     0,     0,
       0,   117,    61,     0,     0,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
     117,     0,     0,   409,   745,     0,     0,    95,   117,     0,
      72,   745,     0,     0,     0,    51,    52,    53,    54,   247,
       0,     0,     0,    95,     0,     0,     0,     0,  1546,     0,
    1493,    74,     0,     0,     0,     0,     0,  1494,     0,     0,
       0,    77,    78,   247,     0,     0,     0,   296,     0,     0,
     247,     0,     0,     0,   371,     0,     0,    61,     0,    95,
       0,     0,    64,    65,    66,    67,    68,    69,    70,   891,
       0,   834,   834,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,   834,   834,     0,     0,     0,
       0,     0,     0,     0,   146,     0,   557,   296,     0,     0,
       0,     0,   345,     0,     0,   658,    74,    61,     0,   892,
     215,   216,    64,    65,    66,    67,    68,    69,    70,   834,
     834,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     296,   345,     0,     0,     0,    72,     0,     0,   117,   117,
     117,   117,   117,   117,     0,     0,     0,     0,   243,    81,
       0,     0,     0,     0,     0,  1877,    74,     0,     0,   485,
       0,     0,   294,     0,     0,     0,    77,    78,   146,   578,
       0,     0,   436,     0,     0,     0,   146,     0,     0,     0,
       0,     0,     0,   247,     0,   113,     0,     0,   113,     0,
    1652,     0,  1661,     0,   247,     0,     0,     0,     0,   409,
     409,   409,   409,   409,   409,   409,   409,   409,   409,   409,
     409,   409,   409,   409,   409,   409,   409,   409,     0,     0,
     436,    57,     0,     0,  1684,     0,     0,     0,  1279,     0,
       0,  1283,     0,     0,     0,  1922,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,  1378,  1379,  1380,     0,
       0,     0,    61,  1381,  1382,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,   113,     0,   371,   371,     0,
       0,     0,     0,   117,     0,     0,     0,     0,     0,     0,
       0,   249,     0,     0,     0,   113,     0,   630,     0,     0,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
    1246,    74,     0,   834,   834,     0,   146,   146,   146,   146,
     146,   146,     0,     0,     0,     0,  1495,   300,     0,     0,
       0,     0,   113,   409,     0,  1740,     0,     0,   113,     0,
     113,     0,   117,     0,   249,     0,     0,     0,     0,  1648,
       0,     0,     0,     0,   363,   113,   399,  1766,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   117,     0,
     117,     0,   363,     0,     0,   243,     0,     0,     0,     0,
       0,     0,     0,     0,   113,   363,     0,     0,   249,  1512,
    1404,     0,     0,   117,     0,     0,     0,     0,  1520,     0,
       0,     0,  1524,     0,  1526,     0,     0,   117,     0,     0,
       0,     0,     0,   117,     0,     0,     0,     0,     0,   834,
       0,     0,     0,     0,     0,   834,     0,     0,   436,     0,
       0,  1810,  1440,     0,   113,  1814,     0,   113,     0,   409,
       0,   834,   834,     0,   409,     0,  1821,     0,     0,     0,
     249,   146,     0,     0,     0,   409,     0,     0,     0,  1838,
       0,     0,     0,     0,     0,     0,     0,   535,     0,     0,
       0,     0,     0,     0,     0,   113,     0,     0,     0,     0,
     249,     0,     0,     0,     0,     0,   249,     0,     0,     0,
       0,     0,     0,     0,   113,     0,   409,     0,     0,     0,
     247,     0,     0,     0,  1622,     0,     0,     0,  1495,     0,
     345,    57,     0,   117,  1495,     0,  1495,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,  1890,     0,  1892,
    1893,     0,     0,     0,     0,     0,   345,     0,   345,   363,
       0,     0,    61,  1616,     0,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,    57,     0,
       0,   345,     0,     0,     0,   113,     0,   249,   113,     0,
      72,     0,   371,     0,  1918,   146,   247,     0,     0,     0,
       0,   146,   113,  1653,  1923,  1664,   113,     0,     0,    61,
     217,    74,   215,   216,    64,    65,    66,    67,    68,    69,
      70,    77,    78,     0,     0,     0,     0,     0,  1653,     0,
    1944,     0,  1923,     0,   117,     0,     0,    72,     0,     0,
       0,     0,     0,   371,   371,     0,   113,  1959,     0,     0,
     363,     0,  1944,  1681,     0,     0,   249,   293,    74,     0,
       0,     0,  1622,  1622,     0,     0,  1967,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,  1495,     0,     0,
    1495,     0,   117,     0,     0,     0,     0,     0,   409,   247,
       0,     0,     0,  1625,     0,     0,     0,     0,   117,     0,
     300,   146,    61,     0,     0,   167,   168,    64,    65,    66,
      67,    68,    69,    70,     0,   363,   363,     0,     0,     0,
     249,   113,     0,    13,    14,    15,    16,    17,     0,   294,
       0,     0,     0,     0,   117,     0,     0,   247,     0,  1748,
       0,     0,     0,  1750,     0,     0,     0,     0,     0,     0,
    1752,    74,   113,     0,     0,     0,     0,   113,     0,   834,
     249,   113,     0,   113,     0,   409,   409,   409,     0,     0,
       0,  1622,   409,   409,   113,  1328,     0,     0,     0,     0,
    1495,    57,     0,     0,     0,     0,     0,     0,   363,     0,
     113,   113,   300,   363,     0,   409,   363,     0,     0,   113,
     113,   113,   345,     0,     0,     0,   409,     0,     0,   146,
       0,     0,    61,     0,   363,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,  1625,  1625,     0,   371,     0,     0,     0,     0,     0,
      72,     0,     0,  1622,     0,     0,  1818,  1820,    57,  1664,
     146,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     743,    74,     0,     0,   675,     0,   146,   146,     0,  1878,
     300,    77,   744,     0,     0,   363,   399,   113,   363,    61,
     249,     0,     0,  1839,    64,    65,    66,    67,    68,    69,
      70,     0,    13,    14,    15,    16,    17,     0,     0,     0,
       0,   113,   146,     0,   249,  1866,     0,    72,   535,     0,
       0,   249,     0,     0,   113,     0,   964,     0,  1878,  1878,
       0,     0,   113,     0,     0,    61,    97,    73,    74,   150,
      64,    65,    66,    67,    68,    69,    70,  1183,    77,    78,
    1625,  1184,     0,  1185,     0,     0,     0,     0,     0,     0,
      57,     0,  1878,     0,   363,     0,     0,  1904,     0,  1906,
       0,    61,     0,     0,   167,   168,    64,    65,    66,    67,
      68,    69,    70,     0,    74,     0,   113,   113,     0,  1919,
       0,    61,     0,    97,   215,   216,    64,    65,    66,    67,
      68,    69,    70,   113,    61,     0,     0,   395,   396,    64,
      65,    66,    67,    68,    69,    70,   194,     0,  1854,    72,
      74,     0,  1625,     0,   192,  1947,  1949,  1950,     0,     0,
    1330,   113,     0,     0,     0,     0,   256,     0,     0,  1877,
      74,     0,     0,   485,     0,     0,     0,     0,  1625,     0,
      77,    78,   247,    74,   249,    75,     0,     0,     0,     0,
     397,     0,   363,     0,     0,   249,     0,   409,   409,   113,
       0,     0,     0,   286,   113,   363,     0,     0,     0,    97,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     363,     0,     0,     0,     0,     0,   373,  1625,  1625,     0,
       0,     0,    61,   192,   192,   167,   168,    64,    65,    66,
      67,    68,    69,    70,   416,     0,     0,     0,     0,     0,
       0,   192,     0,     0,     0,   286,   442,     0,     0,     0,
       0,  1625,     0,     0,   439,     0,     0,     0,    13,    14,
      15,    16,    17,     0,   247,   113,     0,     0,     0,     0,
       0,    74,   409,   479,     0,     0,     0,     0,   564,     0,
       0,   113,     0,  1211,   363,     0,   113,     0,     0,   499,
       0,     0,     0,     0,   504,   506,     0,     0,   194,     0,
       0,     0,     0,     0,     0,     0,   192,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
     526,     0,     0,   528,     0,   529,     0,     0,     0,     0,
      57,     0,     0,   113,   113,     0,   546,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,    61,     0,   558,
     215,   216,    64,    65,    66,    67,    68,    69,    70,     0,
       0,    61,     0,   192,   215,   216,    64,    65,    66,    67,
      68,    69,    70,     0,     0,    72,     0,     0,    13,    14,
      15,    16,    17,     0,     0,     0,     0,   113,     0,    72,
       0,     0,     0,     0,     0,   217,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,    78,   192,  1493,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,    78,     0,     0,     0,     0,   666,   113,     0,   690,
       0,     0,     0,     0,   192,   363,    57,   107,     0,     0,
       0,     0,     0,   697,     0,     0,     0,   697,    61,     0,
       0,   215,   216,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   363,     0,     0,    61,   409,     0,
     215,   216,    64,    65,    66,    67,    68,    69,    70,     0,
       0,   249,   113,     0,     0,     0,     0,   286,     0,     0,
       0,   666,     0,     0,   107,    72,     0,    74,     0,   192,
       0,   113,   409,     0,  1222,   363,     0,     0,     0,   113,
       0,     0,     0,     0,    61,   293,    74,   530,    63,    64,
      65,    66,    67,    68,    69,    70,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,   320,   257,   321,     0,
     322,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,   363,     0,     0,     0,  1211,     0,     0,
       0,     0,   442,    74,   192,   192,   929,     0,     0,  1425,
     439,     0,     0,    13,    14,    15,    16,    17,     0,     0,
     107,   113,   113,     0,   409,   409,   601,     0,     0,    75,
     408,     0,     0,   830,     0,   113,   113,   377,   506,     0,
     113,   113,   841,     0,   546,     0,     0,     0,     0,     0,
       0,   409,     0,     0,     0,   373,     0,     0,     0,     0,
       0,     0,     0,   192,     0,     0,     0,   443,     0,   113,
     113,    57,     0,     0,     0,     0,   409,     0,     0,   113,
     113,   113,   113,   113,   113,     0,     0,     0,     0,     0,
     249,     0,     0,     0,     0,   442,     0,     0,     0,     0,
       0,     0,    61,   439,     0,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,   249,     0,
       0,   527,     0,     0,     0,   170,   173,     0,   925,     0,
    1493,    74,     0,     0,     0,     0,     0,   107,   697,   939,
       0,    77,    78,     0,   192,     0,     0,   439,     0,     0,
       0,     0,     0,   950,     0,     0,     0,     0,     0,   210,
       0,     0,   666,     0,     0,     0,     0,   959,     0,     0,
     192,   363,    61,     0,     0,   697,     0,    64,    65,    66,
      67,    68,    69,    70,  1183,     0,     0,     0,  1184,     0,
    1185,   192,     0,     0,   113,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,     0,     0,     0,   288,
       0,     0,   289,     0,     0,   939,     0,     0,     0,     0,
    1000,    74,     0,   439,  1568,   309,     0,   667,     0,     0,
     257,     0,     0,   113,   113,     0,     0,   442,   442,     0,
       0,     0,     0,     0,   667,   439,   439,     0,   667,     0,
       0,     0,     0,   363,   442,     0,     0,    57,     0,     0,
       0,     0,   439,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,   464,   363,
       0,   363,   830,     0,     0,     0,     0,     0,    61,     0,
       0,     0,   667,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,     0,     0,  1137,     0,   113,    72,     0,   113,     0,
     442,     0,     0,   515,   113,   150,     0,     0,   439,     0,
       0,     0,     0,   170,   192,     0,    73,    74,     0,   113,
       0,   373,     0,     0,   170,   113,     0,    77,    78,   192,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   113,   113,   443,   203,     0,   113,   113,     0,     0,
       0,   561,     0,     0,     0,     0,     0,   563,   565,   315,
       0,     0,   572,   316,     0,   317,     0,     0,     0,     0,
       0,   626,     0,     0,   377,     0,   830,     0,     0,     0,
       0,     0,   318,   257,     0,   107,     0,     0,     0,     0,
       0,     0,   697,     0,     0,  1213,   443,   830,     0,     0,
       0,     0,  1219,   249,   113,     0,     0,     0,     0,   319,
     320,     0,   321,     0,   322,  1816,    63,    64,    65,    66,
      67,    68,    69,    70,   323,   324,   325,     0,   326,   327,
     328,     0,   329,   330,     0,     0,   443,     0,     0,     0,
      72,     0,     0,     0,   830,   830,     0,   704,     0,     0,
       0,     0,   309,     0,     0,   309,     0,   112,     0,     0,
     331,     0,     0,    75,   408,     0,     0,     0,     0,     0,
     333,    77,    78,   334,   335,   336,   337,     0,     0,     0,
       0,     0,     0,     0,  1817,  -173,     0,     0,     0,   210,
       0,     0,     0,     0,     0,   249,     0,     0,   442,   667,
     443,   760,   761,     0,     0,   363,   439,     0,     0,     0,
      61,     0,   113,     0,   112,    64,    65,    66,    67,    68,
      69,    70,  1183,   667,     0,     0,  1184,     0,  1185,     0,
       0,     0,     0,     0,     0,     0,   667,   113,  1306,     0,
       0,     0,     0,     0,     0,     0,  1137,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,   259,     0,    74,
       0,     0,  1570,     0,     0,     0,     0,     0,     0,   113,
     113,     0,     0,   249,    61,  1137,   443,   215,   216,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,  1354,     0,     0,     0,     0,   377,   377,
     112,     0,    72,    61,     0,   113,   215,   216,    64,    65,
      66,    67,    68,    69,    70,   377,   373,   381,     0,     0,
     192,   309,   743,    74,   192,     0,   675,     0,     0,     0,
       0,    72,     0,    77,   744,     0,     0,     0,     0,   113,
       0,     0,     0,   377,     0,     0,   677,   445,     0,     0,
       0,  1493,    74,     0,     0,     0,     0,     0,  1494,     0,
       0,     0,    77,    78,   666,     0,     0,     0,     0,     0,
       0,     0,   192,   504,   107,     0,     0,     0,     0,    61,
       0,   377,   188,    63,    64,    65,    66,    67,    68,    69,
      70,     0,   830,   830,     0,     0,     0,     0,     0,     0,
       0,     0,   443,     0,     0,     0,   830,   830,     0,     0,
       0,   442,   442,     0,     0,     0,     0,     0,     0,   439,
     439,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,  1030,     0,     0,     0,     0,     0,   112,     0,     0,
     830,   830,     0,     0,     0,     0,     0,     0,     0,     0,
    1306,  1306,  1306,   150,     0,     0,   704,   377,     0,     0,
      61,     0,     0,   215,   216,    64,    65,    66,    67,    68,
      69,    70,     0,   667,     0,     0,   257,     0,   377,     0,
       0,     0,     0,     0,     0,     0,     0,    61,    72,     0,
     215,   216,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,  1017,  1877,    74,
       0,     0,   485,     0,  1029,    72,     0,     0,     0,    77,
      78,     0,     0,     0,     0,   377,   377,   669,     0,     0,
     259,     0,     0,     0,     0,   217,    74,     0,     0,     0,
       0,     0,     0,     0,   669,     0,    77,    78,   669,    61,
       0,     0,   215,   216,    64,    65,    66,    67,    68,    69,
      70,     0,   373,     0,     0,     0,     0,     0,     0,     0,
     192,     0,     0,     0,     0,     0,     0,    72,     0,   377,
       0,     0,     0,     0,     0,   150,     0,  1088,     0,     0,
       0,     0,   669,     0,     0,     0,    61,   293,    74,   532,
     533,    64,    65,    66,    67,    68,    69,    70,    77,    78,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,   830,   830,     0,   107,    61,     0,
       0,   167,   168,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,    74,     0,    75,     0,     0,
    1639,     0,   192,     0,     0,     0,   107,     0,     0,    74,
     830,    75,     0,   445,     0,     0,     0,     0,     0,     0,
    1659,     0,  1659,     0,   257,    61,   453,    74,   167,   168,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,   381,  1659,     0,   443,     0,     0,
       0,     0,     0,   259,     0,   112,   373,     0,     0,   150,
       0,     0,     0,     0,   192,  1226,   445,     0,     0,     0,
     704,     0,     0,   457,    74,     0,     0,     0,     0,     0,
     830,     0,     0,     0,     0,     0,   830,     0,     0,     0,
       0,     0,     0,     0,     0,   667,     0,     0,     0,     0,
       0,     0,   830,   830,     0,     0,   445,   442,   442,     0,
       0,     0,     0,     0,     0,   439,   439,     0,     0,     0,
       0,     0,     0,   377,   377,    61,     0,  1730,   167,   168,
      64,    65,    66,    67,    68,    69,    70,   377,   377,     0,
       0,     0,   377,   377,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1751,     0,     0,     0,   669,
     445,   377,   377,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,   669,   118,     0,  1327,  1329,  1331,     0,
       0,     0,     0,     0,     0,     0,   669,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1350,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1088,     0,     0,     0,     0,   445,     0,   118,     0,
       0,     0,   122,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1659,     0,   381,   381,
       0,   118,     0,  1829,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   381,     0,     0,     0,     0,
       0,   118,     0,   443,     0,     0,     0,     0,   442,     0,
       0,     0,     0,     0,     0,     0,   439,     0,     0,   122,
       0,     0,     0,   381,  1659,     0,     0,     0,     0,     0,
       0,     0,   704,     0,     0,     0,     0,     0,   118,     0,
    1659,  1829,   122,     0,   118,     0,   118,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,     0,     0,
     118,   381,   122,     0,     0,   377,   377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1659,     0,   118,     0,
       0,     0,   445,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,  1911,     0,     0,     0,     0,     0,   122,
       0,   377,     0,     0,     0,   122,     0,   122,     0,     0,
     830,   107,     0,   107,     0,     0,     0,     0,     0,  1504,
    1218,   122,  1506,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,     0,     0,   107,   381,     0,   122,
     118,     0,     0,   118,     0,     0,     0,   377,   118,     0,
       0,   122,     0,   669,     0,     0,   259,     0,   381,     0,
       0,     0,     0,   315,     0,     0,     0,   316,     0,   317,
       0,   377,     0,     0,     0,     0,     0,   377,     0,     0,
       0,   118,     0,     0,     0,    57,   318,     0,     0,     0,
       0,     0,     0,   377,   377,     0,     0,     0,   377,   377,
     118,   122,     0,     0,   122,   381,   381,     0,     0,   122,
       0,     0,     0,   319,   320,     0,   321,     0,   322,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   323,   324,
     325,   118,   326,   327,   328,     0,   329,   330,     0,     0,
       0,     0,   122,     0,    72,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   257,     0,     0,   381,
       0,   122,     0,     0,   331,     0,     0,    75,   408,     0,
       0,     0,     0,     0,   333,   435,    78,   334,   335,   336,
     337,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,   112,     0,     0,
       0,     0,  1645,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   259,     0,     0,   107,     0,     0,
       0,     0,     0,     0,   667,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   445,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,     0,   122,     0,     0,     0,     0,     0,   704,
       0,   118,   118,     0,     0,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,   667,     0,     0,   669,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,   118,     0,   118,
       0,     0,     0,   381,   381,     0,     0,   107,     0,     0,
       0,     0,   122,   122,     0,     0,     0,   381,   381,     0,
       0,     0,   381,   381,   118,     0,   118,   118,     0,   118,
       0,     0,   118,     0,     0,   118,   118,   118,     0,     0,
       0,   377,     0,     0,     0,     1,     0,  1779,   144,     0,
       0,   381,   381,     0,   122,     0,     0,     0,   122,     0,
     122,     0,     0,     0,     0,     0,   704,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,   122,   122,     0,
     122,     0,     0,   122,     0,     0,   122,   122,   122,     0,
       0,     0,     0,     0,     0,     0,     0,   240,     0,     0,
       0,   118,     0,     0,     0,     0,    13,    14,    15,    16,
      17,   191,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,   445,    57,     0,     0,     0,     0,   281,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     202,     0,     0,     0,     0,     0,   212,   213,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,   381,   381,     0,     0,     0,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   299,     0,     0,
       0,     0,     0,     0,    77,    78,     0,     0,     0,     0,
       0,   381,   281,     0,     0,     0,     0,     0,     0,     0,
       0,   112,     0,   112,     0,     0,     0,   507,   118,     0,
       0,     0,     0,     0,     0,     0,     0,   281,     0,     0,
     118,   118,     0,     0,     0,     0,   112,   281,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   381,     0,     0,
       0,   538,   542,     0,     0,     0,     0,     0,   549,   550,
     315,     0,     0,     0,   316,     0,   317,     0,     0,     0,
       0,   381,     0,     0,   560,     0,     0,   381,     0,   122,
       0,     0,     0,   318,     0,     0,     0,     0,     0,     0,
       0,   122,   122,   381,   381,     0,     0,     0,   381,   381,
       0,     0,     0,     0,     0,     0,     0,   600,     0,     0,
     319,   320,     0,   765,     0,   322,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   323,   324,   325,     0,   326,
     327,   328,     0,   329,   330,   554,     0,     0,     0,     0,
       0,    72,     0,     0,     0,   664,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   259,     0,     0,     0,
       0,   331,    74,     0,   766,   767,     0,     0,     0,   460,
       0,   333,    77,    78,   334,   335,   336,   337,     0,     0,
       0,     0,     0,   720,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   737,     0,     0,     0,   740,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   762,     0,
       0,     0,   763,   764,     0,     0,     0,     0,     0,     0,
       0,   779,   780,   781,   782,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
     804,     0,     0,   118,   669,     0,     0,   164,   807,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
     741,     0,   742,     0,     0,     0,     0,     0,     0,   381,
       0,   758,   759,   164,     0,     0,   281,     0,     0,     0,
     118,     0,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   112,   669,     0,   122,     0,     0,   845,     0,     0,
       0,     0,   122,     0,   538,     0,     0,   118,   164,   850,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,   164,     0,   164,     0,     0,     0,   112,     0,     0,
       0,   122,     0,   866,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   122,
       0,     0,     0,   401,     0,     0,     0,     0,     0,   840,
       0,   381,     0,     0,     0,     0,     0,     0,   122,     0,
     401,     0,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   921,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   164,     0,     0,
       0,   164,     0,     0,   164,   164,     0,     0,   164,   942,
     947,   164,   164,     0,     0,     0,     0,     0,     0,     0,
       0,  1418,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,   118,   118,   118,   118,   118,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   315,   989,     0,     0,   316,   990,
     317,     0,     0,   164,     0,     0,   164,     0,   942,     0,
       0,     0,     0,     0,     0,     0,    57,   318,     0,     0,
       0,     0,     0,     0,     0,     0,   122,   122,   122,   122,
     122,   122,  1032,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1043,   319,   320,     0,   321,     0,   322,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   323,
     324,   325,     0,   326,   327,   328,     0,   329,   330,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,   164,
       1,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,  1015,     0,     0,   164,   331,     0,     0,    75,   408,
     118,   245,     0,     0,     0,   333,  1419,    78,   334,   335,
     336,   337,   266,     1,   269,     0,   271,     0,     0,     0,
       0,  1161,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   245,     0,   269,   271,     0,   118,
       0,   122,     0,     0,     0,     0,  1084,  1085,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1143,  1144,  1145,
     401,     0,  1147,     0,     0,   118,     0,   118,     0,     0,
       0,     0,   164,     0,     0,     0,     0,     0,   245,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,  1181,     0,     0,     0,     0,
     122,     0,     0,     0,   118,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,  1237,   122,     0,
       0,  1238,     0,     0,     0,     0,     0,     0,   942,     0,
     245,     0,   269,   271,     0,     0,   401,     0,  1251,     0,
       0,   122,     0,     0,     0,  1252,     0,     0,     0,     0,
       0,     0,     0,     0,  1256,   122,  1257,     0,     0,     0,
     245,   122,     0,     0,     0,  1263,   245,     0,     0,     0,
       0,     0,     0,  1271,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1235,     0,     0,     0,     0,  1285,
       0,     0,     0,  1286,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,   144,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1254,     0,     0,     0,     0,     0,     0,     0,     0,
    1258,  1259,  1260,  1261,     0,     0,   164,   164,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   245,     0,  1275,
       0,     0,   162,   693,     0,   271,     0,     0,     0,     0,
       0,   122,   164,   164,     0,     0,     0,     0,     0,     0,
    1289,     0,  1290,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     245,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   245,     0,   693,   271,
       0,     0,     0,   273,     0,  1346,   164,     0,     0,   164,
     164,     0,   164,     0,   164,   164,   279,     0,   280,     0,
       0,     0,  1407,     0,     0,     0,     0,     0,     0,   118,
       0,  1360,     0,   245,     0,     0,     0,  1364,     0,  1366,
    1368,     0,   122,     0,     0,   118,  1430,     0,     0,     0,
       0,   164,     0,   245,     0,   164,     0,     0,   245,     0,
     245,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     245,   118,   245,   245,  1401,     0,     0,     0,     0,     0,
     122,     0,  1469,  1408,     0,  1409,  1470,  1410,     0,  1412,
     245,     0,     0,     0,  1420,     0,   122,     0,     0,   489,
     490,     0,   245,   494,     0,     0,   497,   498,     0,     0,
       0,     0,     0,     0,  1435,  1436,  1505,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1515,
    1517,     0,   122,     0,     0,     0,     0,     0,     0,     0,
       0,  1462,  1463,     0,     0,     0,     0,     0,     0,  1466,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1551,     0,     0,  1554,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   212,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   164,     0,
       0,     0,     0,     0,     0,     0,     0,  1575,     0,     0,
     245,     0,   693,   271,     0,     0,     0,     0,  1581,  1541,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   245,   693,     0,     0,     0,     0,
       0,   245,     0,  1597,   662,     0,     0,  1598,     0,   164,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   694,
       0,  1602,  1603,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1579,     0,     0,     0,
       0,     0,     0,     0,  1584,     0,  1586,     0,   164,     0,
     731,   164,     0,     0,     0,     0,     0,     0,  1593,  1594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1599,  1600,     0,  1601,     0,     0,     0,
       0,     0,     0,     0,  1604,     0,     0,     0,     0,     0,
    1609,     0,     0,     0,     0,     0,  1614,  1615,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   800,     0,     0,
    1687,  1688,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   245,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   245,     0,     0,   543,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
     164,     0,     0,     0,     0,    57,     0,  1701,  1702,     0,
       0,     0,     0,     0,     0,   164,     0,  1708,     0,   245,
       0,     0,     0,   164,   164,     0,     0,     0,     0,     0,
    1721,  1768,     0,   245,     0,     0,     0,  1722,  1723,    62,
      63,     0,   164,     0,   245,   164,     0,   164,   164,     0,
       0,     0,     0,   245,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1554,     0,     0,     0,     0,     0,     0,
       0,  1793,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   927,   928,     0,     0,     0,     0,    75,   164,     0,
       0,     0,     0,     0,   245,     0,     0,     0,  1809,     0,
       0,     0,     0,     0,     0,     0,     0,   953,   954,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1825,
     961,     0,  1826,  1784,     0,  1785,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1791,  1792,     0,     0,
       0,     0,     0,  1794,     0,     0,     0,     0,     0,     0,
     983,     0,     0,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1004,     0,     0,  1007,  1008,     0,  1011,     0,  1013,
    1014,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,     0,     0,  1895,     0,     0,
       0,     0,     0,     0,     0,     0,  1055,     0,     0,     0,
    1059,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   245,     0,   432,   388,     0,     0,   164,     0,  1864,
       0,     0,     0,     0,     0,     0,  1872,     0,     0,     0,
       0,     0,  1876,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   488,     0,     0,   164,     0,     0,
       0,   488,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,     0,     0,     0,     0,     0,   164,
    1908,     0,     0,     0,     0,   315,     0,   245,     0,   316,
       0,   317,     0,   245,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1925,     0,     0,     0,   318,     0,
       0,     0,  1930,     0,     0,     0,     0,     0,     0,   488,
       0,     0,     0,     0,     0,     0,     0,  1943,     0,     0,
     164,     0,     0,     0,     0,   319,   320,     0,   321,     0,
     322,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     323,   324,   325,  1220,   326,   327,   328,     0,   329,   330,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     245,     0,     0,     0,     0,   488,   331,   775,     0,    75,
     408,     0,     0,     0,   388,   679,   333,    77,    78,   334,
     335,   336,   337,     0,  1220,     0,     0,     0,   164,   164,
       0,     0,     0,     0,     0,   700,   401,     0,     0,     0,
     164,     0,     0,     0,     0,     0,     0,     0,   245,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   488,   733,
       0,   488,   736,  1278,     0,     0,  1282,     0,     0,   388,
       0,   360,     0,   679,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   437,     0,   488,     0,     0,     0,   488,     0,
       0,     0,     0,     0,     0,     0,   461,     0,     0,     0,
       0,     0,   164,     0,     0,     4,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
     388,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,   164,    56,
     488,     0,    57,   388,     0,     0,     0,  -334,  -334,     0,
       0,     0,   555,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1403,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
    1414,     0,     0,   388,     0,     0,     0,     0,  1423,  1424,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,  1439,     0,     0,
    1443,     0,  1446,  1447,     0,     0,     0,   164,     0,     0,
    -334,     0,     0,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,     0,   488,   488,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   937,   388,     0,     0,
       0,     0,     0,  1474,     0,     0,     0,   679,     0,     0,
       0,   679,     0,     0,     0,     0,     0,     0,   955,     0,
     388,     0,     0,   245,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   245,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   488,   985,
       0,   488,   988,     0,     0,     0,     0,     0,     0,     0,
    1548,   461,     0,   388,   776,     0,   679,     0,   679,   679,
       0,     0,     0,     0,     0,   679,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   388,   388,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   199,     0,     0,     0,
       0,     0,   388,     0,   245,     0,   488,     0,     0,     0,
     488,     0,   253,     0,   488,  1057,     0,     0,   488,  1061,
       0,     0,     0,     0,     0,   245,  1064,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1443,     0,     0,     0,     0,     0,     0,     0,
       0,   844,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   199,     0,     0,     0,   301,     0,     0,   388,   488,
       0,     0,  1611,     0,     0,   199,   393,     0,     0,     0,
       0,     0,   860,     0,     0,     0,     0,     0,     0,   388,
       0,     0,     0,   199,     0,     0,     0,     0,     0,     0,
     776,   880,     0,   245,     0,     0,   452,     0,     0,   456,
       0,   890,     0,   895,   890,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   899,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   488,     0,
       0,     0,     0,     0,     0,  1685,     0,     0,   199,     0,
       0,     0,   933,     0,     0,   437,     0,     0,     0,     0,
       0,   253,     0,   679,     0,     0,     0,     0,   949,     0,
       0,     0,     0,     0,     0,   245,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   456,     0,     0,
       0,     0,     0,     0,     0,   199,   679,   679,     0,     0,
       0,   979,     0,   679,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1734,  1735,     0,     0,     0,   437,     0,
       0,   880,     0,     0,     0,  1739,   598,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     199,     0,     0,     0,     0,     0,   388,     0,     0,     0,
       0,   488,  1280,   461,   488,  1284,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   672,     0,   689,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,  1065,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,  1802,     0,     0,
     369,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,   672,   360,     0,     0,    57,     0,   757,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   880,     0,     0,
       0,     0,     0,     0,     0,  1174,     0,     0,     0,     0,
       0,     0,   890,     0,   388,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,   461,
       0,     0,     0,  1853,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,   199,   199,     0,     0,
       0,     0,   452,   488,  1405,     0,     0,     0,     0,     0,
       0,     0,   388,     0,     0,     0,     0,    75,   679,  1416,
    1210,  1212,     0,     0,     0,    77,    78,     0,   437,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   488,  1441,     0,   679,   461,
       0,     0,     0,     0,     0,   393,     0,     0,   890,   388,
     388,     0,     0,     0,     0,     0,     0,     0,     0,   598,
       0,   598,   598,     0,   598,     0,     0,   598,     0,     0,
     598,   598,   598,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   452,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1266,   315,   461,     0,     0,   316,     0,
     317,     0,     0,  1273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   318,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   199,     0,     0,   452,
       0,   941,     0,     0,   319,   320,     0,   321,     0,   322,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   323,
     324,   325,   672,   326,   327,   328,     0,   329,   330,  1338,
    1338,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,   199,     0,     0,     0,     0,     0,     0,
     388,     0,     0,     0,     0,   331,  1177,     0,    75,   408,
       0,     0,     0,  1178,     0,   333,    77,    78,   334,   335,
     336,   337,     0,     0,     0,   452,     0,     0,     0,     0,
       0,     0,   437,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   452,   452,   461,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   890,     0,   452,   776,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   488,     0,     0,     0,
       0,     0,     0,     0,  1411,     0,     0,     0,     0,     0,
    1421,     0,   488,     0,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
     452,  -402,    45,    46,     0,  -402,   199,     0,     0,     0,
     461,     0,  1465,     0,   388,     0,     0,  1468,     0,     0,
       0,   393,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,   890,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   388,   388,     0,     0,     0,
       0,     0,     0,   461,     0,     0,   776,     0,     0,     0,
       0,    72,     0,     0,   488,   488,     0,     0,     0,     0,
       0,     0,     0,     0,   757,     0,     0,     0,     0,     0,
     488,    73,    74,     0,    75,   299,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   461,     0,   776,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   979,     0,
       0,     0,     0,    13,    14,    15,    16,    17,  1582,  1583,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -402,  -402,     0,
    -402,    45,    46,   488,  -402,     0,     0,     0,   452,     0,
       0,   488,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,  1638,     0,     0,   598,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   388,     0,     0,     0,
       0,   488,  1855,     0,     0,   488,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1677,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   488,   253,    75,     0,     0,     0,     0,     0,  1689,
       0,     0,  1691,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   199,     0,     0,     0,   393,     0,     0,     0,
     598,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     488,   488,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   672,     0,    13,    14,    15,    16,
      17,     0,     0,    19,   488,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,     0,
       0,   452,   452,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     598,   598,   598,     0,   598,   598,     0,     0,    62,    63,
       0,   456,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   299,     0,   253,
       0,     0,     0,     0,    77,    78,     0,   890,     0,     4,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,  1089,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,   393,     0,   315,     0,    45,    46,   316,     0,
     317,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,  1090,    57,  1091,    -2,     0,
    1092,     0,     0,  1093,  1094,  1095,  1096,  1097,  1098,  1099,
    1100,  1101,  1102,  1103,  1104,  -281,  1105,  1106,  1107,  1108,
    1109,     0,  1110,     0,   319,   320,    60,   765,     0,   322,
    1111,  1112,    64,    65,    66,    67,    68,    69,    70,   323,
     324,   325,  1113,   326,   327,   328,     0,   329,   330,     0,
       0,     0,     0,     0,   199,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,   331,    74,     0,    75,   332,
       0,     0,     0,   277,     0,   333,    77,    78,   334,   335,
     336,   337,     0,     0,     0,     0,     0,     0,     0,     0,
    -173,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   393,    13,    14,    15,
      16,    17,     0,     0,    19,   598,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -401,  -401,     0,  -401,    45,    46,     0,  -401,     0,
       0,     0,     0,     0,     0,     0,     0,   452,   452,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     4,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,  1089,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   315,   253,    45,    46,   316,     0,   317,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,  1090,    57,  1091,    -2,     0,  1092,
       0,     0,  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,
    1101,  1102,  1103,  1104,  -281,  1105,  1106,  1107,  1108,  1109,
       0,  1110,     0,   319,   320,    60,   765,     0,   322,  1111,
    1112,    64,    65,    66,    67,    68,    69,    70,   323,   324,
     325,  1113,   326,   327,   328,     0,   329,   330,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   253,     0,     0,     0,
       0,     0,     0,     0,   331,    74,     0,    75,   332,     0,
       0,     0,   277,   598,   333,    77,    78,   334,   335,   336,
     337,     0,     0,     0,     0,     0,     0,     0,     0,  -173,
       0,     0,     0,     0,     0,     0,     0,     0,   452,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   598,     0,     0,   456,     4,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     315,     0,    45,    46,   316,     0,   317,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,   318,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     319,   320,    60,   321,     0,   322,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   323,   324,   325,     0,   326,
     327,   328,     0,   329,   330,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,     0,     0,    75,   332,     0,     0,     0,     0,
       0,   333,    77,    78,   334,   335,   336,   337,     0,     0,
       0,     0,     0,     0,     0,  1662,  1663,     4,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   315,     0,    45,    46,   316,     0,   317,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,   318,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   319,   320,    60,   321,     0,   322,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   323,   324,   325,
       0,   326,   327,   328,     0,   329,   330,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   331,     0,     0,    75,   332,     0,     0,
       0,     0,     0,   333,    77,    78,   334,   335,   336,   337,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   315,     0,    45,    46,   316,     0,
     317,   369,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   318,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   319,   320,     0,   321,     0,   322,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   323,
     324,   325,     0,   326,   327,   328,     0,   329,   330,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   331,     0,     0,    75,   434,
       0,     0,     0,     0,     0,   333,   435,    78,   334,   335,
     336,   337,   241,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   315,     0,    45,    46,
     316,     0,   317,   369,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   318,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   319,   320,     0,   321,
       0,   322,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   323,   324,   325,     0,   326,   327,   328,     0,   329,
     330,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   331,     0,     0,
      75,  1207,     0,     0,     0,     0,     0,   333,  1208,    78,
     334,   335,   336,   337,   241,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   315,     0,
      45,    46,   316,     0,   317,   369,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   318,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   319,   320,
       0,   321,     0,   322,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   323,   324,   325,     0,   326,   327,   328,
       0,   329,   330,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   331,
       0,     0,    75,   408,     0,     0,     0,     0,     0,   333,
      77,    78,   334,   335,   336,   337,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     315,     0,    45,    46,   316,     0,   317,   369,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   318,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     319,   320,     0,   321,     0,   322,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   323,   324,   325,     0,   326,
     327,   328,     0,   329,   330,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,     0,     0,    75,   434,     0,     0,     0,     0,
       0,   333,    77,    78,   334,   335,   336,   337,     4,     5,
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
       0,     0,     0,     0,     0,    77,    78,   240,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,    61,    45,    46,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,   242,     0,     0,
       0,  -708,     0,     0,    77,    78,     4,   241,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,     0,    57,     0,     0,     0,     0,  -335,  -335,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -335,     0,     0,     0,    75,    76,     0,     0,     0,
       0,     0,     0,    77,    78,   240,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
       0,  -402,    45,    46,     0,  -402,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   242,     0,     0,  1298,     0,
       0,     0,    77,    78,  1299,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,  1300,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1301,     0,     0,     0,    75,   856,     0,
       0,  1298,     0,     0,     0,    77,    78,  1299,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,  1300,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1480,     0,     0,     0,
      75,   856,     0,     0,  1298,     0,     0,     0,    77,    78,
    1299,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,  1300,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1481,
       0,     0,     0,    75,   856,     0,     0,  1298,     0,     0,
       0,    77,    78,  1299,     0,     0,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,  1300,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1482,     0,     0,     0,    75,   856,     0,     0,
       0,     0,     0,     0,    77,    78,   240,   241,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -402,
    -402,     0,  -402,    45,    46,     0,  -402,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -402,
    -402,     0,  -402,    45,    46,     0,  -402,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   242,     0,     0,     0,
       0,     0,     0,    77,    78,   241,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -402,  -402,     0,
    -402,    45,    46,     0,  -402,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1322,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,   242,     0,     0,     0,  -712,   315,
       0,    77,    78,   316,     0,   317,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1090,     0,   318,     0,     0,  1092,  1741,  1742,  1093,  1094,
    1095,  1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1104,
    -281,  1105,  1106,  1107,  1108,  1109,     0,  1110,     0,   319,
     320,     0,   765,     0,   322,  1111,  1112,    64,    65,    66,
      67,    68,    69,    70,   323,   324,   325,  1113,   326,   327,
     328,     0,   329,   330,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,    74,     0,    75,   408,     0,     0,     0,   277,     0,
     333,    77,    78,   334,   335,   336,   337,     0,     0,     0,
       0,     0,     0,     0,     0,  -173,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
       0,  -402,    45,    46,     0,  -402,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,  1322,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,   315,     0,     0,     0,   316,
       0,   317,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   242,  1090,     0,   318,    -2,
       0,  1092,    77,    78,  1093,  1094,  1095,  1096,  1097,  1098,
    1099,  1100,  1101,  1102,  1103,  1104,  -281,  1105,  1106,  1107,
    1108,  1109,     0,  1110,     0,   319,   320,     0,   765,     0,
     322,  1111,  1112,    64,    65,    66,    67,    68,    69,    70,
     323,   324,   325,  1113,   326,   327,   328,     0,   329,   330,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,  1322,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   331,    74,     0,    75,
     408,     0,     0,     0,   277,     0,   333,    77,    78,   334,
     335,   336,   337,   315,     0,     0,     0,   316,     0,   317,
       0,  -173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1090,     0,   318,     0,     0,  1092,
       0,     0,  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,
    1101,  1102,  1103,  1104,  -281,  1105,  1106,  1107,  1108,  1109,
       0,  1110,     0,   319,   320,     0,   765,     0,   322,  1111,
    1112,    64,    65,    66,    67,    68,    69,    70,   323,   324,
     325,  1113,   326,   327,   328,     0,   329,   330,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   331,    74,     0,    75,   408,     0,
       0,     0,   277,     0,   333,    77,    78,   334,   335,   336,
     337,     0,     0,     0,     0,     0,     0,     0,     0,  -173,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   369,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,  1024,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -583,    75,   370,
       0,     0,    62,    63,     0,     0,    77,    78,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   369,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   369,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,  1717,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   370,     0,     0,
      62,    63,     0,     0,    77,    78,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    75,     0,
       0,     0,    45,    46,     0,     0,     0,   369,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,  1719,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   370,     0,     0,     0,     0,
       0,     0,    77,    78,   241,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   369,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   370,     0,     0,     0,     0,     0,     0,
      77,    78,   241,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   369,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   299,     0,     0,     0,     0,     0,     0,    77,    78,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -402,  -402,     0,  -402,    45,    46,     0,  -402,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   242,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,   585,    19,   586,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   315,     0,    45,    46,   316,     0,
     317,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   318,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   587,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   319,   320,     0,   321,     0,   322,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   323,
     324,   325,     0,   326,   327,   328,     0,   329,   330,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   331,     0,     0,    75,   588,
       0,     0,     0,   277,     0,   333,    77,    78,   589,   590,
     336,   337,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   315,     0,
      45,    46,   316,     0,   317,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   318,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   319,   320,
       0,   321,     0,   322,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   323,   324,   325,     0,   326,   327,   328,
       0,   329,   330,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   331,
       0,   414,    75,   332,     0,     0,     0,     0,     0,   333,
      77,    78,   334,   335,   336,   337,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   315,     0,    45,    46,   316,     0,   317,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   318,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   319,   320,     0,   321,     0,   322,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   323,   324,   325,
       0,   326,   327,   328,     0,   329,   330,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   331,     0,     0,    75,   588,     0,     0,
       0,   277,     0,   333,    77,    78,   334,   335,   336,   337,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   315,     0,    45,    46,
     316,     0,   317,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   318,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   319,   320,     0,   321,
       0,   322,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   323,   324,   325,     0,   326,   327,   328,     0,   329,
     330,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   331,     0,     0,
      75,   332,     0,     0,     0,     0,     0,   333,    77,    78,
     334,   335,   336,   337,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     315,     0,    45,    46,   316,     0,   317,   369,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   318,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     319,   320,     0,   321,     0,   322,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   323,   324,   325,     0,   326,
     327,   328,     0,   329,   330,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,     0,     0,    75,   434,     0,     0,     0,     0,
       0,   333,    77,    78,   334,   335,   336,   337,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   315,     0,    45,    46,   316,     0,
     317,   369,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   318,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   319,   320,     0,   321,     0,   322,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   323,
     324,   325,     0,   326,   327,   328,     0,   329,   330,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   331,     0,     0,    75,   408,
       0,     0,     0,     0,     0,   333,    77,    78,   334,   335,
     336,   337,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,    76,     0,     0,     0,  -710,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,  1374,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   862,    75,   856,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   856,     0,     0,     0,     0,
       0,     0,    77,    78,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   284,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,     0,
       0,     0,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   369,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   430,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   370,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   369,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   369,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   284,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   430,     0,     0,     0,     0,     0,     0,    77,    78,
     240,   241,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -402,  -402,     0,  -402,    45,    46,     0,
    -402,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,    75,
       0,    45,    46,    62,    63,     0,   369,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   856,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   299,     0,     0,     0,     0,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   369,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   856,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,     0,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   369,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   829,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -596,    75,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     369,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1646,     0,     0,     0,
       0,   241,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,    75,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   369,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    62,    63,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,    75,    47,    48,    49,
      50,    51,    52,    53,    54,   315,     0,     0,     0,   316,
       0,   317,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   318,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   319,   320,     0,   321,     0,
     322,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     323,   324,   325,     0,   326,   327,   328,   315,   329,   330,
       0,   316,     0,   317,     0,     0,    72,     0,     0,     0,
       0,     0,    74,     0,    75,     0,     0,     0,     0,     0,
     318,     0,     0,     0,     0,     0,   331,     0,     0,    75,
     408,     0,     0,     0,   460,     0,   333,    77,    78,   334,
     335,   336,   337,     0,     0,     0,     0,   319,   320,     0,
     321,     0,   322,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   323,   324,   325,     0,   326,   327,   328,   315,
     329,   330,     0,   316,     0,   317,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   318,     0,     0,     0,     0,     0,   331,     0,
       0,    75,   408,     0,     0,     0,   277,     0,   333,    77,
      78,   334,   335,   336,   337,     0,     0,     0,     0,   319,
     320,     0,   321,     0,   322,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   323,   324,   325,     0,   326,   327,
     328,   315,   329,   330,     0,   316,     0,   317,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   318,     0,     0,     0,     0,     0,
     331,   887,     0,    75,   408,     0,     0,     0,     0,     0,
     333,    77,    78,   334,   335,   336,   337,     0,     0,     0,
       0,   319,   320,     0,   321,     0,   322,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   323,   324,   325,     0,
     326,   327,   328,   315,   329,   330,     0,   316,     0,   317,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   318,     0,     0,     0,
       0,     0,   331,     0,     0,    75,   408,     0,     0,   919,
       0,     0,   333,    77,    78,   334,   335,   336,   337,     0,
       0,     0,     0,   319,   320,     0,   321,     0,   322,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   323,   324,
     325,     0,   326,   327,   328,   315,   329,   330,     0,   316,
       0,   317,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   318,     0,
       0,     0,     0,     0,   331,  1272,     0,    75,   408,     0,
       0,     0,     0,     0,   333,    77,    78,   334,   335,   336,
     337,     0,     0,     0,     0,   319,   320,     0,   321,     0,
     322,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     323,   324,   325,     0,   326,   327,   328,   315,   329,   330,
       0,   316,     0,   317,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     318,     0,     0,     0,     0,     0,   331,     0,     0,    75,
     408,     0,     0,     0,  1332,     0,   333,    77,    78,   334,
     335,   336,   337,     0,     0,     0,     0,   319,   320,     0,
     321,     0,   322,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   323,   324,   325,     0,   326,   327,   328,   315,
     329,   330,     0,   316,     0,   317,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   318,     0,     0,     0,     0,     0,   331,     0,
    1747,    75,   408,     0,     0,     0,     0,     0,   333,    77,
      78,   334,   335,   336,   337,     0,     0,     0,     0,   319,
     320,     0,   321,     0,   322,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   323,   324,   325,     0,   326,   327,
     328,   315,   329,   330,     0,   316,     0,   317,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   318,     0,     0,     0,     0,     0,
     331,  1948,     0,    75,   408,     0,     0,     0,     0,     0,
     333,    77,    78,   334,   335,   336,   337,     0,     0,     0,
       0,   319,   320,     0,   321,     0,   322,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   323,   324,   325,     0,
     326,   327,   328,   315,   329,   330,     0,   316,     0,   317,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   318,     0,     0,     0,
       0,     0,   331,     0,     0,    75,   408,     0,     0,     0,
       0,     0,   333,    77,    78,   334,   335,   336,   337,     0,
       0,     0,     0,   319,   320,     0,   321,     0,   322,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   323,   324,
     325,     0,   326,   327,   328,   315,   329,   330,     0,   316,
       0,   317,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   318,     0,
       0,     0,     0,     0,   574,     0,     0,    75,   408,     0,
       0,     0,     0,     0,   333,    77,    78,   334,   335,   336,
     337,     0,     0,     0,     0,   319,   320,     0,   321,     0,
     322,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     323,   324,   325,     0,   326,   327,   328,   315,   329,   330,
       0,   316,     0,   317,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     318,     0,     0,     0,     0,     0,   579,     0,     0,    75,
     408,     0,     0,     0,     0,     0,   333,    77,    78,   334,
     335,   336,   337,     0,     0,     0,     0,   319,   320,     0,
     321,     0,   322,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   323,   324,   325,     0,   326,   327,   328,   315,
     329,   330,     0,   316,     0,   317,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   318,     0,     0,     0,     0,     0,   582,     0,
       0,    75,   408,     0,     0,     0,     0,     0,   333,    77,
      78,   334,   335,   336,   337,     0,     0,     0,     0,   319,
     320,     0,   321,     0,   322,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   323,   324,   325,     0,   326,   327,
     328,   315,   329,   330,     0,   316,     0,   317,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   318,     0,     0,     0,     0,     0,
     331,     0,     0,    75,   408,     0,     0,     0,     0,     0,
     333,   843,    78,   334,   335,   336,   337,     0,     0,     0,
       0,   319,   320,     0,   321,     0,   322,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   323,   324,   325,     0,
     326,   327,   328,     0,   329,   330,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   331,     0,     0,    75,   408,     0,     0,     0,
       0,     0,   333,   435,    78,   334,   335,   336,   337
};

static const yytype_int16 yycheck[] =
{
       1,     1,    73,     4,     1,   344,     1,   172,   178,   254,
     220,   217,    73,   217,   161,    73,    73,   161,   239,   522,
     743,   706,   331,   281,   205,   217,   371,   149,   218,   687,
     613,   599,  1256,  1257,     4,   745,  1099,    75,   177,   373,
     138,  1621,   217,   388,   501,   502,   388,   392,   217,    94,
     392,   217,   674,   674,    55,    56,   217,    58,    58,   562,
     592,    58,   162,    58,   653,   746,   191,   678,   898,   331,
     892,   752,    73,   674,    70,   838,     1,  1621,   288,   289,
      81,  1621,  1683,  1226,     0,    73,    81,   293,    89,   293,
     745,   436,   190,    94,     1,   763,    97,     4,   313,    94,
     101,   293,    97,   743,   294,    70,   101,   929,   442,   180,
    1741,   779,   234,    73,   822,     1,   743,   177,   293,   180,
       0,   743,   743,   180,   293,    87,    58,   293,     1,   680,
     227,   101,   293,    58,   745,   743,    73,  1622,   139,   146,
     848,   142,   743,   144,   144,     1,   217,   144,   149,   144,
    1745,    58,    70,   149,   155,   252,   217,   153,   149,   217,
     217,   162,    59,    60,   115,   262,   173,    12,    13,    14,
      15,    16,    58,   218,   149,   130,   151,   845,   179,   180,
      95,   161,   173,  1078,   179,    58,    70,     1,   153,   404,
      97,   838,   180,   194,   101,   157,   747,    87,    81,   194,
     751,  1184,    58,   155,   661,   157,  1083,   208,    94,   760,
     761,    97,   144,   762,   763,   101,   217,   218,  1083,   144,
     180,   226,   293,   218,   229,    70,     1,  1858,   101,   217,
     779,   149,   293,   234,   272,   293,   293,   144,     0,   484,
    1083,    97,   243,   180,    58,  1730,   251,   402,   243,   294,
     405,   708,   253,  1148,   209,   256,   261,   217,   144,   174,
     157,   256,   263,   146,   470,   149,   470,   157,    70,   153,
     993,   144,   273,   274,    82,   276,   771,   174,   470,   149,
     217,   149,  1425,    58,   281,   970,   131,   157,   144,   479,
    1000,   549,   293,   294,   120,   470,   845,  1877,   881,   294,
     301,   470,   611,  1898,   470,    70,   307,   308,   194,   470,
      87,   312,   574,   994,   576,   577,   819,   579,   663,  1920,
     582,   663,   149,   585,   586,   587,   152,   282,  1091,   560,
     144,  1926,   666,  1877,   432,   146,   925,  1877,   870,   950,
     440,   149,   687,  1828,    70,  1000,    70,   149,  1178,   256,
     556,   153,   556,   993,    70,   700,   690,  1120,   700,   149,
     243,   176,   173,   697,   556,   704,   993,  1075,   558,   144,
     256,   993,   993,    75,    76,  1043,   331,   499,  1602,  1603,
     157,   556,   507,   505,   149,   993,   157,   556,   153,  1000,
     556,   371,   993,  1878,   977,   556,   397,  1292,    70,   470,
     256,   402,    87,   174,   405,   406,   131,    70,   281,   470,
    1393,  1394,  1395,   470,    19,   416,   560,   300,  1295,  1296,
    1297,   416,    70,   149,   521,   149,  1911,   153,    81,   153,
    1295,  1296,  1297,   149,   479,    70,    96,   153,   439,   440,
     165,  1263,    56,  1860,  1091,    59,    60,   263,    62,   155,
     451,   452,  1295,  1296,  1297,   157,   436,  1874,   274,   460,
     104,   105,   345,   131,    70,   115,  1017,   281,   174,   470,
     155,    70,   157,  1120,   130,   600,   971,   149,   479,    75,
      76,   153,   470,  1900,   479,   556,   149,   492,     1,   149,
     153,     4,    70,   146,  1043,   556,   830,   165,   499,   556,
      70,   149,  1332,   153,   505,   153,   281,   160,   152,   514,
     470,    73,   157,   558,   149,   520,   172,   373,   153,   524,
      12,    13,    14,    15,    16,   178,    88,   155,    70,   174,
     416,   762,   763,   470,  1215,   149,   491,  1222,   149,   664,
      73,   496,   543,   149,   545,    58,   174,   153,   779,  1515,
     149,  1209,   549,  1383,   153,   556,    89,   558,   513,   104,
     105,   157,   155,   558,  1245,   151,  1388,   131,   523,  1793,
     571,   149,   165,   106,  1540,   153,   162,   163,    70,   149,
     560,   146,   155,   153,   125,   126,   442,   152,   101,   149,
     243,   772,   937,   479,   151,   937,   160,   161,   155,   155,
    1663,   174,   603,   604,  1667,   939,   607,   149,   173,   191,
     611,   153,   155,   614,   845,   740,   148,   756,   174,   574,
     149,   659,   165,   155,   579,   149,  1194,   582,   169,   170,
       1,   144,   156,     4,   154,   866,   155,   762,   763,   131,
      12,    13,    14,    15,    16,   745,   601,   300,   149,   118,
     150,   120,   121,   122,   779,   174,   157,   157,   659,   781,
       3,   662,   151,   143,   144,   145,   155,   664,   174,    12,
      13,    14,    15,    16,   547,   155,   549,   151,   800,   155,
     149,   851,   156,   152,   153,   165,   151,    58,   157,   158,
     546,   156,   345,   694,   174,  1758,   756,   148,    70,   174,
    1157,  1196,    73,     3,   155,   706,   155,   687,     3,   129,
      81,     3,    12,    13,    14,    15,    16,   155,   371,   155,
     845,   152,   866,    94,   149,   174,    97,    70,   766,   149,
     101,  1314,   155,   153,  1229,   549,   174,   129,   174,    70,
     160,   161,   743,   740,   745,   151,  1568,   160,  1570,   157,
     156,   174,  1815,   173,   167,   168,   757,   149,   104,   105,
    1823,   153,   129,   764,   155,   766,   151,  1335,   160,   161,
      70,   156,   151,   144,   549,   151,   155,   153,   149,   780,
     781,   782,   149,   436,  1029,   173,   153,   968,   157,   371,
     149,   162,   374,   160,   161,   633,   634,   635,   171,   800,
      12,    13,    14,    15,    16,   149,   388,  1870,   179,   180,
     392,   149,  1043,   151,   149,   153,   821,  1385,    46,    47,
     149,    49,   151,   194,   153,    53,     4,     5,     6,     7,
       8,     9,    10,    11,   690,   836,   837,   838,   838,    56,
     151,   838,    59,    60,   155,    62,   217,   218,   501,   502,
     115,   806,  1062,  1034,   436,   149,   152,   153,    70,   919,
     156,   961,   817,   234,  1209,   820,   151,   149,  1515,   824,
    1517,   153,   243,    12,    13,    14,    15,    16,    17,  1213,
     850,  1480,  1481,  1482,   885,   256,   866,  1226,   149,  1384,
     151,   892,   153,  1540,   149,   151,   151,   898,   153,   999,
    1000,   151,   273,  1165,  1589,   276,   151,   722,   151,     1,
     155,   151,     4,   838,   129,   155,   151,   129,  1043,   151,
     155,   151,   293,   294,   149,   507,   155,  1422,   929,   151,
    1161,   838,   151,   155,   149,   149,   155,   149,   153,   153,
     151,   153,    21,   850,   155,   160,   161,   149,   160,   161,
       4,     5,     6,     7,     8,     9,    10,    11,   149,   149,
     961,   149,   153,   153,   965,   838,    58,   151,   155,   970,
     155,   155,  1032,   151,   830,   976,   162,   163,   560,   143,
     144,   145,   838,   151,   149,   841,   801,   155,   153,    81,
    1299,   155,   993,  1158,  1159,   155,   174,  1218,   999,  1000,
     155,   165,    73,   108,   109,   110,   111,   112,   661,    63,
     174,    12,    13,    14,    15,    16,    17,  1161,   600,   154,
     155,  1706,   151,    94,   838,  1256,   155,  1522,    96,  1151,
     149,   151,    17,  1034,   687,   155,  1037,   151,   148,   151,
     151,   155,  1287,   155,   155,   416,   138,   123,   124,   151,
     149,   101,   144,   155,   146,   708,   106,   107,   108,   109,
     110,   111,   112,   838,   151,   127,   128,  1562,   155,   440,
      55,    56,    57,    58,    59,    60,    61,    62,   149,   157,
    1882,   663,   664,   939,  1886,   157,  1425,    87,  1269,   460,
     157,  1091,  1093,   149,  1091,  1096,  1097,  1098,   190,   470,
     150,   149,   151,   153,   149,   687,   155,   151,   479,   151,
    1257,   155,   151,   155,  1761,  1305,   155,   151,   700,  1120,
    1120,   155,  1077,  1120,   115,  1126,   151,   158,   499,   158,
     155,  1256,  1257,  1134,   505,  1090,  1137,  1138,  1198,   166,
    1141,   151,  1137,  1138,   161,   155,   217,   218,   240,   151,
    1151,   243,  1107,   155,   640,   641,   642,   643,   740,   154,
     155,  1162,  1809,   234,   160,   161,  1091,   159,  1138,  1080,
    1081,  1082,   543,   171,   545,   129,  1407,  1178,  1825,   154,
     155,  1161,   152,  1184,  1091,   556,   154,   558,   151,   281,
     154,   155,   157,  1776,   173,  1120,   154,   155,   851,   151,
    1083,    12,    13,    14,    15,    16,    17,   155,   300,   154,
     155,  1271,  1707,  1120,  1861,   155,   156,   151,  1091,   154,
     155,  1222,   293,   294,   154,   155,   154,   155,   157,  1209,
    1137,  1138,   603,   604,   157,  1091,   607,   154,   155,   151,
     611,   154,   155,   614,   151,  1246,   151,  1120,   154,   155,
     151,  1137,  1138,   345,   154,   155,   151,  1515,   154,   155,
    1305,   101,  1263,  1407,  1120,  1138,   106,   107,   108,   109,
     110,   111,   112,   113,   154,   155,   153,  1091,   154,   155,
     154,  1137,  1540,   131,   866,   154,   155,  1493,   659,  1493,
     149,   662,   131,  1788,   154,   155,   154,  1487,  1560,   154,
     155,  1493,   155,   156,  1305,  1495,  1120,   151,  1309,   151,
     150,  1312,   149,   153,   155,   156,  1091,   151,  1493,  1489,
     151,  1460,  1461,   694,  1493,   838,   153,  1493,  1298,   154,
     155,  1332,  1493,   154,   155,    75,    76,   850,   151,   102,
     432,   155,   156,   106,   151,  1120,   109,   157,   111,  1185,
    1186,  1352,   157,  1354,   157,   937,   157,  1213,   940,  1354,
    1315,  1316,    12,    13,    14,    15,    16,   636,   637,   440,
      68,  1602,   743,   154,   745,   638,   639,   644,   645,   149,
    1505,   157,  1383,  1500,  1501,  1158,  1159,  1388,   157,   151,
     151,  1298,  1393,  1394,  1395,   766,    76,   154,  1353,   470,
    1460,  1461,    17,   155,  1551,   173,   157,  1551,   479,   780,
     781,   782,  1295,  1296,  1297,   149,  1299,  1300,   174,   174,
      70,   154,   154,  1768,    17,   156,   156,  1407,   499,   800,
    1636,   156,  1636,   155,   505,  1493,    12,    13,    14,    15,
      16,    17,  1487,   151,  1636,   151,   151,  1354,   151,  1639,
    1495,   543,   151,   148,   151,   151,  1603,   549,   151,   151,
    1430,  1636,    68,   157,   174,   836,   157,  1636,  1354,   157,
    1636,   173,   151,   155,   545,  1636,   151,  1602,  1603,   129,
     151,  1826,   155,   246,   151,   556,  1487,   558,   151,   151,
     151,   151,  1493,   151,  1495,  1829,   151,   151,  1354,   149,
     151,  1502,   151,   153,  1157,   151,   151,   148,   151,   157,
     160,   161,   151,  1514,   885,  1516,   151,   151,  1515,   151,
    1517,   892,   151,  1430,   154,   151,   151,   898,    12,    13,
      14,    15,    16,     4,     5,     6,     7,     8,     9,    10,
      11,   148,   173,  1540,   148,   155,   151,  1548,   151,  1739,
     149,   149,   149,   149,     1,   149,  1209,     4,   929,   149,
      13,   156,    72,   155,    89,   174,  1688,  1568,   156,  1570,
     154,  1551,   174,   154,   174,   157,  1531,   148,  1091,  1161,
     148,   155,   151,   174,  1554,   174,    70,   154,  1589,   151,
     961,   155,   155,   151,  1639,   151,   151,   148,   148,   174,
     149,   174,  1741,   149,    78,   148,   151,  1120,  1515,   151,
    1517,    58,   375,   376,   174,   378,   149,   380,   149,   151,
     174,   174,   993,   151,   148,  1138,   174,  1209,   999,  1000,
     174,   148,   155,  1540,    81,  1636,   174,   174,  1639,   155,
     118,   156,  1515,   156,  1517,   129,  1793,  1554,  1649,   148,
     151,   154,   154,   157,   101,   151,   154,   154,   148,  1515,
     151,  1517,   151,  1034,   151,   149,  1037,  1540,  1793,   153,
    1671,  1877,   743,  1877,   745,   155,   160,   161,   154,   151,
     148,  1741,   174,   156,  1540,  1877,  1687,  1688,  1878,   149,
    1660,   138,   149,   107,  1739,   149,   148,   144,   157,   146,
     148,  1515,  1877,  1517,   154,  1706,  1828,   151,  1877,   154,
     781,  1877,   154,   160,   151,   151,  1877,   151,  1857,  1858,
     155,  1911,     1,   154,  1539,     4,  1540,   154,   151,   800,
     151,   178,   151,   148,   174,   151,   149,    88,  1739,   151,
    1515,   148,  1517,   190,  1745,  1884,   838,   148,  1749,   154,
     154,   151,  1659,  1660,   151,   518,   154,   151,   850,  1760,
     151,   151,   151,  1134,  1761,  1540,  1137,  1138,   153,   156,
     156,   174,  1773,   151,   151,   174,   148,   151,   174,    58,
    1151,   174,   174,   151,   155,  1298,   174,  1660,   152,   156,
     174,  1162,   101,   240,    73,   149,   243,  1857,  1858,   155,
      73,   248,    81,  1659,   149,   148,  1877,  1178,   150,   150,
     154,   174,  1809,  1184,   174,    94,  1877,  1956,    97,  1877,
    1877,   107,   101,   107,  1884,  1407,   165,  1828,  1825,  1830,
     165,   151,   156,  1878,   281,   151,  1489,   148,   148,   151,
    1841,   149,  1843,  1844,  1751,   174,    73,   151,   151,  1909,
     333,   174,   174,   300,  1761,  1560,   591,   807,  1191,   138,
     646,   648,   647,  1678,  1861,   144,  1911,   146,   650,   649,
     149,   150,  1109,  1120,  1926,  1246,  1877,  1878,  1540,  1858,
    1874,   160,   161,  1878,   331,  1768,  1670,  1888,  1761,  1761,
    1818,  1921,  1263,  1920,  1908,  1751,  1956,  1898,   345,   178,
     179,   180,  1809,  1777,  1826,  1761,  1532,  1532,  1825,  1887,
    1911,   190,   191,  1938,  1141,   194,  1911,  1430,  1825,    48,
     248,  1487,   993,  1924,  1802,  1926,  1739,  1501,   999,  1000,
    1300,  1134,   854,  1034,  1305,   773,  1809,  1430,   217,   218,
    1941,  1554,     0,  1826,   571,  1946,   976,  1761,   652,    -1,
      -1,   652,  1825,  1809,  1861,   234,    -1,  1772,    -1,  1960,
      -1,  1332,   652,  1964,   243,    -1,    -1,    -1,    -1,  1825,
      -1,   921,    -1,  1974,    -1,    -1,    -1,   256,  1933,    -1,
      -1,    -1,    -1,  1354,    -1,   432,  1761,    -1,  1861,    -1,
      -1,  1083,   942,    -1,    -1,  1809,    -1,  1952,    -1,  1091,
      -1,    -1,  1515,    -1,  1517,  1861,    -1,   286,    -1,    -1,
      -1,  1825,  1383,   292,   293,   294,    -1,  1388,    -1,    -1,
      -1,   300,  1393,  1394,  1395,    -1,    -1,  1540,  1120,    -1,
      -1,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,   989,
     990,  1554,    -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,
    1825,    -1,   331,    -1,   501,   502,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   345,  1882,   101,    -1,
      -1,  1886,  1887,   106,   107,   108,   109,   110,   111,   112,
    1151,    -1,   538,   362,    12,    -1,  1861,    -1,    -1,    -1,
      -1,    -1,   371,   372,   373,    -1,   543,    -1,  1913,   331,
     547,    -1,   549,    -1,    -1,    -1,   318,    -1,    -1,   388,
      -1,    -1,    -1,   392,    -1,    -1,  1487,   150,   228,  1934,
      -1,    -1,  1493,  1938,  1495,    -1,    -1,   574,    -1,   576,
     577,    -1,   579,   345,   346,   582,    -1,   416,   585,   586,
     587,    -1,    -1,    -1,    -1,    -1,    -1,  1660,  1963,    -1,
      -1,    -1,    -1,   432,    -1,    -1,    -1,   436,    86,    -1,
      97,   440,    -1,   442,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,   101,    -1,  1246,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,  1768,    -1,    -1,    -1,
      -1,   470,    -1,    -1,    -1,    -1,    -1,  1568,   410,  1570,
     479,    -1,    -1,  1295,  1296,  1297,  1298,  1299,  1300,    -1,
      -1,    -1,    -1,   150,   661,    -1,    -1,    -1,    -1,    -1,
     499,    -1,   501,   502,   436,    -1,   505,    -1,   507,    -1,
       9,   101,    -1,    -1,  1305,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,  1826,    -1,    -1,   117,  1761,   119,
      -1,    -1,   362,    -1,    -1,    -1,    -1,   194,    -1,    -1,
      -1,   708,    -1,    -1,    -1,  1636,   545,    -1,  1639,   501,
     502,    -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,   558,
     150,   560,    -1,   153,    -1,    -1,    -1,  1237,  1238,    -1,
      -1,    -1,    -1,    -1,    -1,   574,  1809,   576,   577,    -1,
     579,  1251,  1252,   582,    -1,    -1,   585,   586,   587,    -1,
      -1,    -1,  1825,    -1,    -1,   425,    -1,  1688,   428,   256,
      -1,   600,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,  1285,  1286,    -1,  1430,    -1,
      -1,    -1,   574,    -1,    -1,    -1,    -1,   579,  1861,   286,
     582,   807,    -1,   101,    -1,   292,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   475,    -1,    -1,  1739,   601,
      -1,   150,    -1,   652,   653,     9,    -1,    -1,    -1,    -1,
      -1,    -1,   661,    -1,   663,   664,    -1,   666,    -1,    -1,
      -1,   838,    -1,    -1,    -1,   674,    -1,    -1,    -1,   678,
      -1,    -1,   150,   850,   851,    -1,    -1,    -1,   687,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1487,    -1,   697,   651,
     632,   700,  1493,  1515,  1495,  1517,   174,    -1,    -1,   708,
      -1,    -1,   101,    -1,    -1,    -1,   373,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,  1540,    63,
      64,    65,    66,    -1,   177,    -1,    -1,  1828,    -1,  1830,
      -1,   740,  1554,    -1,   743,   921,   745,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1093,
     149,   150,    -1,   762,   763,    -1,   942,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     779,    -1,   781,   101,    -1,   442,  1877,  1878,   106,   107,
     108,   109,   110,   111,   112,    -1,   150,    -1,    -1,  1469,
    1470,   800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,   989,   990,    -1,   150,    -1,    -1,   153,
    1911,    -1,   652,   653,    -1,    76,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,    -1,   171,    -1,   838,
      -1,    -1,   160,   161,    -1,  1636,   845,    -1,  1639,   506,
     101,   850,   851,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,   866,    -1,   526,
      -1,    -1,   315,  1964,     3,   318,   319,    -1,    -1,    -1,
      -1,    -1,    -1,  1974,    -1,    -1,   329,   330,    -1,   546,
      -1,    -1,    -1,    -1,    -1,   725,    -1,  1688,   728,   150,
      -1,    -1,   345,   346,    -1,  1575,    -1,    -1,    -1,    -1,
      -1,  1581,    -1,    -1,    -1,    -1,  1083,    -1,    -1,    -1,
      -1,    -1,    -1,   174,  1091,    -1,   925,  1597,  1598,    -1,
     862,    -1,    -1,    -1,    -1,   867,    -1,    -1,   937,    -1,
     939,    -1,    -1,    -1,    -1,    -1,   878,    -1,  1739,  1761,
      -1,   950,    -1,  1120,    -1,    -1,    -1,   787,    -1,    -1,
      -1,   791,    -1,    17,    -1,   795,    -1,   410,    -1,    -1,
      -1,  1138,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
    1157,    -1,    -1,   436,   993,    -1,    -1,  1809,  1165,    -1,
     129,  1000,    -1,    -1,    -1,    59,    60,    61,    62,   666,
      -1,    -1,    -1,  1825,    -1,    -1,    -1,    -1,  1352,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
      -1,   160,   161,   690,    -1,    -1,    -1,  1828,    -1,    -1,
     697,    -1,    -1,    -1,  1043,    -1,    -1,   101,    -1,  1861,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,  1237,  1238,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1251,  1252,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1083,    -1,  1877,  1878,    -1,    -1,
      -1,    -1,  1091,    -1,    -1,   925,   150,   101,    -1,   153,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1285,
    1286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1911,  1120,    -1,    -1,    -1,   129,    -1,    -1,  1295,  1296,
    1297,  1298,  1299,  1300,    -1,    -1,    -1,    -1,  1137,  1138,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,   153,
      -1,    -1,  1151,    -1,    -1,    -1,   160,   161,  1157,  1091,
      -1,    -1,  1161,    -1,    -1,    -1,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,   830,    -1,     1,    -1,    -1,     4,    -1,
    1514,    -1,  1516,    -1,   841,    -1,    -1,    -1,    -1,   632,
     633,   634,   635,   636,   637,   638,   639,   640,   641,   642,
     643,   644,   645,   646,   647,   648,   649,   650,    -1,    -1,
    1209,    70,    -1,    -1,  1548,    -1,    -1,    -1,  1048,    -1,
      -1,  1051,    -1,    -1,    -1,  1895,    -1,    -1,    -1,    -1,
      -1,    -1,    58,    -1,    -1,    -1,  1168,  1169,  1170,    -1,
      -1,    -1,   101,  1175,  1176,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    81,    -1,  1256,  1257,    -1,
      -1,    -1,    -1,  1430,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,   101,    -1,  1209,    -1,    -1,
      -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,  1469,  1470,    -1,  1295,  1296,  1297,  1298,
    1299,  1300,    -1,    -1,    -1,    -1,  1305,  1306,    -1,    -1,
      -1,    -1,   138,   756,    -1,  1649,    -1,    -1,   144,    -1,
     146,    -1,  1489,    -1,   150,    -1,    -1,    -1,    -1,  1505,
      -1,    -1,    -1,    -1,   160,   161,   162,  1671,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1515,    -1,
    1517,    -1,   178,    -1,    -1,  1354,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   190,   191,    -1,    -1,   194,  1321,
    1200,    -1,    -1,  1540,    -1,    -1,    -1,    -1,  1330,    -1,
      -1,    -1,  1334,    -1,  1336,    -1,    -1,  1554,    -1,    -1,
      -1,    -1,    -1,  1560,    -1,    -1,    -1,    -1,    -1,  1575,
      -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,  1407,    -1,
      -1,  1745,  1242,    -1,   240,  1749,    -1,   243,    -1,   862,
      -1,  1597,  1598,    -1,   867,    -1,  1760,    -1,    -1,    -1,
     256,  1430,    -1,    -1,    -1,   878,    -1,    -1,    -1,  1773,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   273,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   281,    -1,    -1,    -1,    -1,
     286,    -1,    -1,    -1,    -1,    -1,   292,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   300,    -1,   919,    -1,    -1,    -1,
    1137,    -1,    -1,    -1,  1483,    -1,    -1,    -1,  1487,    -1,
    1489,    70,    -1,  1660,  1493,    -1,  1495,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   331,    -1,  1841,    -1,  1843,
    1844,    -1,    -1,    -1,    -1,    -1,  1515,    -1,  1517,   345,
      -1,    -1,   101,  1475,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    70,    -1,
      -1,  1540,    -1,    -1,    -1,   371,    -1,   373,   374,    -1,
     129,    -1,  1551,    -1,  1888,  1554,  1213,    -1,    -1,    -1,
      -1,  1560,   388,  1515,  1898,  1517,   392,    -1,    -1,   101,
     149,   150,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   160,   161,    -1,    -1,    -1,    -1,    -1,  1540,    -1,
    1924,    -1,  1926,    -1,  1761,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,  1602,  1603,    -1,   432,  1941,    -1,    -1,
     436,    -1,  1946,  1545,    -1,    -1,   442,   149,   150,    -1,
      -1,    -1,  1621,  1622,    -1,    -1,  1960,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1636,    -1,    -1,
    1639,    -1,  1809,    -1,    -1,    -1,    -1,    -1,  1091,  1306,
      -1,    -1,    -1,  1483,    -1,    -1,    -1,    -1,  1825,    -1,
    1659,  1660,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   501,   502,    -1,    -1,    -1,
     506,   507,    -1,    12,    13,    14,    15,    16,    -1,  1688,
      -1,    -1,    -1,    -1,  1861,    -1,    -1,  1354,    -1,  1651,
      -1,    -1,    -1,  1655,    -1,    -1,    -1,    -1,    -1,    -1,
    1662,   150,   538,    -1,    -1,    -1,    -1,   543,    -1,  1895,
     546,   547,    -1,   549,    -1,  1168,  1169,  1170,    -1,    -1,
      -1,  1730,  1175,  1176,   560,   174,    -1,    -1,    -1,    -1,
    1739,    70,    -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,
     576,   577,  1751,   579,    -1,  1198,   582,    -1,    -1,   585,
     586,   587,  1761,    -1,    -1,    -1,  1209,    -1,    -1,  1768,
      -1,    -1,   101,    -1,   600,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1621,  1622,    -1,  1793,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,  1802,    -1,    -1,  1758,  1759,    70,  1761,
    1809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,    -1,   153,    -1,  1825,  1826,    -1,  1828,
    1829,   160,   161,    -1,    -1,   661,   662,   663,   664,   101,
     666,    -1,    -1,  1775,   106,   107,   108,   109,   110,   111,
     112,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,   687,  1861,    -1,   690,  1817,    -1,   129,   694,    -1,
      -1,   697,    -1,    -1,   700,    -1,   702,    -1,  1877,  1878,
      -1,    -1,   708,    -1,    -1,   101,     1,   149,   150,     4,
     106,   107,   108,   109,   110,   111,   112,   113,   160,   161,
    1730,   117,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,  1911,    -1,   740,    -1,    -1,  1869,    -1,  1871,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   150,    -1,   762,   763,    -1,  1891,
      -1,   101,    -1,    58,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   779,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    81,    -1,  1798,   129,
     150,    -1,  1802,    -1,    81,  1927,  1928,  1929,    -1,    -1,
     160,   807,    -1,    -1,    -1,    -1,   101,    -1,    -1,   149,
     150,    -1,    -1,   153,    -1,    -1,    -1,    -1,  1828,    -1,
     160,   161,  1659,   150,   830,   152,    -1,    -1,    -1,    -1,
     157,    -1,   838,    -1,    -1,   841,    -1,  1460,  1461,   845,
      -1,    -1,    -1,   138,   850,   851,    -1,    -1,    -1,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
     866,    -1,    -1,    -1,    -1,    -1,   161,  1877,  1878,    -1,
      -1,    -1,   101,   160,   161,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   179,    -1,    -1,    -1,    -1,    -1,
      -1,   178,    -1,    -1,    -1,   190,   191,    -1,    -1,    -1,
      -1,  1911,    -1,    -1,   191,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,  1751,   921,    -1,    -1,    -1,    -1,
      -1,   150,  1545,   218,    -1,    -1,    -1,    -1,   157,    -1,
      -1,   937,    -1,   939,   940,    -1,   942,    -1,    -1,   234,
      -1,    -1,    -1,    -1,   239,   240,    -1,    -1,   243,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   243,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
     265,    -1,    -1,   268,    -1,   270,    -1,    -1,    -1,    -1,
      70,    -1,    -1,   989,   990,    -1,   281,    -1,    -1,    -1,
      -1,    -1,  1829,    -1,    -1,    -1,    -1,   101,    -1,   294,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   101,    -1,   300,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   129,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    -1,    -1,  1043,    -1,   129,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,   345,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,   371,  1083,    -1,   374,
      -1,    -1,    -1,    -1,   371,  1091,    70,     1,    -1,    -1,
      -1,    -1,    -1,   388,    -1,    -1,    -1,   392,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,  1120,    -1,    -1,   101,  1741,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,  1137,  1138,    -1,    -1,    -1,    -1,   432,    -1,    -1,
      -1,   436,    -1,    -1,    58,   129,    -1,   150,    -1,   436,
      -1,  1157,  1775,    -1,   157,  1161,    -1,    -1,    -1,  1165,
      -1,    -1,    -1,    -1,   101,   149,   150,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   101,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,  1209,    -1,    -1,    -1,  1213,    -1,    -1,
      -1,    -1,   507,   150,   501,   502,   153,    -1,    -1,  1225,
     507,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
     144,  1237,  1238,    -1,  1857,  1858,   149,    -1,    -1,   152,
     153,    -1,    -1,   538,    -1,  1251,  1252,   161,   543,    -1,
    1256,  1257,   547,    -1,   549,    -1,    -1,    -1,    -1,    -1,
      -1,  1884,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   560,    -1,    -1,    -1,   191,    -1,  1285,
    1286,    70,    -1,    -1,    -1,    -1,  1909,    -1,    -1,  1295,
    1296,  1297,  1298,  1299,  1300,    -1,    -1,    -1,    -1,    -1,
    1306,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   600,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1956,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1354,    -1,
      -1,   265,    -1,    -1,    -1,    55,    56,    -1,   653,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   281,   663,   664,
      -1,   160,   161,    -1,   661,    -1,    -1,   664,    -1,    -1,
      -1,    -1,    -1,   678,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    -1,   687,    -1,    -1,    -1,    -1,   692,    -1,    -1,
     687,  1407,   101,    -1,    -1,   700,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,    -1,   117,    -1,
     119,   708,    -1,    -1,  1430,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,   139,
      -1,    -1,   142,    -1,    -1,   740,    -1,    -1,    -1,    -1,
     745,   150,    -1,   740,   153,   155,    -1,   371,    -1,    -1,
     374,    -1,    -1,  1469,  1470,    -1,    -1,   762,   763,    -1,
      -1,    -1,    -1,    -1,   388,   762,   763,    -1,   392,    -1,
      -1,    -1,    -1,  1489,   779,    -1,    -1,    70,    -1,    -1,
      -1,    -1,   779,    -1,    -1,    -1,    -1,    -1,    -1,  1505,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   208,  1515,
      -1,  1517,   807,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   436,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,  1540,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   838,    -1,  1551,   129,    -1,  1554,    -1,
     845,    -1,    -1,   253,  1560,   850,    -1,    -1,   845,    -1,
      -1,    -1,    -1,   263,   851,    -1,   149,   150,    -1,  1575,
      -1,   866,    -1,    -1,   274,  1581,    -1,   160,   161,   866,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,  1597,  1598,   507,   146,    -1,  1602,  1603,    -1,    -1,
      -1,   301,    -1,    -1,    -1,    -1,    -1,   307,   308,    48,
      -1,    -1,   312,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,   173,    -1,    -1,   538,    -1,   921,    -1,    -1,    -1,
      -1,    -1,    71,   547,    -1,   549,    -1,    -1,    -1,    -1,
      -1,    -1,   937,    -1,    -1,   940,   560,   942,    -1,    -1,
      -1,    -1,   947,  1659,  1660,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,   600,    -1,    -1,    -1,
     129,    -1,    -1,    -1,   989,   990,    -1,   397,    -1,    -1,
      -1,    -1,   402,    -1,    -1,   405,    -1,     1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   173,   174,    -1,    -1,    -1,   439,
      -1,    -1,    -1,    -1,    -1,  1751,    -1,    -1,  1043,   663,
     664,   451,   452,    -1,    -1,  1761,  1043,    -1,    -1,    -1,
     101,    -1,  1768,    -1,    58,   106,   107,   108,   109,   110,
     111,   112,   113,   687,    -1,    -1,   117,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   700,  1793,  1083,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1091,    -1,    -1,    -1,
      -1,    -1,    -1,  1809,    -1,    -1,    -1,   101,    -1,   150,
      -1,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,  1825,
    1826,    -1,    -1,  1829,   101,  1120,   740,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1138,    -1,    -1,    -1,    -1,   762,   763,
     144,    -1,   129,   101,    -1,  1861,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   779,  1161,   161,    -1,    -1,
    1157,   571,   149,   150,  1161,    -1,   153,    -1,    -1,    -1,
      -1,   129,    -1,   160,   161,    -1,    -1,    -1,    -1,  1895,
      -1,    -1,    -1,   807,    -1,    -1,   173,   191,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,    -1,   160,   161,  1209,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1209,  1218,   838,    -1,    -1,    -1,    -1,   101,
      -1,   845,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1237,  1238,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   866,    -1,    -1,    -1,  1251,  1252,    -1,    -1,
      -1,  1256,  1257,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
    1257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,    -1,    -1,    -1,    -1,    -1,   281,    -1,    -1,
    1285,  1286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1295,  1296,  1297,  1298,    -1,    -1,   706,   921,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   937,    -1,    -1,   940,    -1,   942,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   129,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,   149,   150,
      -1,    -1,   153,    -1,   764,   129,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,   989,   990,   371,    -1,    -1,
     374,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   388,    -1,   160,   161,   392,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1407,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,  1043,
      -1,    -1,    -1,    -1,    -1,  1430,    -1,   837,    -1,    -1,
      -1,    -1,   436,    -1,    -1,    -1,   101,   149,   150,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   160,   161,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,  1469,  1470,    -1,  1091,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,   150,    -1,   152,    -1,    -1,
    1495,    -1,  1489,    -1,    -1,    -1,  1120,    -1,    -1,   150,
    1505,   152,    -1,   507,    -1,    -1,    -1,    -1,    -1,    -1,
    1515,    -1,  1517,    -1,  1138,   101,   149,   150,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   538,  1540,    -1,  1161,    -1,    -1,
      -1,    -1,    -1,   547,    -1,   549,  1551,    -1,    -1,  1554,
      -1,    -1,    -1,    -1,  1551,   965,   560,    -1,    -1,    -1,
     970,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
    1575,    -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1209,    -1,    -1,    -1,    -1,
      -1,    -1,  1597,  1598,    -1,    -1,   600,  1602,  1603,    -1,
      -1,    -1,    -1,    -1,    -1,  1602,  1603,    -1,    -1,    -1,
      -1,    -1,    -1,  1237,  1238,   101,    -1,  1622,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1251,  1252,    -1,
      -1,    -1,  1256,  1257,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,   663,
     664,  1285,  1286,    -1,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,   687,     4,    -1,  1096,  1097,  1098,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   700,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1141,    -1,    -1,    -1,    -1,   740,    -1,    58,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1761,    -1,   762,   763,
      -1,    81,    -1,  1768,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   779,    -1,    -1,    -1,    -1,
      -1,   101,    -1,  1407,    -1,    -1,    -1,    -1,  1793,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1793,    -1,    -1,    58,
      -1,    -1,    -1,   807,  1809,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1222,    -1,    -1,    -1,    -1,    -1,   138,    -1,
    1825,  1826,    81,    -1,   144,    -1,   146,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   838,    -1,    -1,    -1,    -1,    -1,
     160,   845,   101,    -1,    -1,  1469,  1470,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1861,    -1,   178,    -1,
      -1,    -1,   866,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     190,    -1,    -1,  1878,    -1,    -1,    -1,    -1,    -1,   138,
      -1,  1505,    -1,    -1,    -1,   144,    -1,   146,    -1,    -1,
    1895,  1515,    -1,  1517,    -1,    -1,    -1,    -1,    -1,  1309,
       5,   160,  1312,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    -1,    -1,  1540,   921,    -1,   178,
     240,    -1,    -1,   243,    -1,    -1,    -1,  1551,   248,    -1,
      -1,   190,    -1,   937,    -1,    -1,   940,    -1,   942,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,
      -1,  1575,    -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,
      -1,   281,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,  1597,  1598,    -1,    -1,    -1,  1602,  1603,
     300,   240,    -1,    -1,   243,   989,   990,    -1,    -1,   248,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   331,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,   281,    -1,   129,   345,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1660,    -1,    -1,  1043,
      -1,   300,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   345,  1091,    -1,    -1,
      -1,    -1,  1502,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   432,    -1,    -1,    -1,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1138,    -1,    -1,  1761,    -1,    -1,
      -1,    -1,    -1,    -1,  1768,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1793,
      -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,  1589,
      -1,   501,   502,    -1,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1825,  1826,    -1,    -1,  1209,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   543,    -1,    -1,    -1,   547,    -1,   549,
      -1,    -1,    -1,  1237,  1238,    -1,    -1,  1861,    -1,    -1,
      -1,    -1,   501,   502,    -1,    -1,    -1,  1251,  1252,    -1,
      -1,    -1,  1256,  1257,   574,    -1,   576,   577,    -1,   579,
      -1,    -1,   582,    -1,    -1,   585,   586,   587,    -1,    -1,
      -1,  1895,    -1,    -1,    -1,     0,    -1,  1687,     3,    -1,
      -1,  1285,  1286,    -1,   543,    -1,    -1,    -1,   547,    -1,
     549,    -1,    -1,    -1,    -1,    -1,  1706,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   574,    -1,   576,   577,    -1,
     579,    -1,    -1,   582,    -1,    -1,   585,   586,   587,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,   661,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    76,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,   708,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   661,  1407,    70,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,    -1,    -1,    -1,    91,    92,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   708,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,  1469,  1470,    -1,    -1,    -1,
     125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,  1505,   227,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1515,    -1,  1517,    -1,    -1,    -1,   242,   838,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   252,    -1,    -1,
     850,   851,    -1,    -1,    -1,    -1,  1540,   262,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1551,    -1,    -1,
      -1,   276,   277,    -1,    -1,    -1,    -1,    -1,   283,   284,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,  1575,    -1,    -1,   299,    -1,    -1,  1581,    -1,   838,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   850,   851,  1597,  1598,    -1,    -1,    -1,  1602,  1603,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   332,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,   290,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,   370,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   430,    -1,    -1,    -1,   434,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,
      -1,    -1,   457,   458,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   466,   467,   468,   469,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1761,    -1,    -1,
     485,    -1,    -1,  1083,  1768,    -1,    -1,    47,   493,    -1,
      -1,  1091,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     435,    -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,  1793,
      -1,   446,   447,    73,    -1,    -1,   521,    -1,    -1,    -1,
    1120,    -1,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1138,    -1,
      -1,  1825,  1826,    -1,  1083,    -1,    -1,   552,    -1,    -1,
      -1,    -1,  1091,    -1,   559,    -1,    -1,  1157,   118,   564,
      -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,    -1,    -1,
      -1,   131,    -1,   133,    -1,    -1,    -1,  1861,    -1,    -1,
      -1,  1120,    -1,   588,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1138,
      -1,    -1,    -1,   163,    -1,    -1,    -1,    -1,    -1,   544,
      -1,  1895,    -1,    -1,    -1,    -1,    -1,    -1,  1157,    -1,
     180,    -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   652,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   217,    -1,    -1,
      -1,   221,    -1,    -1,   224,   225,    -1,    -1,   228,   674,
     675,   231,   232,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,  1295,  1296,  1297,  1298,  1299,
    1300,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,   730,    -1,    -1,    52,   734,
      54,    -1,    -1,   293,    -1,    -1,   296,    -1,   743,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1295,  1296,  1297,  1298,
    1299,  1300,   767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   778,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,   369,
     815,    -1,    -1,    -1,    -1,    -1,    -1,   822,    -1,    -1,
      -1,   756,    -1,    -1,   384,   149,    -1,    -1,   152,   153,
    1430,    97,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,   108,   848,   110,    -1,   112,    -1,    -1,    -1,
      -1,   856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   425,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,    -1,   152,   153,    -1,  1489,
      -1,  1430,    -1,    -1,    -1,    -1,   831,   832,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   842,   843,   844,
     470,    -1,   847,    -1,    -1,  1515,    -1,  1517,    -1,    -1,
      -1,    -1,   482,    -1,    -1,    -1,    -1,    -1,   194,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1540,    -1,    -1,    -1,    -1,   880,    -1,    -1,    -1,    -1,
    1489,    -1,    -1,    -1,  1554,    -1,    -1,    -1,    -1,    -1,
    1560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1515,   982,  1517,    -1,
      -1,   986,    -1,    -1,    -1,    -1,    -1,    -1,   993,    -1,
     256,    -1,   258,   259,    -1,    -1,   556,    -1,  1003,    -1,
      -1,  1540,    -1,    -1,    -1,  1010,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1019,  1554,  1021,    -1,    -1,    -1,
     286,  1560,    -1,    -1,    -1,  1030,   292,    -1,    -1,    -1,
      -1,    -1,    -1,  1038,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   979,    -1,    -1,    -1,    -1,  1054,
      -1,    -1,    -1,  1058,    -1,    -1,    -1,    -1,    -1,    -1,
    1660,    -1,    -1,    -1,    -1,    -1,    -1,  1072,    -1,    -1,
    1075,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1016,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1025,  1026,  1027,  1028,    -1,    -1,   656,   657,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   373,    -1,  1044,
      -1,    -1,    47,   379,    -1,   381,    -1,    -1,    -1,    -1,
      -1,  1660,   682,   683,    -1,    -1,    -1,    -1,    -1,    -1,
    1065,    -1,  1067,    -1,    -1,   695,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1761,    -1,    -1,    -1,   725,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,   444,   445,
      -1,    -1,    -1,   118,    -1,  1120,   746,    -1,    -1,   749,
     750,    -1,   752,    -1,   754,   755,   131,    -1,   133,    -1,
      -1,    -1,  1207,    -1,    -1,    -1,    -1,    -1,    -1,  1809,
      -1,  1146,    -1,   479,    -1,    -1,    -1,  1152,    -1,  1154,
    1155,    -1,  1761,    -1,    -1,  1825,  1231,    -1,    -1,    -1,
      -1,   791,    -1,   499,    -1,   795,    -1,    -1,   504,    -1,
     506,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     526,  1861,   528,   529,  1199,    -1,    -1,    -1,    -1,    -1,
    1809,    -1,  1277,  1208,    -1,  1210,  1281,  1212,    -1,  1214,
     546,    -1,    -1,    -1,  1219,    -1,  1825,    -1,    -1,   224,
     225,    -1,   558,   228,    -1,    -1,   231,   232,    -1,    -1,
      -1,    -1,    -1,    -1,  1239,  1240,  1311,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1324,
    1325,    -1,  1861,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1266,  1267,    -1,    -1,    -1,    -1,    -1,    -1,  1274,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1361,    -1,    -1,  1364,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1304,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   948,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1402,    -1,    -1,
     666,    -1,   668,   669,    -1,    -1,    -1,    -1,  1413,  1344,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   690,   691,    -1,    -1,    -1,    -1,
      -1,   697,    -1,  1438,   369,    -1,    -1,  1442,    -1,   999,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   384,
      -1,  1456,  1457,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1411,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1419,    -1,  1421,    -1,  1048,    -1,
     425,  1051,    -1,    -1,    -1,    -1,    -1,    -1,  1433,  1434,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1448,  1449,    -1,  1451,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1459,    -1,    -1,    -1,    -1,    -1,
    1465,    -1,    -1,    -1,    -1,    -1,  1471,  1472,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   482,    -1,    -1,
    1555,  1556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   841,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
    1200,    -1,    -1,    -1,    -1,    70,    -1,  1582,  1583,    -1,
      -1,    -1,    -1,    -1,    -1,  1215,    -1,  1592,    -1,   925,
      -1,    -1,    -1,  1223,  1224,    -1,    -1,    -1,    -1,    -1,
    1605,  1676,    -1,   939,    -1,    -1,    -1,  1612,  1613,   104,
     105,    -1,  1242,    -1,   950,  1245,    -1,  1247,  1248,    -1,
      -1,    -1,    -1,   959,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1708,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1716,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   656,   657,    -1,    -1,    -1,    -1,   152,  1288,    -1,
      -1,    -1,    -1,    -1,  1000,    -1,    -1,    -1,  1743,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   682,   683,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1764,
     695,    -1,  1767,  1698,    -1,  1700,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1711,  1712,    -1,    -1,
      -1,    -1,    -1,  1718,    -1,    -1,    -1,    -1,    -1,    -1,
     725,    -1,    -1,    -1,    -1,  1355,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   746,    -1,    -1,   749,   750,    -1,   752,    -1,   754,
     755,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   161,    -1,    -1,  1852,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   791,    -1,    -1,    -1,
     795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1137,    -1,   190,   191,    -1,    -1,  1437,    -1,  1814,
      -1,    -1,    -1,    -1,    -1,    -1,  1821,    -1,    -1,    -1,
      -1,    -1,  1827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   221,    -1,    -1,  1467,    -1,    -1,
      -1,   228,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1493,    -1,    -1,    -1,    -1,    -1,  1499,
    1875,    -1,    -1,    -1,    -1,    48,    -1,  1213,    -1,    52,
      -1,    54,    -1,  1219,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1899,    -1,    -1,    -1,    71,    -1,
      -1,    -1,  1907,    -1,    -1,    -1,    -1,    -1,    -1,   296,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1922,    -1,    -1,
    1550,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   948,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1306,    -1,    -1,    -1,    -1,   362,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   371,   372,   159,   160,   161,   162,
     163,   164,   165,    -1,   999,    -1,    -1,    -1,  1628,  1629,
      -1,    -1,    -1,    -1,    -1,   392,  1636,    -1,    -1,    -1,
    1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1354,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,   426,
      -1,   428,   429,  1048,    -1,    -1,  1051,    -1,    -1,   436,
      -1,   160,    -1,   440,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   191,    -1,   471,    -1,    -1,    -1,   475,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   205,    -1,    -1,    -1,
      -1,    -1,  1732,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     507,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,  1798,    67,
     557,    -1,    70,   560,    -1,    -1,    -1,    75,    76,    -1,
      -1,    -1,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1200,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,
    1215,    -1,    -1,   600,    -1,    -1,    -1,    -1,  1223,  1224,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,  1242,    -1,    -1,
    1245,    -1,  1247,  1248,    -1,    -1,    -1,  1877,    -1,    -1,
     148,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,   652,   653,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   663,   664,    -1,    -1,
      -1,    -1,    -1,  1288,    -1,    -1,    -1,   674,    -1,    -1,
      -1,   678,    -1,    -1,    -1,    -1,    -1,    -1,   685,    -1,
     687,    -1,    -1,  1639,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1659,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   725,   726,
      -1,   728,   729,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1355,   460,    -1,   740,   463,    -1,   743,    -1,   745,   746,
      -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   762,   763,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,    -1,
      -1,    -1,   779,    -1,  1730,    -1,   783,    -1,    -1,    -1,
     787,    -1,    97,    -1,   791,   792,    -1,    -1,   795,   796,
      -1,    -1,    -1,    -1,    -1,  1751,   803,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,    -1,   150,    -1,    -1,   845,   846,
      -1,    -1,  1467,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,   581,    -1,    -1,    -1,    -1,    -1,    -1,   866,
      -1,    -1,    -1,   178,    -1,    -1,    -1,    -1,    -1,    -1,
     599,   600,    -1,  1829,    -1,    -1,   191,    -1,    -1,   194,
      -1,   610,    -1,   612,   613,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   627,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   925,    -1,
      -1,    -1,    -1,    -1,    -1,  1550,    -1,    -1,   243,    -1,
      -1,    -1,   661,    -1,    -1,   664,    -1,    -1,    -1,    -1,
      -1,   256,    -1,   950,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,    -1,    -1,    -1,  1911,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   292,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,   993,   994,    -1,    -1,
      -1,   720,    -1,  1000,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1628,  1629,    -1,    -1,    -1,   737,    -1,
      -1,   740,    -1,    -1,    -1,  1640,   331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     345,    -1,    -1,    -1,    -1,    -1,  1043,    -1,    -1,    -1,
      -1,  1048,  1049,   772,  1051,  1052,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   371,    -1,   373,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,   804,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,  1732,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,   436,   851,    -1,    -1,    70,    -1,   442,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   866,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,    -1,    -1,
      -1,    -1,   881,    -1,  1161,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   898,
      -1,    -1,    -1,  1798,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,   501,   502,    -1,    -1,
      -1,    -1,   507,  1200,  1201,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1209,    -1,    -1,    -1,    -1,   152,  1215,  1216,
     939,   940,    -1,    -1,    -1,   160,   161,    -1,   947,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1242,  1243,    -1,  1245,   968,
      -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,   977,  1256,
    1257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   574,
      -1,   576,   577,    -1,   579,    -1,    -1,   582,    -1,    -1,
     585,   586,   587,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1032,    48,  1034,    -1,    -1,    52,    -1,
      54,    -1,    -1,  1042,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   661,    -1,    -1,   664,
      -1,   666,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   687,   117,   118,   119,    -1,   121,   122,  1108,
    1109,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   708,    -1,    -1,    -1,    -1,    -1,    -1,
    1407,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,   740,    -1,    -1,    -1,    -1,
      -1,    -1,  1161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,   763,  1178,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1191,    -1,   779,  1194,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1483,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1213,    -1,    -1,    -1,    -1,    -1,
    1219,    -1,  1499,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
     845,    49,    50,    51,    -1,    53,   851,    -1,    -1,    -1,
    1269,    -1,  1271,    -1,  1551,    -1,    -1,  1276,    -1,    -1,
      -1,   866,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,  1314,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1602,  1603,    -1,    -1,    -1,
      -1,    -1,    -1,  1332,    -1,    -1,  1335,    -1,    -1,    -1,
      -1,   129,    -1,    -1,  1621,  1622,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,
    1637,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1383,    -1,  1385,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1407,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,  1417,  1418,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,  1730,    53,    -1,    -1,    -1,  1043,    -1,
      -1,  1738,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1489,    -1,    -1,    -1,    -1,  1494,    -1,    -1,  1083,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1793,    -1,    -1,    -1,
      -1,  1798,  1799,    -1,    -1,  1802,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1536,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1828,  1137,   152,    -1,    -1,    -1,    -1,    -1,  1558,
      -1,    -1,  1561,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1157,    -1,    -1,    -1,  1161,    -1,    -1,    -1,
    1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1877,  1878,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1209,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    19,  1911,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,  1256,  1257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1295,  1296,  1297,    -1,  1299,  1300,    -1,    -1,   104,   105,
      -1,  1306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,  1354,
      -1,    -1,    -1,    -1,   160,   161,    -1,  1776,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  1407,    -1,    48,    -1,    50,    51,    52,    -1,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    71,    72,    -1,
      74,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    -1,    96,    -1,    98,    99,   100,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,  1489,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   148,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1551,    12,    13,    14,
      15,    16,    -1,    -1,    19,  1560,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1602,  1603,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,  1659,    50,    51,    52,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    69,    70,    71,    72,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1751,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,   157,  1768,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1793,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1826,    -1,    -1,  1829,     3,     4,     5,     6,     7,
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
     160,   161,   162,   163,   164,   165,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      48,    -1,    50,    51,    52,    -1,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,     3,    -1,
      -1,    -1,   160,   161,     9,    -1,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,
      -1,     3,    -1,    -1,    -1,   160,   161,     9,    -1,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,     6,
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
      -1,    -1,    -1,   160,   161,     4,     5,     6,     7,     8,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    48,
      -1,   160,   161,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    71,    -1,    -1,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    17,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    69,    -1,    71,    72,
      -1,    74,   160,   161,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    48,    -1,    -1,    -1,    52,    -1,    54,
      -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    71,    -1,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,   153,
      -1,    -1,   104,   105,    -1,    -1,   160,   161,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     152,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    12,    13,
      14,    15,    16,    17,    70,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,   104,   105,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
     104,   105,    -1,    -1,   160,   161,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,   152,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    12,    13,    14,    15,    16,    17,    -1,    19,
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
      -1,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    12,    13,    14,    15,
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
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
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
     162,   163,   164,   165,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      48,    -1,    50,    51,    52,    -1,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    12,    13,
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
     164,   165,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,    -1,
     160,   161,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    12,    13,    14,    15,    16,    17,
      70,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,   104,   105,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    76,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,   153,    -1,    -1,   104,   105,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    12,    13,
      14,    15,    16,    17,    70,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,   104,   105,
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
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    17,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,   104,   105,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    12,    13,    14,    15,    16,    17,    70,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,   152,
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
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    12,    13,    14,    15,
      16,   160,   161,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    70,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,   104,   105,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,
     104,   105,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,     4,
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
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,   152,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,   104,   105,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,   152,    55,    56,    57,
      58,    59,    60,    61,    62,    48,    -1,    -1,    -1,    52,
      -1,    54,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,   150,    -1,   152,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
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
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,
     151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
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
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
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
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   176,   383,   384,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    50,    51,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    67,    70,    71,    96,
     100,   101,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   116,   129,   149,   150,   152,   153,   160,   161,   179,
     180,   194,   275,   276,   277,   278,   279,   280,   281,   282,
     283,   284,   285,   286,   288,   290,   292,   293,   294,   295,
     296,   297,   298,   299,   300,   302,   304,   305,   306,   308,
     309,   313,   314,   315,   316,   317,   319,   325,   326,   327,
     328,   339,   342,   375,   378,   388,   393,   395,   401,   405,
     410,   411,   412,   413,   414,   415,   416,   417,   437,   454,
     455,   456,   457,     0,   176,   180,   194,   279,   281,   290,
     293,   305,   309,   314,   115,   149,    56,    59,    60,    62,
     149,   149,   399,   400,   401,   301,   302,   104,   105,   180,
     355,   376,   377,   355,   149,   388,   149,   149,   149,   194,
     400,   405,   411,   412,   413,   415,   416,   417,   104,   316,
     154,   176,   282,   290,   293,   410,   414,   453,   454,   457,
     458,   174,   177,   146,   173,   215,   358,    87,   155,   394,
     355,   177,   177,   177,   174,   104,   105,   149,   194,   287,
     396,   405,   406,   407,   408,   409,   410,   414,   418,   419,
     420,   421,   422,   428,     3,    46,    47,    49,    53,   307,
       3,     4,   153,   194,   281,   294,   298,   300,   310,   315,
     390,   410,   414,   457,   279,   281,   293,   305,   309,   314,
     391,   410,   414,    63,   299,   299,   294,   300,   299,   294,
     299,   294,   152,   399,   155,   177,   149,   157,   223,   399,
     399,   176,   270,   271,   153,   290,   293,   455,   355,   355,
     388,   173,   293,   149,   194,   396,   405,   410,   419,   153,
     194,   457,   389,    63,    64,    65,    66,   153,   171,   355,
     364,   366,   370,   372,   373,    48,    52,    54,    71,    98,
      99,   101,   103,   113,   114,   115,   117,   118,   119,   121,
     122,   149,   153,   159,   162,   163,   164,   165,   178,   179,
     181,   182,   183,   186,   193,   194,   195,   196,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     212,   217,   290,   315,   356,   357,   374,   453,   458,    55,
     153,   194,   289,   293,   297,   298,   304,   305,   311,   312,
     313,   314,   318,   325,   326,   342,   351,   353,   437,   449,
     450,   451,   452,   457,   458,   104,   105,   157,   180,   315,
     428,   401,   149,   371,   372,   149,   149,   181,   153,   193,
     194,   210,   211,   315,   151,   374,   293,   411,   412,   413,
     415,   416,   417,   151,   151,   151,   151,   151,   151,   151,
     153,   290,   437,   455,   153,   160,   194,   212,   281,   282,
     289,   291,   293,   305,   312,   314,   346,   347,   350,   351,
     352,   449,   457,   149,   410,   414,   457,   149,   155,    21,
     157,   212,   359,   149,   355,   223,   149,   155,   155,   155,
     400,   405,   407,   408,   409,   418,   420,   421,   422,   293,
     406,   419,   155,    96,   398,   153,   399,   436,   437,   399,
     399,   394,   270,   149,   399,   436,   394,   399,   399,   293,
     396,   149,   149,   292,   293,   290,   293,   176,   290,   453,
     458,   317,   157,   394,   270,   355,   358,   281,   298,   392,
     410,   414,   157,   394,   270,   376,   293,   305,   293,   293,
     104,   316,   104,   105,   180,   315,   320,   376,   176,   180,
     354,   148,   176,     3,   286,   288,   293,   297,   223,   176,
     176,   398,   149,   398,   177,   212,   400,   405,   293,   149,
     176,   355,   157,   355,   157,   355,   131,   160,   161,   369,
     151,   155,   355,   373,   149,   193,   149,   149,   196,   149,
     193,   149,   149,   193,   193,    18,    20,    83,   153,   162,
     163,   197,   198,   212,   219,   223,   328,   356,   457,   155,
     176,   149,   183,   158,   158,   115,   118,   120,   121,   122,
     149,   152,   153,   157,   158,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   173,   214,   215,   216,
     196,   196,   166,   160,   167,   168,   162,   163,   123,   124,
     125,   126,   169,   170,   127,   128,   161,   159,   171,   129,
     130,   172,   149,   194,   432,   433,   434,   435,   436,   152,
     151,   155,   399,   154,   176,   291,   293,   305,   312,   314,
     448,   449,   457,   458,   149,   153,   161,   173,   194,   437,
     438,   439,   440,   441,   442,   443,   460,   194,   318,   457,
     293,   312,   299,   294,   399,   151,   291,   293,   450,   291,
     437,   450,     9,   343,   355,   340,   157,   364,   173,   364,
      12,    86,   101,   104,   105,   179,   402,   403,   404,   151,
     176,   151,   155,   151,   151,   151,   151,   151,   151,   151,
     149,   399,   436,   437,   149,   436,   437,   176,   290,   455,
     176,   177,   177,   149,   161,   194,   405,   423,   424,   425,
     426,   427,   428,   429,   430,   431,   131,   457,   177,   177,
     355,   355,   176,   176,   176,   101,   152,   153,   179,   180,
     359,   360,   361,   362,   363,   150,   212,   218,   149,   176,
     176,   176,   176,   405,   407,   408,   409,   418,   420,   421,
     422,   151,   151,   151,   151,   151,   151,   151,   406,   419,
     399,   149,   358,   154,   176,   223,   394,   176,   223,   396,
     219,   357,   219,   357,   396,   386,   223,   394,   398,   157,
     394,   270,   386,   223,   394,   322,   323,   321,   157,   131,
     293,   348,   349,   352,   353,   151,   155,    68,   272,   273,
     177,   293,   286,   160,   212,   176,   405,   347,   386,   154,
     176,   149,   368,   366,   367,   356,   153,   356,   356,   356,
     212,   356,   151,   356,   356,   356,   176,   151,   162,   163,
     198,    17,   295,   151,   155,   151,   160,   161,   151,   218,
     212,   157,   180,   180,   113,   153,   180,   150,   187,   188,
     212,   113,   153,   180,   328,   212,   187,   180,   157,   212,
     196,   199,   199,   199,   200,   200,   201,   201,   202,   202,
     202,   202,   203,   203,   204,   205,   206,   207,   208,   156,
     219,   176,   433,   434,   435,   293,   432,   399,   399,   153,
     180,    76,   303,   212,   357,   180,   291,   437,   450,   293,
     297,   457,   176,   439,   440,   441,   154,   176,    17,   212,
     293,   438,   460,   399,   399,   437,   291,   448,   458,   293,
     180,   399,   291,   450,   315,   155,   459,   173,   215,   344,
     157,   343,   151,   357,   151,   151,   155,   149,   174,   212,
     174,   181,   149,   399,   436,   437,   149,   436,   437,   176,
     176,   154,   154,   149,   405,   424,   425,   426,   429,    17,
     293,   423,   427,   149,   399,   442,   460,   399,   399,   460,
     149,   399,   442,   399,   399,   177,   211,   355,   154,   155,
     154,   155,   460,   460,   131,   345,   346,   347,   345,   355,
     153,   180,   176,   156,   155,   459,   359,   152,   153,   156,
     363,   151,   155,   176,   345,   180,   396,   180,   151,   151,
     151,   151,   151,   151,   149,   399,   436,   437,   149,   399,
     436,   437,   396,   181,   437,   212,   223,   348,   151,   151,
     151,   151,   384,   385,   223,   386,   223,   394,   385,   223,
     157,   157,   157,   329,   177,   177,   180,   274,   355,    17,
      69,    71,    74,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    90,    91,    92,    93,    94,
      96,   104,   105,   116,   176,   219,   220,   221,   222,   223,
     224,   225,   227,   228,   238,   242,   243,   244,   245,   246,
     247,   252,   253,   259,   260,   261,   275,   293,   297,   355,
     395,    68,   174,   177,   177,   177,   345,   177,   385,   279,
     281,   290,   379,   380,   381,   382,   374,   173,   365,   365,
     151,   176,   155,   151,   151,   155,   151,   196,   151,   151,
     151,   196,    17,   295,   212,   151,   151,   150,   157,   196,
     154,   177,   187,   113,   117,   119,   180,   189,   190,   191,
     151,   155,   189,   154,   155,   148,   360,   210,   156,   348,
     151,   151,   151,   432,   189,   291,   450,   153,   160,   194,
     212,   315,   212,   293,   348,   151,   151,   151,     5,   293,
     399,   438,   157,   180,   428,     9,   355,   148,   359,   343,
     459,   157,   151,   403,   187,   177,   151,   176,   176,   348,
     348,   429,   151,   151,   151,   151,   149,   405,   428,   423,
     427,   176,   176,   154,   177,   460,   176,   176,   177,   177,
     177,   177,   358,   176,   210,   211,   212,   397,   359,   361,
     148,   176,   150,   212,   345,   177,   173,   149,   399,   436,
     437,   149,   399,   436,   437,   176,   176,   398,   151,   177,
     177,   387,   385,   223,   387,   329,   329,   329,     3,     9,
      71,   148,   276,   283,   284,   290,   293,   330,   335,   453,
     151,   155,   155,   174,   149,    59,    60,   174,   223,   275,
     395,   149,    17,   221,   149,   149,   174,   355,   174,   355,
     160,   355,   157,   220,   149,   149,   149,   223,   212,   213,
     213,    13,   262,    72,   229,   174,   177,   225,    76,   174,
     355,    89,   248,   354,   293,   156,   274,   174,   154,   154,
     177,   155,   387,   396,   177,   174,   177,   174,   177,   151,
     357,   371,   371,   180,    76,   184,   185,   356,   196,   196,
     196,   196,   196,   157,   360,   155,   148,   192,   153,   190,
     192,   192,   154,   155,   120,   152,   188,   154,   218,   459,
     210,   177,   149,   399,   436,   437,   154,   176,   177,   177,
     177,   212,   177,   149,   399,   442,   437,   292,     5,   160,
     177,   212,   343,   399,   399,   315,   344,   459,   148,   148,
     176,   151,   174,   348,   348,   177,   177,   151,   149,   399,
     436,   437,   149,   399,   442,   405,   399,   399,   348,   348,
     154,   347,   350,   350,   351,   151,   155,   155,   151,   189,
     131,   165,   177,   177,   359,   212,   177,   151,   212,   176,
     176,   348,   348,   358,   399,   155,   151,   148,   387,   148,
     148,   148,   148,   290,   328,   336,   453,   290,   335,   149,
     324,   174,   174,   149,   156,   194,   331,   332,   338,   405,
     406,   419,   155,   174,   355,   176,   355,   187,   174,   223,
     174,   223,   219,    78,   151,   176,   151,   176,   174,   174,
     219,   174,   360,   174,   219,   218,   219,   108,   109,   110,
     111,   112,   254,   256,   257,   174,    95,   174,    82,   149,
     149,   177,   148,   174,   174,   149,   221,   223,   399,   174,
     151,   176,   148,   148,   176,   155,   155,   151,   156,   151,
     155,   156,   360,   459,   218,   118,   189,   190,   153,   190,
     153,   190,   154,   148,   151,   176,   154,   154,   154,   177,
     151,   176,   212,   212,   177,   154,   177,   459,   341,   157,
     344,   148,   379,   177,   177,   151,   151,   176,   176,   177,
     177,   177,   176,   176,   177,   211,   211,   154,   154,   177,
     151,   399,   348,   348,   177,   177,   219,   148,   324,   324,
     324,   149,   194,   333,   334,   436,   444,   445,   446,   447,
     174,   155,   174,   331,   174,   374,   400,   405,   212,   293,
     155,   174,   337,   338,   337,   355,   131,   352,   353,   151,
     151,   149,   221,   219,   230,   275,   277,   280,   286,   293,
     297,   221,   173,   174,   219,   239,   240,   275,   174,   459,
     151,   151,   151,   223,   256,   257,   149,   212,   149,   181,
     230,   196,   249,   107,   221,   399,   380,   176,   176,   212,
     185,   212,   459,   148,   154,   154,   189,   189,   348,   154,
     348,   177,   177,   154,   154,   148,   157,   343,   177,   151,
     151,   348,   348,   151,   151,   154,   155,   131,   347,   131,
     154,   177,   177,   177,   151,   151,   154,   445,   446,   447,
     293,   444,   155,   174,   399,   399,   174,   151,   405,   399,
     221,    75,    76,   157,   233,   234,   235,   151,   219,   151,
     219,   293,   219,   220,   143,   144,   145,   165,   174,   241,
     151,   156,   220,   148,   157,   235,   221,   149,   176,   174,
     181,   151,   156,   151,   151,   155,   156,   247,   251,   355,
     396,   148,   154,   154,   177,   177,   154,   154,   343,   459,
     148,   177,   177,   176,   177,   154,   151,   151,   151,   151,
     151,   444,   399,   332,   211,   231,   232,   397,   156,   176,
     221,   233,   174,   151,   221,   174,   104,   173,   219,   220,
     219,   221,   240,   174,   174,   176,   176,   258,   291,   293,
     453,   156,   174,   153,   181,   263,   264,   265,   221,   196,
     187,    73,   106,   248,   250,   151,   151,   459,   148,   151,
     151,   350,   149,   399,   436,   437,   334,   131,   155,   156,
     268,   269,   275,   174,   177,   220,   219,   144,   165,   241,
     174,   165,   177,   220,   268,   258,   177,   149,   194,   396,
     444,   180,   156,   101,   149,   151,   156,   155,    73,   151,
     221,   149,   221,   221,   148,   176,   211,   231,   234,   236,
     237,   275,   150,   150,   219,   220,   219,   236,   177,   174,
     255,   293,   263,   154,   211,   174,   263,   265,   221,   219,
     107,   107,   348,   221,   226,   177,   234,   165,   165,   165,
     177,   255,   210,   151,   156,   181,   151,   151,   156,   151,
     251,    73,   246,   177,   221,   148,   226,   219,   150,   219,
     219,   148,   151,   223,   181,   266,   149,   174,   266,   221,
      73,   151,   223,   155,   156,   211,   151,   221,   181,   180,
     267,   151,   174,   151,   155,   174,   180
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   175,   176,   177,   178,   178,   178,   178,   178,   179,
     179,   179,   179,   179,   179,   179,   180,   180,   180,   181,
     182,   182,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   184,   184,   185,   185,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   187,   187,   187,   188,
     188,   189,   189,   190,   190,   190,   190,   190,   190,   190,
     191,   191,   191,   192,   192,   193,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   194,
     194,   194,   195,   195,   195,   195,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   197,   197,   197,   197,   198,
     198,   199,   199,   200,   200,   200,   200,   201,   201,   201,
     202,   202,   202,   203,   203,   203,   203,   203,   204,   204,
     204,   205,   205,   206,   206,   207,   207,   208,   208,   209,
     209,   210,   210,   210,   211,   212,   212,   212,   213,   213,
     214,   214,   215,   215,   216,   216,   216,   216,   216,   216,
     216,   216,   216,   216,   216,   217,   217,   218,   218,   218,
     218,   219,   219,   220,   220,   221,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   221,   221,   222,   223,
     223,   224,   224,   225,   225,   225,   225,   225,   226,   226,
     227,   228,   228,   228,   228,   228,   229,   229,   230,   230,
     230,   230,   231,   231,   231,   232,   232,   233,   233,   234,
     234,   235,   236,   236,   237,   237,   238,   238,   238,   238,
     238,   238,   239,   239,   240,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   240,   240,   240,   240,   240,
     240,   240,   241,   241,   241,   241,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   243,   243,   244,   245,
     246,   247,   247,   248,   248,   249,   249,   250,   251,   251,
     251,   251,   251,   251,   252,   252,   253,   253,   253,   254,
     254,   255,   255,   256,   256,   256,   256,   257,   258,   258,
     258,   258,   258,   259,   260,   260,   261,   261,   261,   261,
     261,   262,   262,   263,   263,   264,   264,   265,   265,   266,
     266,   266,   267,   267,   268,   268,   269,   269,   270,   270,
     271,   271,   272,   272,   273,   273,   274,   274,   275,   275,
     275,   276,   276,   277,   277,   277,   277,   277,   278,   278,
     278,   279,   279,   279,   280,   280,   280,   280,   280,   281,
     281,   282,   282,   283,   283,   283,   284,   284,   284,   284,
     284,   285,   285,   286,   286,   286,   286,   287,   287,   288,
     288,   288,   289,   289,   289,   290,   290,   290,   291,   291,
     291,   292,   292,   293,   293,   294,   294,   295,   295,   295,
     295,   295,   296,   297,   297,   297,   298,   298,   299,   299,
     299,   299,   299,   299,   299,   299,   300,   300,   300,   300,
     300,   300,   300,   300,   300,   300,   300,   300,   300,   300,
     300,   300,   300,   300,   300,   300,   300,   300,   300,   300,
     300,   300,   300,   300,   301,   301,   302,   303,   303,   304,
     304,   304,   304,   304,   305,   305,   306,   306,   306,   306,
     307,   307,   307,   307,   307,   307,   308,   308,   308,   308,
     309,   310,   309,   309,   311,   311,   311,   311,   312,   312,
     312,   313,   313,   313,   313,   314,   314,   314,   315,   315,
     315,   315,   315,   315,   316,   316,   316,   317,   317,   318,
     318,   320,   319,   321,   319,   322,   319,   323,   319,   319,
     324,   324,   325,   325,   326,   326,   327,   327,   327,   328,
     328,   328,   328,   328,   328,   328,   328,   329,   329,   330,
     330,   330,   330,   330,   330,   330,   330,   330,   330,   331,
     331,   331,   332,   332,   332,   333,   333,   333,   334,   335,
     335,   336,   336,   337,   337,   338,   339,   340,   339,   339,
     339,   341,   339,   339,   339,   342,   342,   343,   343,   343,
     343,   344,   344,   345,   345,   345,   345,   345,   345,   345,
     346,   346,   346,   346,   347,   347,   348,   348,   348,   348,
     349,   349,   349,   349,   350,   350,   350,   350,   350,   351,
     351,   351,   351,   351,   352,   352,   353,   353,   354,   354,
     355,   355,   355,   356,   356,   356,   357,   357,   358,   358,
     358,   359,   359,   360,   360,   360,   360,   360,   361,   361,
     362,   362,   363,   363,   363,   363,   363,   364,   364,   365,
     365,   367,   366,   368,   366,   366,   366,   369,   369,   369,
     369,   370,   370,   370,   370,   371,   371,   372,   372,   373,
     373,   374,   374,   374,   374,   375,   375,   375,   376,   376,
     377,   377,   378,   378,   379,   379,   380,   380,   381,   381,
     381,   382,   382,   383,   383,   384,   384,   385,   385,   386,
     387,   388,   388,   388,   388,   388,   389,   388,   390,   388,
     391,   388,   392,   388,   393,   393,   393,   394,   394,   395,
     395,   395,   395,   395,   395,   395,   395,   395,   395,   396,
     396,   396,   397,   398,   398,   399,   399,   400,   400,   401,
     402,   402,   403,   403,   403,   404,   404,   404,   404,   404,
     404,   405,   405,   406,   406,   406,   406,   407,   407,   407,
     407,   408,   408,   408,   408,   408,   408,   408,   409,   409,
     409,   409,   410,   410,   410,   411,   411,   411,   411,   411,
     412,   412,   412,   412,   413,   413,   413,   413,   413,   413,
     414,   414,   414,   415,   415,   415,   415,   415,   416,   416,
     416,   416,   417,   417,   417,   417,   417,   417,   418,   418,
     419,   419,   419,   419,   420,   420,   420,   420,   421,   421,
     421,   421,   421,   421,   421,   422,   422,   422,   422,   422,
     423,   423,   423,   423,   423,   424,   424,   424,   425,   425,
     425,   425,   426,   426,   426,   427,   427,   427,   427,   427,
     428,   428,   429,   429,   429,   430,   430,   431,   431,   432,
     432,   432,   433,   433,   433,   433,   433,   434,   434,   434,
     434,   435,   435,   435,   436,   436,   436,   436,   437,   437,
     437,   437,   438,   438,   438,   438,   439,   439,   439,   439,
     439,   440,   440,   440,   440,   441,   441,   441,   442,   442,
     442,   443,   443,   443,   443,   443,   443,   444,   444,   444,
     445,   445,   445,   445,   445,   446,   446,   446,   446,   447,
     447,   448,   448,   448,   449,   449,   450,   450,   450,   450,
     450,   450,   451,   451,   451,   451,   451,   451,   451,   451,
     451,   451,   452,   452,   452,   452,   453,   453,   453,   454,
     454,   455,   455,   455,   455,   455,   455,   456,   456,   456,
     456,   456,   456,   457,   457,   457,   458,   458,   459,   459,
     460,   460
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     1,     3,     3,     3,     5,
       6,     1,     3,     3,     3,     1,     6,     4,     4,     4,
       3,     3,     3,     3,     3,     2,     5,     3,     3,     3,
       5,     2,     2,     7,     8,     5,     0,     1,     3,     1,
       1,     1,     3,     1,     2,     4,     3,     5,     3,     5,
       2,     2,     2,     0,     2,     1,     1,     1,     2,     2,
       2,     2,     2,     2,     4,     2,     4,     6,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     5,     5,
       4,     5,     5,     5,     4,     2,     2,     3,     3,     1,
       1,     1,     3,     1,     3,     3,     3,     1,     3,     3,
       1,     3,     3,     1,     3,     3,     3,     3,     1,     3,
       3,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     1,     5,     4,     1,     1,     3,     6,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     4,     7,     1,     1,     3,
       3,     1,     3,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     2,
       6,     1,     2,     1,     2,     1,     2,     1,     1,     2,
       2,     3,     5,    10,     5,    10,     5,     7,     1,     1,
       1,     2,     1,     3,     1,     1,     3,     3,     2,     1,
       2,     2,     0,     1,     2,     3,     7,     4,     7,     6,
       7,     4,     1,     3,     4,     5,     4,     1,     2,     3,
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
       2,     1,     4,     0,     1,     2,     3,     4,     2,     2,
       1,     2,     2,     5,     5,     7,     6,     1,     3,     0,
       2,     0,     5,     0,     5,     3,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     2,     5,
       6,     1,     1,     3,     3,     2,     3,     3,     2,     4,
       1,     4,     7,    10,     1,     4,     2,     2,     1,     1,
       5,     2,     5,     0,     1,     3,     4,     0,     1,     0,
       0,     1,     1,     1,     2,     5,     0,     8,     0,     7,
       0,     7,     0,     8,     1,     2,     3,     0,     4,     3,
       4,     4,     4,     4,     5,     5,     5,     5,     6,     1,
       1,     1,     3,     0,     5,     0,     1,     1,     2,     6,
       1,     3,     0,     1,     4,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     1,     2,     2,     2,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     8,     9,
       3,     4,     2,     1,     2,     6,     8,     9,     3,     4,
       2,     3,     4,     5,     4,     5,     4,     5,     3,     4,
       1,     1,     1,     4,     8,     9,     3,     4,     2,     3,
       3,     4,     4,     5,     4,     5,     3,     4,     1,     3,
       2,     1,     2,     2,     2,     3,     4,     5,     2,     4,
       5,     4,     5,     3,     4,     6,     8,     9,     3,     4,
       2,     4,     1,     2,     2,     2,     3,     4,     2,     4,
       4,     3,     6,     8,     3,     2,     4,     1,     2,     2,
       1,     1,     2,     3,     4,     2,     4,     6,     8,     1,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     5,     8,     3,     2,     3,     7,     1,     5,     5,
       6,     6,     1,     3,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     5,     8,     3,     1,     2,
       1,     2,     6,     5,     6,     7,     7,     1,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     8,
       3,     1,     1,     2,     1,     1,     2,     3,     2,     3,
       2,     3,     3,     2,     4,     3,     2,     3,     2,     4,
       3,     2,     6,     6,     6,     7,     1,     2,     1,     1,
       1,     2,     3,     2,     3,     2,     3,     3,     4,     2,
       3,     4,     2,     5,     6,     7,     6,     6,     0,     1,
       0,     2
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
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 6650 "Parser/parser.cc"
    break;

  case 3:
#line 537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6656 "Parser/parser.cc"
    break;

  case 4:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6662 "Parser/parser.cc"
    break;

  case 5:
#line 545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6668 "Parser/parser.cc"
    break;

  case 6:
#line 546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6674 "Parser/parser.cc"
    break;

  case 7:
#line 547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6680 "Parser/parser.cc"
    break;

  case 8:
#line 548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6686 "Parser/parser.cc"
    break;

  case 18:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6692 "Parser/parser.cc"
    break;

  case 19:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6698 "Parser/parser.cc"
    break;

  case 20:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6704 "Parser/parser.cc"
    break;

  case 21:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6714 "Parser/parser.cc"
    break;

  case 22:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6720 "Parser/parser.cc"
    break;

  case 23:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6726 "Parser/parser.cc"
    break;

  case 24:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6732 "Parser/parser.cc"
    break;

  case 26:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6738 "Parser/parser.cc"
    break;

  case 27:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6744 "Parser/parser.cc"
    break;

  case 28:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6750 "Parser/parser.cc"
    break;

  case 29:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6756 "Parser/parser.cc"
    break;

  case 30:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6766 "Parser/parser.cc"
    break;

  case 32:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6777 "Parser/parser.cc"
    break;

  case 33:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6786 "Parser/parser.cc"
    break;

  case 34:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6792 "Parser/parser.cc"
    break;

  case 36:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6798 "Parser/parser.cc"
    break;

  case 37:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6804 "Parser/parser.cc"
    break;

  case 38:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6814 "Parser/parser.cc"
    break;

  case 39:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6820 "Parser/parser.cc"
    break;

  case 40:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6826 "Parser/parser.cc"
    break;

  case 41:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6832 "Parser/parser.cc"
    break;

  case 42:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6838 "Parser/parser.cc"
    break;

  case 43:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6844 "Parser/parser.cc"
    break;

  case 44:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6850 "Parser/parser.cc"
    break;

  case 45:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 6856 "Parser/parser.cc"
    break;

  case 46:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6862 "Parser/parser.cc"
    break;

  case 47:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 6868 "Parser/parser.cc"
    break;

  case 48:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6874 "Parser/parser.cc"
    break;

  case 49:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6880 "Parser/parser.cc"
    break;

  case 50:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6886 "Parser/parser.cc"
    break;

  case 51:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 6892 "Parser/parser.cc"
    break;

  case 52:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 6898 "Parser/parser.cc"
    break;

  case 53:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 6904 "Parser/parser.cc"
    break;

  case 54:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 6910 "Parser/parser.cc"
    break;

  case 55:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 6920 "Parser/parser.cc"
    break;

  case 56:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 6926 "Parser/parser.cc"
    break;

  case 58:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 6932 "Parser/parser.cc"
    break;

  case 59:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6938 "Parser/parser.cc"
    break;

  case 62:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 6944 "Parser/parser.cc"
    break;

  case 64:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 6950 "Parser/parser.cc"
    break;

  case 65:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6956 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 6962 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6968 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 6974 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6980 "Parser/parser.cc"
    break;

  case 70:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 6986 "Parser/parser.cc"
    break;

  case 71:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 6992 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7000 "Parser/parser.cc"
    break;

  case 73:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7006 "Parser/parser.cc"
    break;

  case 74:
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7015 "Parser/parser.cc"
    break;

  case 77:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7021 "Parser/parser.cc"
    break;

  case 78:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7027 "Parser/parser.cc"
    break;

  case 79:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7047 "Parser/parser.cc"
    break;

  case 80:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7053 "Parser/parser.cc"
    break;

  case 81:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7059 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7065 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7071 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7077 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7083 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7089 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7095 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7104 "Parser/parser.cc"
    break;

  case 89:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7110 "Parser/parser.cc"
    break;

  case 90:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7116 "Parser/parser.cc"
    break;

  case 91:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7122 "Parser/parser.cc"
    break;

  case 92:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7128 "Parser/parser.cc"
    break;

  case 93:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7134 "Parser/parser.cc"
    break;

  case 94:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7140 "Parser/parser.cc"
    break;

  case 95:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7146 "Parser/parser.cc"
    break;

  case 97:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7152 "Parser/parser.cc"
    break;

  case 98:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7158 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7164 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7170 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7176 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7182 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7188 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7194 "Parser/parser.cc"
    break;

  case 112:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7200 "Parser/parser.cc"
    break;

  case 114:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7206 "Parser/parser.cc"
    break;

  case 115:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7212 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7218 "Parser/parser.cc"
    break;

  case 118:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7224 "Parser/parser.cc"
    break;

  case 119:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7230 "Parser/parser.cc"
    break;

  case 121:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7236 "Parser/parser.cc"
    break;

  case 122:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7242 "Parser/parser.cc"
    break;

  case 124:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7248 "Parser/parser.cc"
    break;

  case 125:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7254 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7260 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7266 "Parser/parser.cc"
    break;

  case 129:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7272 "Parser/parser.cc"
    break;

  case 130:
#line 900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7278 "Parser/parser.cc"
    break;

  case 132:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7284 "Parser/parser.cc"
    break;

  case 134:
#line 912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7290 "Parser/parser.cc"
    break;

  case 136:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7296 "Parser/parser.cc"
    break;

  case 138:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7302 "Parser/parser.cc"
    break;

  case 140:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 142:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7314 "Parser/parser.cc"
    break;

  case 143:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 146:
#line 950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7332 "Parser/parser.cc"
    break;

  case 147:
#line 958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7338 "Parser/parser.cc"
    break;

  case 148:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7344 "Parser/parser.cc"
    break;

  case 152:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7350 "Parser/parser.cc"
    break;

  case 153:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7356 "Parser/parser.cc"
    break;

  case 154:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7362 "Parser/parser.cc"
    break;

  case 155:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7368 "Parser/parser.cc"
    break;

  case 156:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7374 "Parser/parser.cc"
    break;

  case 157:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7380 "Parser/parser.cc"
    break;

  case 158:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7386 "Parser/parser.cc"
    break;

  case 159:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7392 "Parser/parser.cc"
    break;

  case 160:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7398 "Parser/parser.cc"
    break;

  case 161:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7404 "Parser/parser.cc"
    break;

  case 162:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7410 "Parser/parser.cc"
    break;

  case 163:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7416 "Parser/parser.cc"
    break;

  case 164:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7422 "Parser/parser.cc"
    break;

  case 165:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7428 "Parser/parser.cc"
    break;

  case 166:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7434 "Parser/parser.cc"
    break;

  case 168:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7440 "Parser/parser.cc"
    break;

  case 169:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7446 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7452 "Parser/parser.cc"
    break;

  case 172:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7458 "Parser/parser.cc"
    break;

  case 173:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7464 "Parser/parser.cc"
    break;

  case 185:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7470 "Parser/parser.cc"
    break;

  case 187:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7476 "Parser/parser.cc"
    break;

  case 188:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7482 "Parser/parser.cc"
    break;

  case 189:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7488 "Parser/parser.cc"
    break;

  case 190:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7494 "Parser/parser.cc"
    break;

  case 192:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7500 "Parser/parser.cc"
    break;

  case 193:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7506 "Parser/parser.cc"
    break;

  case 194:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7512 "Parser/parser.cc"
    break;

  case 195:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7518 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7524 "Parser/parser.cc"
    break;

  case 199:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7530 "Parser/parser.cc"
    break;

  case 200:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7536 "Parser/parser.cc"
    break;

  case 201:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7542 "Parser/parser.cc"
    break;

  case 202:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7548 "Parser/parser.cc"
    break;

  case 203:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7562 "Parser/parser.cc"
    break;

  case 204:
#line 1109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7568 "Parser/parser.cc"
    break;

  case 205:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7577 "Parser/parser.cc"
    break;

  case 206:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7583 "Parser/parser.cc"
    break;

  case 207:
#line 1122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7589 "Parser/parser.cc"
    break;

  case 208:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7595 "Parser/parser.cc"
    break;

  case 209:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7601 "Parser/parser.cc"
    break;

  case 210:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7607 "Parser/parser.cc"
    break;

  case 211:
#line 1133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7613 "Parser/parser.cc"
    break;

  case 212:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7619 "Parser/parser.cc"
    break;

  case 213:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7625 "Parser/parser.cc"
    break;

  case 215:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7631 "Parser/parser.cc"
    break;

  case 216:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7637 "Parser/parser.cc"
    break;

  case 217:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7643 "Parser/parser.cc"
    break;

  case 218:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7649 "Parser/parser.cc"
    break;

  case 220:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7655 "Parser/parser.cc"
    break;

  case 221:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7661 "Parser/parser.cc"
    break;

  case 222:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7667 "Parser/parser.cc"
    break;

  case 224:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7673 "Parser/parser.cc"
    break;

  case 225:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7679 "Parser/parser.cc"
    break;

  case 226:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7685 "Parser/parser.cc"
    break;

  case 227:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7691 "Parser/parser.cc"
    break;

  case 228:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7697 "Parser/parser.cc"
    break;

  case 229:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7703 "Parser/parser.cc"
    break;

  case 230:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7709 "Parser/parser.cc"
    break;

  case 231:
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7715 "Parser/parser.cc"
    break;

  case 233:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7734 "Parser/parser.cc"
    break;

  case 234:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7740 "Parser/parser.cc"
    break;

  case 235:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7746 "Parser/parser.cc"
    break;

  case 236:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7752 "Parser/parser.cc"
    break;

  case 237:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7759 "Parser/parser.cc"
    break;

  case 238:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7766 "Parser/parser.cc"
    break;

  case 239:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7772 "Parser/parser.cc"
    break;

  case 240:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7778 "Parser/parser.cc"
    break;

  case 241:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7784 "Parser/parser.cc"
    break;

  case 242:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7791 "Parser/parser.cc"
    break;

  case 243:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7798 "Parser/parser.cc"
    break;

  case 244:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7804 "Parser/parser.cc"
    break;

  case 245:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7810 "Parser/parser.cc"
    break;

  case 246:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7819 "Parser/parser.cc"
    break;

  case 247:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7825 "Parser/parser.cc"
    break;

  case 248:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7831 "Parser/parser.cc"
    break;

  case 249:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7837 "Parser/parser.cc"
    break;

  case 250:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 7843 "Parser/parser.cc"
    break;

  case 251:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 7849 "Parser/parser.cc"
    break;

  case 252:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 7855 "Parser/parser.cc"
    break;

  case 253:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 7861 "Parser/parser.cc"
    break;

  case 254:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 7867 "Parser/parser.cc"
    break;

  case 255:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 7873 "Parser/parser.cc"
    break;

  case 256:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 7879 "Parser/parser.cc"
    break;

  case 257:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 7885 "Parser/parser.cc"
    break;

  case 258:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 7891 "Parser/parser.cc"
    break;

  case 259:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 7897 "Parser/parser.cc"
    break;

  case 260:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 7903 "Parser/parser.cc"
    break;

  case 261:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 7909 "Parser/parser.cc"
    break;

  case 262:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 7915 "Parser/parser.cc"
    break;

  case 263:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 7921 "Parser/parser.cc"
    break;

  case 264:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 7927 "Parser/parser.cc"
    break;

  case 265:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 7933 "Parser/parser.cc"
    break;

  case 266:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7939 "Parser/parser.cc"
    break;

  case 267:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 268:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 269:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 7957 "Parser/parser.cc"
    break;

  case 270:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 271:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 7969 "Parser/parser.cc"
    break;

  case 272:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 273:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 274:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 275:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 278:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 8001 "Parser/parser.cc"
    break;

  case 279:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Mutex statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8007 "Parser/parser.cc"
    break;

  case 280:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8013 "Parser/parser.cc"
    break;

  case 281:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8019 "Parser/parser.cc"
    break;

  case 283:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8025 "Parser/parser.cc"
    break;

  case 284:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8031 "Parser/parser.cc"
    break;

  case 286:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8037 "Parser/parser.cc"
    break;

  case 287:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8043 "Parser/parser.cc"
    break;

  case 288:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8049 "Parser/parser.cc"
    break;

  case 289:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8055 "Parser/parser.cc"
    break;

  case 290:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8061 "Parser/parser.cc"
    break;

  case 291:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8067 "Parser/parser.cc"
    break;

  case 292:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8073 "Parser/parser.cc"
    break;

  case 293:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8079 "Parser/parser.cc"
    break;

  case 294:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8085 "Parser/parser.cc"
    break;

  case 295:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8091 "Parser/parser.cc"
    break;

  case 296:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8097 "Parser/parser.cc"
    break;

  case 297:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8103 "Parser/parser.cc"
    break;

  case 298:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8109 "Parser/parser.cc"
    break;

  case 299:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8115 "Parser/parser.cc"
    break;

  case 300:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8121 "Parser/parser.cc"
    break;

  case 301:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8127 "Parser/parser.cc"
    break;

  case 302:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8133 "Parser/parser.cc"
    break;

  case 303:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8139 "Parser/parser.cc"
    break;

  case 304:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8145 "Parser/parser.cc"
    break;

  case 305:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8151 "Parser/parser.cc"
    break;

  case 306:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8157 "Parser/parser.cc"
    break;

  case 307:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8163 "Parser/parser.cc"
    break;

  case 309:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8169 "Parser/parser.cc"
    break;

  case 310:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8175 "Parser/parser.cc"
    break;

  case 311:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8181 "Parser/parser.cc"
    break;

  case 316:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8187 "Parser/parser.cc"
    break;

  case 317:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8193 "Parser/parser.cc"
    break;

  case 318:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8199 "Parser/parser.cc"
    break;

  case 319:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8205 "Parser/parser.cc"
    break;

  case 320:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8211 "Parser/parser.cc"
    break;

  case 321:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8217 "Parser/parser.cc"
    break;

  case 322:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8223 "Parser/parser.cc"
    break;

  case 323:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8229 "Parser/parser.cc"
    break;

  case 326:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8235 "Parser/parser.cc"
    break;

  case 327:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8241 "Parser/parser.cc"
    break;

  case 328:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8247 "Parser/parser.cc"
    break;

  case 329:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8253 "Parser/parser.cc"
    break;

  case 330:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8259 "Parser/parser.cc"
    break;

  case 331:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8265 "Parser/parser.cc"
    break;

  case 332:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8274 "Parser/parser.cc"
    break;

  case 333:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8283 "Parser/parser.cc"
    break;

  case 334:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8289 "Parser/parser.cc"
    break;

  case 337:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8295 "Parser/parser.cc"
    break;

  case 338:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8301 "Parser/parser.cc"
    break;

  case 340:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8307 "Parser/parser.cc"
    break;

  case 341:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8313 "Parser/parser.cc"
    break;

  case 351:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8319 "Parser/parser.cc"
    break;

  case 352:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8325 "Parser/parser.cc"
    break;

  case 356:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8331 "Parser/parser.cc"
    break;

  case 358:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8337 "Parser/parser.cc"
    break;

  case 359:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8343 "Parser/parser.cc"
    break;

  case 360:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8349 "Parser/parser.cc"
    break;

  case 361:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8355 "Parser/parser.cc"
    break;

  case 362:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8361 "Parser/parser.cc"
    break;

  case 363:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8367 "Parser/parser.cc"
    break;

  case 365:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8373 "Parser/parser.cc"
    break;

  case 366:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8379 "Parser/parser.cc"
    break;

  case 367:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8385 "Parser/parser.cc"
    break;

  case 368:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8396 "Parser/parser.cc"
    break;

  case 369:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8402 "Parser/parser.cc"
    break;

  case 370:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8408 "Parser/parser.cc"
    break;

  case 371:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8414 "Parser/parser.cc"
    break;

  case 372:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8420 "Parser/parser.cc"
    break;

  case 373:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8429 "Parser/parser.cc"
    break;

  case 374:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8438 "Parser/parser.cc"
    break;

  case 375:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8447 "Parser/parser.cc"
    break;

  case 376:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8456 "Parser/parser.cc"
    break;

  case 377:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8465 "Parser/parser.cc"
    break;

  case 378:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8474 "Parser/parser.cc"
    break;

  case 379:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8483 "Parser/parser.cc"
    break;

  case 380:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8492 "Parser/parser.cc"
    break;

  case 381:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8500 "Parser/parser.cc"
    break;

  case 382:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8508 "Parser/parser.cc"
    break;

  case 383:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 387:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8520 "Parser/parser.cc"
    break;

  case 388:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8526 "Parser/parser.cc"
    break;

  case 401:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8532 "Parser/parser.cc"
    break;

  case 404:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8538 "Parser/parser.cc"
    break;

  case 407:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8544 "Parser/parser.cc"
    break;

  case 408:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8550 "Parser/parser.cc"
    break;

  case 409:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8556 "Parser/parser.cc"
    break;

  case 410:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8562 "Parser/parser.cc"
    break;

  case 412:
#line 1822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8568 "Parser/parser.cc"
    break;

  case 414:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8574 "Parser/parser.cc"
    break;

  case 415:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8580 "Parser/parser.cc"
    break;

  case 417:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8586 "Parser/parser.cc"
    break;

  case 418:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8592 "Parser/parser.cc"
    break;

  case 419:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8598 "Parser/parser.cc"
    break;

  case 420:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8604 "Parser/parser.cc"
    break;

  case 421:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8610 "Parser/parser.cc"
    break;

  case 422:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8616 "Parser/parser.cc"
    break;

  case 423:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8622 "Parser/parser.cc"
    break;

  case 424:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8628 "Parser/parser.cc"
    break;

  case 425:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8634 "Parser/parser.cc"
    break;

  case 426:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8640 "Parser/parser.cc"
    break;

  case 427:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8646 "Parser/parser.cc"
    break;

  case 428:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8652 "Parser/parser.cc"
    break;

  case 429:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8658 "Parser/parser.cc"
    break;

  case 430:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8664 "Parser/parser.cc"
    break;

  case 431:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8670 "Parser/parser.cc"
    break;

  case 432:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8676 "Parser/parser.cc"
    break;

  case 433:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8682 "Parser/parser.cc"
    break;

  case 434:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8688 "Parser/parser.cc"
    break;

  case 435:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8694 "Parser/parser.cc"
    break;

  case 436:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8700 "Parser/parser.cc"
    break;

  case 437:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8706 "Parser/parser.cc"
    break;

  case 438:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8712 "Parser/parser.cc"
    break;

  case 439:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8718 "Parser/parser.cc"
    break;

  case 440:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8724 "Parser/parser.cc"
    break;

  case 441:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8730 "Parser/parser.cc"
    break;

  case 442:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8736 "Parser/parser.cc"
    break;

  case 443:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8742 "Parser/parser.cc"
    break;

  case 444:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8748 "Parser/parser.cc"
    break;

  case 445:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8754 "Parser/parser.cc"
    break;

  case 446:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8760 "Parser/parser.cc"
    break;

  case 447:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8766 "Parser/parser.cc"
    break;

  case 448:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8772 "Parser/parser.cc"
    break;

  case 449:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8778 "Parser/parser.cc"
    break;

  case 450:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8784 "Parser/parser.cc"
    break;

  case 451:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8790 "Parser/parser.cc"
    break;

  case 452:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8796 "Parser/parser.cc"
    break;

  case 454:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8802 "Parser/parser.cc"
    break;

  case 456:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8808 "Parser/parser.cc"
    break;

  case 457:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8814 "Parser/parser.cc"
    break;

  case 458:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8820 "Parser/parser.cc"
    break;

  case 460:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 461:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8832 "Parser/parser.cc"
    break;

  case 462:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8838 "Parser/parser.cc"
    break;

  case 463:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 8844 "Parser/parser.cc"
    break;

  case 465:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8850 "Parser/parser.cc"
    break;

  case 467:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8856 "Parser/parser.cc"
    break;

  case 468:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8862 "Parser/parser.cc"
    break;

  case 469:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 8868 "Parser/parser.cc"
    break;

  case 470:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 8874 "Parser/parser.cc"
    break;

  case 471:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 8880 "Parser/parser.cc"
    break;

  case 472:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 8886 "Parser/parser.cc"
    break;

  case 473:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 8892 "Parser/parser.cc"
    break;

  case 474:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 8898 "Parser/parser.cc"
    break;

  case 475:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 8904 "Parser/parser.cc"
    break;

  case 477:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8910 "Parser/parser.cc"
    break;

  case 478:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8916 "Parser/parser.cc"
    break;

  case 479:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8922 "Parser/parser.cc"
    break;

  case 481:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 8928 "Parser/parser.cc"
    break;

  case 482:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 8934 "Parser/parser.cc"
    break;

  case 483:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 8943 "Parser/parser.cc"
    break;

  case 485:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8949 "Parser/parser.cc"
    break;

  case 486:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8955 "Parser/parser.cc"
    break;

  case 487:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8961 "Parser/parser.cc"
    break;

  case 489:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8967 "Parser/parser.cc"
    break;

  case 490:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8973 "Parser/parser.cc"
    break;

  case 492:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8979 "Parser/parser.cc"
    break;

  case 493:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8985 "Parser/parser.cc"
    break;

  case 494:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8991 "Parser/parser.cc"
    break;

  case 496:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8997 "Parser/parser.cc"
    break;

  case 497:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9003 "Parser/parser.cc"
    break;

  case 498:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9009 "Parser/parser.cc"
    break;

  case 499:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9015 "Parser/parser.cc"
    break;

  case 500:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9021 "Parser/parser.cc"
    break;

  case 502:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9027 "Parser/parser.cc"
    break;

  case 503:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9033 "Parser/parser.cc"
    break;

  case 504:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9039 "Parser/parser.cc"
    break;

  case 505:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9045 "Parser/parser.cc"
    break;

  case 506:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9051 "Parser/parser.cc"
    break;

  case 511:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9057 "Parser/parser.cc"
    break;

  case 512:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9063 "Parser/parser.cc"
    break;

  case 513:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9072 "Parser/parser.cc"
    break;

  case 514:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9078 "Parser/parser.cc"
    break;

  case 515:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9087 "Parser/parser.cc"
    break;

  case 516:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9096 "Parser/parser.cc"
    break;

  case 517:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9105 "Parser/parser.cc"
    break;

  case 518:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9114 "Parser/parser.cc"
    break;

  case 520:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9120 "Parser/parser.cc"
    break;

  case 521:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9126 "Parser/parser.cc"
    break;

  case 522:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9136 "Parser/parser.cc"
    break;

  case 523:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9151 "Parser/parser.cc"
    break;

  case 526:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9157 "Parser/parser.cc"
    break;

  case 527:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9163 "Parser/parser.cc"
    break;

  case 528:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9169 "Parser/parser.cc"
    break;

  case 529:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9175 "Parser/parser.cc"
    break;

  case 530:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9181 "Parser/parser.cc"
    break;

  case 531:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9187 "Parser/parser.cc"
    break;

  case 532:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9193 "Parser/parser.cc"
    break;

  case 533:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9199 "Parser/parser.cc"
    break;

  case 534:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9205 "Parser/parser.cc"
    break;

  case 535:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9211 "Parser/parser.cc"
    break;

  case 536:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9217 "Parser/parser.cc"
    break;

  case 537:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9223 "Parser/parser.cc"
    break;

  case 538:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9229 "Parser/parser.cc"
    break;

  case 539:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9235 "Parser/parser.cc"
    break;

  case 540:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9241 "Parser/parser.cc"
    break;

  case 541:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9254 "Parser/parser.cc"
    break;

  case 542:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9260 "Parser/parser.cc"
    break;

  case 545:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9266 "Parser/parser.cc"
    break;

  case 546:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9272 "Parser/parser.cc"
    break;

  case 549:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9278 "Parser/parser.cc"
    break;

  case 551:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9284 "Parser/parser.cc"
    break;

  case 552:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9290 "Parser/parser.cc"
    break;

  case 553:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9296 "Parser/parser.cc"
    break;

  case 554:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9302 "Parser/parser.cc"
    break;

  case 555:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9308 "Parser/parser.cc"
    break;

  case 557:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 559:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9320 "Parser/parser.cc"
    break;

  case 560:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9326 "Parser/parser.cc"
    break;

  case 562:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9332 "Parser/parser.cc"
    break;

  case 563:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9338 "Parser/parser.cc"
    break;

  case 565:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9344 "Parser/parser.cc"
    break;

  case 566:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9350 "Parser/parser.cc"
    break;

  case 567:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9356 "Parser/parser.cc"
    break;

  case 568:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 569:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 570:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9377 "Parser/parser.cc"
    break;

  case 571:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9386 "Parser/parser.cc"
    break;

  case 572:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9394 "Parser/parser.cc"
    break;

  case 573:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9404 "Parser/parser.cc"
    break;

  case 575:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9410 "Parser/parser.cc"
    break;

  case 576:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9416 "Parser/parser.cc"
    break;

  case 577:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9422 "Parser/parser.cc"
    break;

  case 578:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9428 "Parser/parser.cc"
    break;

  case 579:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9434 "Parser/parser.cc"
    break;

  case 580:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9440 "Parser/parser.cc"
    break;

  case 581:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9446 "Parser/parser.cc"
    break;

  case 582:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9452 "Parser/parser.cc"
    break;

  case 583:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9458 "Parser/parser.cc"
    break;

  case 584:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9464 "Parser/parser.cc"
    break;

  case 587:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9470 "Parser/parser.cc"
    break;

  case 588:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9476 "Parser/parser.cc"
    break;

  case 589:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9482 "Parser/parser.cc"
    break;

  case 591:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9488 "Parser/parser.cc"
    break;

  case 592:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9494 "Parser/parser.cc"
    break;

  case 593:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9500 "Parser/parser.cc"
    break;

  case 595:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9506 "Parser/parser.cc"
    break;

  case 596:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9512 "Parser/parser.cc"
    break;

  case 597:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9518 "Parser/parser.cc"
    break;

  case 599:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9524 "Parser/parser.cc"
    break;

  case 602:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9530 "Parser/parser.cc"
    break;

  case 603:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9536 "Parser/parser.cc"
    break;

  case 605:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9542 "Parser/parser.cc"
    break;

  case 606:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9548 "Parser/parser.cc"
    break;

  case 607:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9554 "Parser/parser.cc"
    break;

  case 612:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9560 "Parser/parser.cc"
    break;

  case 614:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9566 "Parser/parser.cc"
    break;

  case 615:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9572 "Parser/parser.cc"
    break;

  case 616:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9578 "Parser/parser.cc"
    break;

  case 617:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9584 "Parser/parser.cc"
    break;

  case 618:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9590 "Parser/parser.cc"
    break;

  case 619:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 625:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 628:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9608 "Parser/parser.cc"
    break;

  case 629:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9614 "Parser/parser.cc"
    break;

  case 630:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9620 "Parser/parser.cc"
    break;

  case 631:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 632:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9632 "Parser/parser.cc"
    break;

  case 633:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9638 "Parser/parser.cc"
    break;

  case 635:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9644 "Parser/parser.cc"
    break;

  case 636:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 637:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9656 "Parser/parser.cc"
    break;

  case 639:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 641:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9668 "Parser/parser.cc"
    break;

  case 642:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 643:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9680 "Parser/parser.cc"
    break;

  case 644:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9686 "Parser/parser.cc"
    break;

  case 645:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 646:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9698 "Parser/parser.cc"
    break;

  case 648:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 649:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9710 "Parser/parser.cc"
    break;

  case 650:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9716 "Parser/parser.cc"
    break;

  case 651:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9727 "Parser/parser.cc"
    break;

  case 652:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 653:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9739 "Parser/parser.cc"
    break;

  case 654:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 655:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9754 "Parser/parser.cc"
    break;

  case 656:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9760 "Parser/parser.cc"
    break;

  case 657:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9766 "Parser/parser.cc"
    break;

  case 658:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9772 "Parser/parser.cc"
    break;

  case 659:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9778 "Parser/parser.cc"
    break;

  case 660:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9784 "Parser/parser.cc"
    break;

  case 661:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9790 "Parser/parser.cc"
    break;

  case 662:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9796 "Parser/parser.cc"
    break;

  case 663:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9802 "Parser/parser.cc"
    break;

  case 664:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9808 "Parser/parser.cc"
    break;

  case 665:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9814 "Parser/parser.cc"
    break;

  case 668:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9820 "Parser/parser.cc"
    break;

  case 669:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9826 "Parser/parser.cc"
    break;

  case 670:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9832 "Parser/parser.cc"
    break;

  case 671:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 9838 "Parser/parser.cc"
    break;

  case 673:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 9844 "Parser/parser.cc"
    break;

  case 674:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9850 "Parser/parser.cc"
    break;

  case 675:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9856 "Parser/parser.cc"
    break;

  case 676:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9862 "Parser/parser.cc"
    break;

  case 677:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 9868 "Parser/parser.cc"
    break;

  case 678:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 9874 "Parser/parser.cc"
    break;

  case 679:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 9880 "Parser/parser.cc"
    break;

  case 680:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 9889 "Parser/parser.cc"
    break;

  case 681:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 9898 "Parser/parser.cc"
    break;

  case 682:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 9904 "Parser/parser.cc"
    break;

  case 683:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 9910 "Parser/parser.cc"
    break;

  case 685:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 9916 "Parser/parser.cc"
    break;

  case 690:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9922 "Parser/parser.cc"
    break;

  case 691:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9928 "Parser/parser.cc"
    break;

  case 692:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 9934 "Parser/parser.cc"
    break;

  case 694:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 9940 "Parser/parser.cc"
    break;

  case 695:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9946 "Parser/parser.cc"
    break;

  case 696:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 9952 "Parser/parser.cc"
    break;

  case 697:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9958 "Parser/parser.cc"
    break;

  case 699:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 9964 "Parser/parser.cc"
    break;

  case 700:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 9970 "Parser/parser.cc"
    break;

  case 701:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 9976 "Parser/parser.cc"
    break;

  case 704:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 9985 "Parser/parser.cc"
    break;

  case 705:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 9991 "Parser/parser.cc"
    break;

  case 706:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10000 "Parser/parser.cc"
    break;

  case 707:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10010 "Parser/parser.cc"
    break;

  case 708:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10019 "Parser/parser.cc"
    break;

  case 709:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10029 "Parser/parser.cc"
    break;

  case 710:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10038 "Parser/parser.cc"
    break;

  case 711:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10048 "Parser/parser.cc"
    break;

  case 712:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10057 "Parser/parser.cc"
    break;

  case 713:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10067 "Parser/parser.cc"
    break;

  case 715:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10073 "Parser/parser.cc"
    break;

  case 716:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10079 "Parser/parser.cc"
    break;

  case 717:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10085 "Parser/parser.cc"
    break;

  case 718:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10091 "Parser/parser.cc"
    break;

  case 719:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10102 "Parser/parser.cc"
    break;

  case 720:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10111 "Parser/parser.cc"
    break;

  case 721:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10120 "Parser/parser.cc"
    break;

  case 722:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10126 "Parser/parser.cc"
    break;

  case 723:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10132 "Parser/parser.cc"
    break;

  case 724:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10138 "Parser/parser.cc"
    break;

  case 725:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10147 "Parser/parser.cc"
    break;

  case 726:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10153 "Parser/parser.cc"
    break;

  case 727:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10159 "Parser/parser.cc"
    break;

  case 728:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 732:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 733:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10177 "Parser/parser.cc"
    break;

  case 734:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10187 "Parser/parser.cc"
    break;

  case 735:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10193 "Parser/parser.cc"
    break;

  case 738:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10199 "Parser/parser.cc"
    break;

  case 739:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10205 "Parser/parser.cc"
    break;

  case 741:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10211 "Parser/parser.cc"
    break;

  case 742:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10217 "Parser/parser.cc"
    break;

  case 743:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10223 "Parser/parser.cc"
    break;

  case 744:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10229 "Parser/parser.cc"
    break;

  case 749:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10235 "Parser/parser.cc"
    break;

  case 750:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10241 "Parser/parser.cc"
    break;

  case 751:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10247 "Parser/parser.cc"
    break;

  case 752:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10253 "Parser/parser.cc"
    break;

  case 753:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10259 "Parser/parser.cc"
    break;

  case 755:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10265 "Parser/parser.cc"
    break;

  case 756:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10271 "Parser/parser.cc"
    break;

  case 757:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10277 "Parser/parser.cc"
    break;

  case 758:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10283 "Parser/parser.cc"
    break;

  case 759:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10289 "Parser/parser.cc"
    break;

  case 760:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10295 "Parser/parser.cc"
    break;

  case 761:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10301 "Parser/parser.cc"
    break;

  case 762:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10307 "Parser/parser.cc"
    break;

  case 763:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10313 "Parser/parser.cc"
    break;

  case 764:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10319 "Parser/parser.cc"
    break;

  case 765:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10325 "Parser/parser.cc"
    break;

  case 766:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10331 "Parser/parser.cc"
    break;

  case 767:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10337 "Parser/parser.cc"
    break;

  case 768:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10343 "Parser/parser.cc"
    break;

  case 769:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10349 "Parser/parser.cc"
    break;

  case 770:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10355 "Parser/parser.cc"
    break;

  case 771:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10361 "Parser/parser.cc"
    break;

  case 772:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 774:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10373 "Parser/parser.cc"
    break;

  case 775:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 776:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 777:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 778:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10397 "Parser/parser.cc"
    break;

  case 779:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 780:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 781:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 782:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10421 "Parser/parser.cc"
    break;

  case 783:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10427 "Parser/parser.cc"
    break;

  case 784:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10433 "Parser/parser.cc"
    break;

  case 785:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10439 "Parser/parser.cc"
    break;

  case 786:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10445 "Parser/parser.cc"
    break;

  case 787:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10451 "Parser/parser.cc"
    break;

  case 788:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10457 "Parser/parser.cc"
    break;

  case 789:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 793:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 794:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 795:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 796:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10487 "Parser/parser.cc"
    break;

  case 797:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 798:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 799:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10505 "Parser/parser.cc"
    break;

  case 800:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10511 "Parser/parser.cc"
    break;

  case 801:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 802:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10523 "Parser/parser.cc"
    break;

  case 803:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10529 "Parser/parser.cc"
    break;

  case 804:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 805:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 806:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10547 "Parser/parser.cc"
    break;

  case 807:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 808:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10562 "Parser/parser.cc"
    break;

  case 809:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10568 "Parser/parser.cc"
    break;

  case 810:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10574 "Parser/parser.cc"
    break;

  case 812:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10580 "Parser/parser.cc"
    break;

  case 813:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10586 "Parser/parser.cc"
    break;

  case 814:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10592 "Parser/parser.cc"
    break;

  case 815:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10598 "Parser/parser.cc"
    break;

  case 816:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10604 "Parser/parser.cc"
    break;

  case 817:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10610 "Parser/parser.cc"
    break;

  case 818:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 819:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 820:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10628 "Parser/parser.cc"
    break;

  case 821:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10634 "Parser/parser.cc"
    break;

  case 822:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10640 "Parser/parser.cc"
    break;

  case 823:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10646 "Parser/parser.cc"
    break;

  case 824:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 825:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10658 "Parser/parser.cc"
    break;

  case 826:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 827:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 828:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10676 "Parser/parser.cc"
    break;

  case 829:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 830:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10688 "Parser/parser.cc"
    break;

  case 831:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 833:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 834:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10706 "Parser/parser.cc"
    break;

  case 835:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 836:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 837:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10724 "Parser/parser.cc"
    break;

  case 838:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 839:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10736 "Parser/parser.cc"
    break;

  case 840:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 841:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10748 "Parser/parser.cc"
    break;

  case 842:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10754 "Parser/parser.cc"
    break;

  case 843:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10760 "Parser/parser.cc"
    break;

  case 844:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10766 "Parser/parser.cc"
    break;

  case 845:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10772 "Parser/parser.cc"
    break;

  case 846:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10778 "Parser/parser.cc"
    break;

  case 848:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10784 "Parser/parser.cc"
    break;

  case 849:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 850:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 851:
#line 3221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10802 "Parser/parser.cc"
    break;

  case 852:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 853:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 854:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10820 "Parser/parser.cc"
    break;

  case 855:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 856:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 857:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10838 "Parser/parser.cc"
    break;

  case 858:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 860:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 861:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10856 "Parser/parser.cc"
    break;

  case 862:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 10862 "Parser/parser.cc"
    break;

  case 863:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 864:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10874 "Parser/parser.cc"
    break;

  case 865:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 866:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 868:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10892 "Parser/parser.cc"
    break;

  case 869:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10898 "Parser/parser.cc"
    break;

  case 870:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10904 "Parser/parser.cc"
    break;

  case 871:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 10910 "Parser/parser.cc"
    break;

  case 872:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 873:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10922 "Parser/parser.cc"
    break;

  case 874:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 10928 "Parser/parser.cc"
    break;

  case 875:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 876:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 878:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 10946 "Parser/parser.cc"
    break;

  case 879:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 10952 "Parser/parser.cc"
    break;

  case 880:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 10958 "Parser/parser.cc"
    break;

  case 881:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 10964 "Parser/parser.cc"
    break;

  case 883:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 10970 "Parser/parser.cc"
    break;

  case 884:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10976 "Parser/parser.cc"
    break;

  case 885:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10982 "Parser/parser.cc"
    break;

  case 886:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 10988 "Parser/parser.cc"
    break;

  case 887:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 10994 "Parser/parser.cc"
    break;

  case 888:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11000 "Parser/parser.cc"
    break;

  case 889:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11006 "Parser/parser.cc"
    break;

  case 890:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11012 "Parser/parser.cc"
    break;

  case 892:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11018 "Parser/parser.cc"
    break;

  case 893:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11024 "Parser/parser.cc"
    break;

  case 894:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11030 "Parser/parser.cc"
    break;

  case 895:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11036 "Parser/parser.cc"
    break;

  case 896:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11042 "Parser/parser.cc"
    break;

  case 897:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11048 "Parser/parser.cc"
    break;

  case 899:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11054 "Parser/parser.cc"
    break;

  case 901:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11060 "Parser/parser.cc"
    break;

  case 902:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11066 "Parser/parser.cc"
    break;

  case 903:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11072 "Parser/parser.cc"
    break;

  case 904:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11078 "Parser/parser.cc"
    break;

  case 905:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11084 "Parser/parser.cc"
    break;

  case 906:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11090 "Parser/parser.cc"
    break;

  case 908:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11096 "Parser/parser.cc"
    break;

  case 909:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11102 "Parser/parser.cc"
    break;

  case 910:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11108 "Parser/parser.cc"
    break;

  case 911:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11114 "Parser/parser.cc"
    break;

  case 912:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11120 "Parser/parser.cc"
    break;

  case 913:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11126 "Parser/parser.cc"
    break;

  case 914:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11132 "Parser/parser.cc"
    break;

  case 916:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11138 "Parser/parser.cc"
    break;

  case 917:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11144 "Parser/parser.cc"
    break;

  case 918:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11150 "Parser/parser.cc"
    break;

  case 919:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11156 "Parser/parser.cc"
    break;

  case 920:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11162 "Parser/parser.cc"
    break;

  case 923:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11168 "Parser/parser.cc"
    break;

  case 926:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11174 "Parser/parser.cc"
    break;

  case 927:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11180 "Parser/parser.cc"
    break;

  case 928:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11186 "Parser/parser.cc"
    break;

  case 929:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11192 "Parser/parser.cc"
    break;

  case 930:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11198 "Parser/parser.cc"
    break;

  case 931:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11204 "Parser/parser.cc"
    break;

  case 932:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11210 "Parser/parser.cc"
    break;

  case 933:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 934:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 935:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11228 "Parser/parser.cc"
    break;

  case 936:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 937:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11240 "Parser/parser.cc"
    break;

  case 938:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11246 "Parser/parser.cc"
    break;

  case 939:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 940:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 941:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 942:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11270 "Parser/parser.cc"
    break;

  case 943:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11276 "Parser/parser.cc"
    break;

  case 944:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11282 "Parser/parser.cc"
    break;

  case 945:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11288 "Parser/parser.cc"
    break;

  case 947:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11294 "Parser/parser.cc"
    break;

  case 951:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11300 "Parser/parser.cc"
    break;

  case 952:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11306 "Parser/parser.cc"
    break;

  case 953:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11312 "Parser/parser.cc"
    break;

  case 954:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11318 "Parser/parser.cc"
    break;

  case 955:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11324 "Parser/parser.cc"
    break;

  case 956:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 957:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 958:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 959:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 960:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 961:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 962:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11366 "Parser/parser.cc"
    break;

  case 963:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 964:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11378 "Parser/parser.cc"
    break;

  case 965:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11384 "Parser/parser.cc"
    break;

  case 966:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11390 "Parser/parser.cc"
    break;

  case 967:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11396 "Parser/parser.cc"
    break;

  case 970:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11402 "Parser/parser.cc"
    break;

  case 971:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11408 "Parser/parser.cc"
    break;


#line 11412 "Parser/parser.cc"

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
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
