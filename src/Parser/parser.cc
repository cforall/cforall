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
#line 229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
#define YYLAST   18826

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
       0,   529,   529,   533,   540,   541,   542,   543,   544,   548,
     549,   550,   551,   552,   553,   554,   558,   559,   560,   565,
     569,   570,   581,   583,   585,   589,   590,   592,   594,   596,
     598,   611,   612,   622,   627,   632,   633,   639,   645,   651,
     653,   655,   657,   659,   661,   663,   665,   667,   669,   671,
     673,   675,   677,   679,   681,   683,   693,   694,   695,   700,
     703,   707,   708,   712,   713,   715,   717,   719,   721,   723,
     728,   730,   732,   740,   741,   749,   752,   753,   755,   760,
     776,   778,   780,   782,   784,   786,   788,   790,   792,   800,
     801,   803,   807,   808,   809,   810,   814,   815,   817,   819,
     821,   823,   825,   827,   829,   836,   837,   838,   839,   843,
     844,   848,   849,   854,   855,   857,   859,   864,   865,   867,
     872,   873,   875,   880,   881,   883,   885,   887,   892,   893,
     895,   900,   901,   906,   907,   912,   913,   918,   919,   924,
     925,   930,   931,   934,   939,   944,   945,   953,   959,   960,
     964,   965,   969,   970,   974,   975,   976,   977,   978,   979,
     980,   981,   982,   983,   984,   994,   996,  1001,  1002,  1004,
    1006,  1011,  1012,  1018,  1019,  1025,  1026,  1027,  1028,  1029,
    1030,  1031,  1032,  1033,  1034,  1035,  1037,  1038,  1044,  1049,
    1051,  1059,  1060,  1065,  1067,  1069,  1071,  1073,  1077,  1078,
    1083,  1090,  1092,  1094,  1104,  1106,  1114,  1117,  1122,  1124,
    1126,  1128,  1136,  1137,  1139,  1143,  1145,  1149,  1150,  1161,
    1162,  1166,  1171,  1172,  1176,  1178,  1183,  1185,  1187,  1189,
    1191,  1193,  1198,  1199,  1221,  1223,  1225,  1228,  1231,  1234,
    1236,  1238,  1240,  1243,  1246,  1248,  1251,  1258,  1260,  1262,
    1264,  1266,  1271,  1273,  1275,  1277,  1282,  1284,  1289,  1291,
    1293,  1295,  1298,  1302,  1305,  1309,  1311,  1313,  1315,  1317,
    1319,  1321,  1323,  1325,  1327,  1329,  1334,  1335,  1339,  1347,
    1352,  1357,  1358,  1362,  1366,  1371,  1372,  1378,  1382,  1384,
    1386,  1388,  1391,  1393,  1398,  1400,  1405,  1407,  1409,  1414,
    1416,  1422,  1423,  1427,  1428,  1429,  1430,  1434,  1439,  1440,
    1442,  1444,  1446,  1450,  1454,  1455,  1459,  1461,  1463,  1465,
    1467,  1473,  1474,  1480,  1481,  1485,  1486,  1491,  1493,  1499,
    1500,  1502,  1507,  1512,  1523,  1524,  1528,  1529,  1535,  1536,
    1540,  1542,  1546,  1548,  1552,  1553,  1557,  1558,  1562,  1563,
    1564,  1568,  1570,  1585,  1586,  1587,  1588,  1590,  1594,  1596,
    1600,  1607,  1609,  1611,  1616,  1617,  1619,  1621,  1623,  1655,
    1658,  1663,  1665,  1671,  1676,  1681,  1692,  1697,  1702,  1707,
    1712,  1721,  1725,  1732,  1734,  1735,  1736,  1742,  1744,  1749,
    1750,  1751,  1760,  1761,  1762,  1766,  1767,  1768,  1777,  1778,
    1779,  1784,  1785,  1794,  1795,  1800,  1801,  1805,  1807,  1809,
    1811,  1813,  1817,  1822,  1823,  1825,  1835,  1836,  1841,  1843,
    1845,  1847,  1849,  1852,  1854,  1856,  1861,  1863,  1865,  1867,
    1869,  1871,  1873,  1875,  1877,  1879,  1881,  1883,  1885,  1887,
    1889,  1891,  1893,  1895,  1897,  1899,  1901,  1903,  1905,  1907,
    1909,  1911,  1913,  1915,  1920,  1921,  1925,  1932,  1933,  1939,
    1940,  1942,  1944,  1946,  1951,  1953,  1958,  1959,  1961,  1963,
    1968,  1970,  1972,  1974,  1976,  1978,  1983,  1984,  1986,  1988,
    1993,  1995,  1994,  1998,  2006,  2007,  2009,  2011,  2016,  2017,
    2019,  2024,  2025,  2027,  2029,  2034,  2035,  2037,  2042,  2044,
    2046,  2048,  2049,  2051,  2056,  2058,  2060,  2065,  2066,  2070,
    2071,  2076,  2075,  2080,  2079,  2087,  2086,  2097,  2096,  2106,
    2111,  2112,  2117,  2123,  2137,  2138,  2142,  2144,  2146,  2152,
    2154,  2156,  2158,  2160,  2162,  2164,  2166,  2172,  2173,  2178,
    2180,  2182,  2191,  2193,  2194,  2195,  2197,  2199,  2200,  2205,
    2206,  2207,  2212,  2214,  2217,  2224,  2225,  2226,  2232,  2237,
    2239,  2245,  2246,  2252,  2253,  2257,  2262,  2265,  2264,  2268,
    2271,  2277,  2276,  2285,  2291,  2295,  2297,  2302,  2304,  2306,
    2308,  2314,  2317,  2323,  2324,  2326,  2327,  2328,  2330,  2332,
    2339,  2340,  2342,  2344,  2349,  2350,  2356,  2357,  2359,  2360,
    2365,  2366,  2367,  2369,  2377,  2378,  2380,  2383,  2385,  2389,
    2390,  2391,  2393,  2395,  2400,  2402,  2407,  2409,  2418,  2420,
    2425,  2426,  2427,  2431,  2432,  2433,  2438,  2439,  2444,  2445,
    2446,  2450,  2451,  2456,  2457,  2458,  2459,  2460,  2475,  2476,
    2481,  2482,  2488,  2490,  2493,  2495,  2497,  2520,  2521,  2527,
    2528,  2534,  2533,  2543,  2542,  2546,  2552,  2558,  2559,  2561,
    2565,  2570,  2572,  2574,  2576,  2582,  2583,  2587,  2588,  2593,
    2595,  2602,  2604,  2605,  2607,  2612,  2614,  2616,  2621,  2623,
    2628,  2633,  2641,  2643,  2648,  2649,  2654,  2655,  2659,  2660,
    2661,  2666,  2668,  2674,  2676,  2681,  2683,  2689,  2690,  2694,
    2698,  2702,  2704,  2705,  2706,  2711,  2714,  2713,  2725,  2724,
    2736,  2735,  2747,  2746,  2760,  2766,  2768,  2774,  2775,  2780,
    2787,  2792,  2798,  2801,  2804,  2808,  2814,  2817,  2820,  2825,
    2826,  2827,  2831,  2837,  2838,  2848,  2849,  2853,  2854,  2859,
    2864,  2865,  2871,  2872,  2874,  2879,  2880,  2881,  2882,  2883,
    2885,  2920,  2922,  2927,  2929,  2930,  2932,  2937,  2939,  2941,
    2943,  2948,  2950,  2952,  2954,  2956,  2958,  2960,  2965,  2967,
    2969,  2971,  2980,  2982,  2983,  2988,  2990,  2992,  2994,  2996,
    3001,  3003,  3005,  3007,  3012,  3014,  3016,  3018,  3020,  3022,
    3034,  3035,  3036,  3040,  3042,  3044,  3046,  3048,  3053,  3055,
    3057,  3059,  3064,  3066,  3068,  3070,  3072,  3074,  3089,  3094,
    3099,  3101,  3102,  3104,  3109,  3111,  3113,  3115,  3120,  3122,
    3124,  3126,  3128,  3130,  3132,  3137,  3139,  3141,  3143,  3145,
    3155,  3157,  3159,  3160,  3162,  3167,  3169,  3171,  3176,  3178,
    3180,  3182,  3187,  3189,  3191,  3205,  3207,  3209,  3210,  3212,
    3217,  3219,  3224,  3226,  3228,  3233,  3235,  3240,  3242,  3259,
    3260,  3262,  3267,  3269,  3271,  3273,  3275,  3280,  3281,  3283,
    3285,  3290,  3292,  3294,  3300,  3302,  3304,  3307,  3311,  3313,
    3315,  3317,  3351,  3352,  3354,  3356,  3361,  3363,  3365,  3367,
    3369,  3374,  3375,  3377,  3379,  3384,  3386,  3388,  3394,  3395,
    3397,  3406,  3409,  3411,  3414,  3416,  3418,  3432,  3433,  3435,
    3440,  3442,  3444,  3446,  3448,  3453,  3454,  3456,  3458,  3463,
    3465,  3473,  3474,  3475,  3480,  3481,  3486,  3488,  3490,  3492,
    3494,  3496,  3503,  3505,  3507,  3509,  3511,  3514,  3516,  3518,
    3520,  3522,  3527,  3529,  3531,  3536,  3562,  3563,  3565,  3569,
    3570,  3574,  3576,  3578,  3580,  3582,  3584,  3591,  3593,  3595,
    3597,  3599,  3601,  3606,  3608,  3610,  3617,  3619,  3637,  3639,
    3644,  3645
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

#define YYPACT_NINF (-1697)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-852)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     129, 11277,   194,   222, 15343,   115, -1697, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697,    87,   885,   167,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697,   121,   302, -1697,
   -1697, -1697, -1697, -1697, -1697,  4525,  4525,   358, 11277,   387,
     399, -1697, -1697,   443, -1697, -1697, -1697, -1697, -1697, -1697,
   -1697, -1697, -1697,  2504, -1697,   352,   217, -1697, -1697, -1697,
   -1697, 15193, -1697, -1697,   452,   498,   270,   305, -1697,  4525,
     498,   498,   498,   500,  4205,   677,   801,  9785, -1697, -1697,
   -1697, 15043,  2072, -1697, -1697, -1697,  1568,   680,  8757,   524,
     775,  1568,   856,   567, -1697, -1697, -1697, -1697,   660, -1697,
   -1697, -1697, -1697,   570, -1697, -1697, -1697, -1697, -1697,   606,
     584,   660, -1697,   660,   614, -1697, -1697, -1697, 15847,  4525,
   -1697, -1697,  4525, -1697, 11277,   624, 15899, -1697, -1697,  5070,
   16911, -1697,   694,   694, -1697,  1493, -1697, -1697, -1697, -1697,
     109, 13653,  3555,   660, -1697, -1697, -1697, -1697, -1697, -1697,
     633, -1697,   635,   665,   690, -1697,   711, 18301, 14273,  3081,
    2504,   597,   678,   695,   700,   702,   705,   713, -1697, -1697,
   16049, 10628,   735, -1697, 15486, -1697, -1697, -1697, -1697,   746,
   -1697, -1697,   770, -1697,   880,  9372, -1697,   782,  4525,   584,
     784,   785,   788,   800, -1697, -1697, -1697,  2686,  3450,   802,
     866,    56, -1697, -1697,   660,   660,   -20,   181,    85,   -20,
   -1697,   660,   660, -1697,  3577, -1697, -1697,   816,   821,   694,
   13231, -1697, -1697, 15193, -1697, -1697,  1568, -1697,   958,   567,
     838,   917,   181,  4525,   270, -1697, 12524, -1697,   694,   694,
     869,   917,   181,  4525, -1697,  9711, -1697, -1697,   694, -1697,
     694, -1697,   897,  3921,  4525, -1697,  1142,   828, -1697, -1697,
   -1697, 15645,   584,   262, -1697, -1697, 16961, -1697,   866,   344,
   -1697, 18301, 16911,  4235,  3577, -1697,   130, -1697, -1697, -1697,
   15899,  4525,   875, -1697, -1697, -1697, -1697,  4525,  3226,   453,
     592, -1697,  4525,   635, -1697,   609,   660,   906, 16101,   655,
   13811, 13389,  1568,  1568, -1697,  1568,   694,  1568,   694, -1697,
   -1697,   660, -1697,   883, -1697, 16251, -1697, -1697, -1697, 16303,
     746, -1697,   918,   466,   879,   921,   567,   954, -1697,  1493,
     940,   635,  1493,  2022, -1697,   898,  1006, 18373,   975,   992,
   18301, 18445,   999, -1697, -1697, -1697, -1697, -1697, -1697, -1697,
   18517, 18517, 14119,  1010,  4105, -1697, -1697, -1697, -1697,  1009,
   -1697,  1012, -1697,  1051, -1697, 18301, 18301, -1697,   996,   591,
     895,   984,   525,  1007,  1014,  1021,  1028,  1056,     3, -1697,
     669, -1697,  1053, -1697,  1002,  4128, 14581, -1697, -1697,   751,
    1053, -1697, -1697,   707, -1697, -1697,  3081,  1059,  1061,  1065,
    1067,  1080,  1084, -1697, -1697,   388,  1086, -1697,   740,  1086,
   -1697, -1697, 15847, -1697,  1004,  1088, 14735, -1697, -1697,  4322,
    4817,  1124, 13811,  1136,   724,   761, -1697, -1697, -1697, -1697,
   -1697,  4525,  4464, -1697, -1697, -1697, -1697, -1697, -1697, -1697,
    8471, -1697, -1697, 17725,  1097, -1697, -1697, -1697, -1697, -1697,
    2686,   617,  1126,  1128,  1137,   656,  1150,  1155,  1163,  3450,
   -1697, -1697,   660,  1111,   270,  1117, -1697, -1697,  1156, -1697,
   -1697,   584,   917, -1697, -1697, -1697,   584, -1697, -1697,  3577,
   -1697, 14581, 14581, -1697,   694,  5070, 17717, 13653, -1697, -1697,
   -1697, -1697, -1697,   584,   917,   344, -1697, -1697,  1568,  1159,
     917,   181, -1697,   584,   917, -1697, 14019, -1697,   694,   694,
   -1697, -1697,  1161,   528,  1166,   567,  1195, -1697, 17119, -1697,
     739, -1697,  1259, 17585, -1697,  5070, 16462, 13231, -1697, 15645,
   18589, -1697, -1697, -1697, -1697, -1697,  4235,   734,  3577, -1697,
   13653,   866, -1697,  1200, -1697,  1207, -1697, -1697, -1697, -1697,
   -1697,  1493, -1697, -1697,  1282,  4309, 16303, 10628, -1697, 16514,
   -1697,   694,   694, -1697, -1697,   746, -1697,   774,  1208,  1348,
   18301,   893,  1156,  1194, -1697,   660,   660, -1697,  1086, -1697,
   16101, -1697, -1697, 17400,   694,   694, -1697,  4309,   660, -1697,
   16768, -1697, -1697, 16251, -1697,   109,  1214,   306,  1219,   879,
     759, 15899,   764, -1697, -1697, -1697, -1697, -1697, -1697,   781,
   -1697,  1225,  1234, -1697, 14427, -1697, 16566, 16566, -1697, 14427,
   -1697, 18301, 14427, -1697, -1697,  9151, 16566, 16566,  1002,  1412,
    1495,   505,  1612, -1697,   797,  1237,  1017,  1255, -1697, 17725,
   18301, 17797,  1254,  1142,  1142, -1697,  3633, -1697, -1697, 17869,
    1286, 18301, 17869,  1142, -1697, -1697, 18301, 18301, 18301, 18301,
   18301, 18301, 18301, 18301, 18301, 18301, 18301, 18301, 18301, 18301,
   18301, 18301, 18301, 18301, 18301, 17941,  1242,   711,  3158, 10628,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697,
   -1697,  1273, 18301, -1697, -1697,   751,  1210, -1697, -1697,   660,
     660, -1697, -1697, 14581, -1697,   397,  1086, -1697,   805,  1086,
   -1697, -1697, -1697,  1156, -1697, -1697,  1156, 18661, -1697, -1697,
   10628,  1290,  1291,  2699,  1418,  3211,   413,  1194, -1697,   660,
     660,  1194,   424, -1697,   660,   660, 18301,  4525,  1029,  1033,
    1194,   161, 13179, 13179,  4525,  1295,  3731,  1010,  1298,  1299,
   -1697,  1301,  9372,   640, -1697, -1697, -1697,   809, -1697, 13179,
    1142,  5070,  1142,   818,  1306,  1307,  1310,   819,  1312,  1313,
    1315,   426,  1086, -1697, -1697,   447,  1086, -1697, -1697, -1697,
    5070,   711, -1697,  1086, 18661, -1697,   584, 17119, -1697, -1697,
     823,  1316,   842,  1317, -1697,  1321, -1697,   584, -1697, -1697,
     584,   917,  1321, -1697,   584,  1318,  1320,  1322, -1697, -1697,
   17400, -1697,  1319, -1697, -1697, -1697,  1142,  4525, 10122,  1406,
    1304, 17487, -1697,  1088, -1697, 13179,   847, -1697,  1321, -1697,
   15899, 14581,  1314, -1697,  1314, -1697, -1697, -1697, -1697, 16251,
   -1697, 10790, 14889, -1697, 17119,  1337,  1338,  1339, -1697,  4123,
     660, -1697,   893, -1697, -1697, -1697, -1697,  1156, -1697, -1697,
   -1697,   694, -1697,  3764, -1697, -1697,   567,  2288,  1343, -1697,
    9372, -1697,   879,  1214, -1697, -1697,  1340,  1344,  2022, 17869,
   -1697,  1345,   217,  1350,  1349,  1363,  1361,  1367, 18301,  1371,
    1374,  1375, 10628, 18301, -1697, -1697,  1718, -1697, -1697, -1697,
   18301, -1697,  1376,  1377,  8897,  1035, -1697, 17869, -1697, -1697,
   -1697,  2128, -1697, -1697,   876, -1697, -1697, -1697,  2128, -1697,
   -1697,  1038,    33, -1697, -1697,   996,   996,   996,   591,   591,
     895,   895,   984,   984,   984,   984,   525,   525,  1007,  1014,
    1021,  1028,  1056, 18301,  1040, -1697,  1378,  2128, -1697, -1697,
    8471, -1697, 17119,  1381,  1383,  1384,  1210, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697,  1156, -1697, -1697,  1156, 17119,
   17119, -1697, -1697,  2699,   771,  1386,  1393,  1394,  1397,   960,
    3211, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697,  1395, -1697,  1194, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697, -1697,  1399,  1400, -1697,   270,
   -1697, -1697, 18301, -1697,  8471,  1373, -1697,  2893, -1697, -1697,
   -1697, -1697, 18013, 13179, -1697, -1697, -1697,  1379,   449,  1086,
   -1697,   456,  1086, -1697, -1697, -1697, -1697,  1156, -1697, -1697,
   -1697,  1156,   866,  1404,  1156, -1697, -1697, -1697, -1697, -1697,
   -1697, -1697,  1414, -1697, -1697,  1321, -1697,   584, -1697, -1697,
   -1697, -1697, -1697, 11907,  1409,  1410, -1697,   -91, -1697,   462,
     114, 10466,  1415, 13008,  1417,  1421,  2630,  2929,  3142, 18085,
    1433, -1697, -1697,  1436,  1438, -1697, -1697,   584, 18301, 18301,
    1576,  1439,   522, -1697,  1520,  1452,  1437, -1697, -1697, -1697,
    9950, -1697, -1697, -1697, -1697, -1697,  2198, -1697, -1697, -1697,
    1521, -1697, -1697, -1697,  1142, -1697, -1697, 11754, 15193,  1456,
   -1697,  4525, -1697,  1441,  1459,  1463, -1697,  1047, -1697, -1697,
   -1697,  5070, -1697, -1697,  1445,  1446,   889, 15899,   635,   635,
   -1697, -1697,  1010,  1088, 14735, -1697,  1053, -1697, 10952, -1697,
     482,  1086, -1697,   694,  4430, -1697, -1697,   879,   660,   660,
     109,   306, -1697, -1697,  1214,  1473,  1484, -1697, -1697,   890,
     471, 10628,  1142, -1697,   471, 15697,   471, -1697, 18301, 18301,
   18301, -1697, -1697, -1697, -1697, 18301, 18301,  1476,  8471, -1697,
   -1697,  1479,   534, -1697,  2349, -1697, -1697,  1060, -1697,   321,
   -1697, 17869,  1066, -1697, 17725, -1697, -1697, 18301,  1462,  1073,
    1079,  1301, -1697,   495,  1086, -1697, -1697, 17119, 17119, -1697,
   -1697,  1486,   508,  1086, -1697,   511,  2219,   660,   660, -1697,
   -1697, 17119, 17119, -1697,  1485, -1697, 13653, 13653,  1489,  1490,
    1492,  1498, -1697,  2128,  1103,   296, -1697, -1697, -1697,  9372,
   -1697, 18301, -1697, -1697, -1697,  1499, 18301, -1697, -1697, -1697,
    1156, -1697, -1697, -1697,  1156, 17119, 17119,   270,   660,  1109,
    1500,  1496, -1697, -1697,  1504, 12060, 12213, 12366, 15899, 16566,
   16566,  1505, -1697,  1467,  1481,  2604,  8103, -1697,   327,  4525,
   -1697, -1697,  4525, -1697, 17869,   472,   480, -1697, -1697, -1697,
   -1697, 18301,  1510,  1584,  1515,  1516, -1697,  1494, -1697,  1497,
   18301,  1501,  8471,  1502, 18301, 17725, 18301,  1232, -1697,  1514,
     224, -1697,    90,  1524, -1697, -1697,  1531, -1697,  1519, -1697,
    1525,  1535, 13008,   664, 12805,   660,   332, -1697, -1697, -1697,
    1518, -1697,  1541, -1697,  1542, -1697,  1543, -1697,  1549, -1697,
   -1697, -1697, -1697, 11114,  1554,  1557,  1559, -1697,  1563, -1697,
   -1697, -1697,  1156, 18301, 18301,  1088,  1562, -1697,  1214, -1697,
    1564,   553, -1697,  1572, -1697, -1697, 15899, -1697,  1571,  1570,
     892, -1697,  1581, -1697, -1697, -1697, -1697, -1697,  8471,  1301,
   17725, -1697,  1609,  2128, -1697,  1609,  1609, -1697,  2128,  2363,
    3304, -1697, -1697,  1115, -1697, -1697, -1697,  1593,  1591, -1697,
   -1697, -1697,  1156, -1697, -1697,  1592,  1594,   660, -1697, -1697,
   -1697,  1156, -1697, -1697, -1697,  1598, -1697, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697,  1595,
   18301, 18301,  1120,  1599, -1697, -1697,  1601,   660, -1697, 17119,
   17119, -1697, -1697, -1697, -1697, 18301, -1697, -1697,  1608, -1697,
    1505,  1505,  1505,   789,  1588,   390, -1697,  4002,   394, 14581,
   -1697, -1697, -1697,  4628, 18301,  3781,   433, -1697, -1697,     5,
    1602,  1602,  4525, -1697, -1697, 17268, -1697,   900, -1697, -1697,
   -1697, -1697,   901,  1614, 13008, 10466, 13008, 10294, -1697, -1697,
     465, -1697,  1301, -1697,   925,   926,   933, -1697, -1697, -1697,
   -1697,   584,  1232,  1615, -1697, -1697, 18301, -1697,  1616,   711,
   10466, -1697, -1697, -1697, -1697, 18301,  1659, -1697, 13008, -1697,
     660, 13653, -1697, -1697, 15899, -1697, -1697, -1697, -1697, -1697,
    1617, -1697, 17119, -1697, -1697,  1621, -1697,  1623,  1620,  1613,
     879, -1697, -1697, -1697, -1697, 18301, -1697, 15697, 18301,  1301,
    1630,  1127, -1697,  1129, -1697,  2128, -1697,  2128, -1697, -1697,
   -1697, -1697, 17119,  1618,  1628, -1697, -1697, 17119, 17119,  1631,
    1632,  1143, 13337, 13495,  1627, -1697, -1697, -1697, -1697,  1633,
   -1697, -1697, -1697, -1697,  1634,  1635,  1158, -1697, -1697, -1697,
   -1697,   789,  1370,   467, -1697, -1697, -1697, -1697,   660,   660,
   -1697, -1697, -1697,   576, -1697,   934,  4628,   681, -1697,  3781,
     660, -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697, 13008,
     171, 18157, -1697,  1452,  1637, 18301,   452,  1636,   500,  9542,
   15899, -1697, 18301, 18301,   548,   245, -1697, 18301, -1697,  1641,
     334, 13008, -1697, -1697,  1645, -1697, -1697,  1622,   711,   369,
    1646,  1648,  1170,  1712, -1697, -1697, -1697,  4525,  5070, -1697,
   -1697,  1647,  1649, -1697, -1697, -1697,   879,  1214,  1654, -1697,
   -1697, -1697,  1656, -1697, -1697, -1697,  1175,  1178, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697, -1697, -1697, -1697,  1655, -1697,
   -1697,  1653,  1657,  1658, -1697, -1697, -1697,  1661,  1663,  1666,
    1370, -1697,   660, -1697, -1697, -1697, -1697, -1697,  1665,  4002,
   -1697, 18301,  1664, -1697, -1697, 12634, -1697,  1660,   935, 13008,
    1452, 13969,  1452,  1667, -1697, -1697, -1697, -1697,  6111, 18301,
   13008, 10294,  1670,  1672, -1697, -1697, -1697, -1697, 16716, -1697,
    1674,  1675,    50, 13008, -1697, 18301, 17869,   510, -1697, -1697,
   -1697,  1671, -1697, -1697,  1214,  1673, -1697, -1697, -1697, -1697,
    1684,  1687,  1688, 13653,  1695, -1697, -1697, -1697,   517,  1086,
   -1697, -1697,   789, -1697,   323, -1697,  1179, -1697, -1697, 11436,
   -1697, -1697, -1697,  1679, -1697, 18301,  1702, 18301,   668,  1681,
      32, -1697, -1697, 18301, -1697, 11436, 16716, -1697,  4791, 16514,
    1142,  1700, -1697,  1756,  1710,   474,  1706, -1697,  1790, -1697,
     943, 13008,  1713, 13008, 13008, -1697,  1716, -1697, -1697, -1697,
   -1697, -1697, -1697, -1697, -1697,  1156, -1697, 18301, 18301, -1697,
    1285, 11595, -1697, -1697, -1697, -1697,  1452,  1717,  1722, 18301,
   18301, 18301, -1697, -1697,  1285, -1697,  1692,  2868,  3366, -1697,
   -1697, -1697,    50,  1714, 18301,  1701,    50,    50, 13008, -1697,
   -1697, 18301,  1767,  1771, -1697, 17119, -1697, -1697, 12634, -1697,
    1285, -1697,  1715,  1720,   347, -1697,  1452, -1697,  1692, 18301,
    1730,  3366,  1726,   711,  1732, -1697,   565, -1697, -1697,   948,
    1712,   411, -1697, -1697, 12890,  1740, 12634, 18301, 18229, 18301,
    1741,  1742, -1697,   584,   711,  1743, -1697,  1724,   711, -1697,
   -1697, 13008,  1818,  1750, -1697, -1697, 12890,  1452, -1697,  1452,
    1452, -1697,   584, -1697, -1697,  1217, 18301, -1697,   949, -1697,
   13008, -1697, -1697,   711,  1142,  1753,  1731, -1697, -1697, -1697,
     950, -1697, -1697,  1733,  1142, -1697, -1697
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
       0,   401,     0,   736,   737,   526,   455,   621,   622,   620,
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
       0,   647,     0,   656,   667,     0,   735,     2,   401,   970,
     402,   401,   413,   392,   459,   393,   484,   394,   491,   488,
     509,   735,   510,     0,   609,   401,   610,   924,   925,   401,
     611,   613,   498,   504,     0,   575,   576,     0,   738,     0,
     678,   666,     0,   742,    20,     0,    19,     0,     0,     0,
       0,     0,     0,    22,    24,     4,     8,     5,     6,     7,
       0,     0,   401,     2,     0,    92,    93,    94,    95,    76,
      23,    77,    35,    75,    96,     0,     0,   111,   113,   117,
     120,   123,   128,   131,   133,   135,   137,   139,   141,   144,
       0,    25,     0,   505,     2,    96,   401,   145,   672,   624,
     495,   626,   671,     0,   623,   627,     0,     0,     0,     0,
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
     412,     0,   651,   668,   457,     0,   401,   401,   926,   402,
     398,   399,   400,   930,   921,   922,   928,     2,     2,    90,
       0,   886,   900,   970,   882,   735,   735,   891,   898,   616,
     401,   489,   612,   402,   485,   486,   490,     0,   735,   936,
     402,   941,   933,   401,   938,     0,   968,   581,     0,     0,
       0,   401,     0,   750,   749,   745,   747,   748,   746,     0,
     740,   743,     0,    21,   401,    83,   401,   401,    78,   401,
      85,     0,   401,    81,    82,   401,   401,   401,     2,    92,
      93,     0,     0,   171,     0,     0,   525,     0,   946,     0,
       0,     0,     0,     0,     0,    45,     0,    51,    52,    56,
       0,     0,    56,     0,    79,    80,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   401,
     154,   155,   156,   157,   158,   159,   160,   161,   162,   163,
     164,   152,     0,   150,   151,     2,   862,   625,   859,   735,
     735,   867,   506,   401,   779,   735,   789,   797,   801,   807,
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
     401,   401,   649,   648,   649,   458,   456,   575,   932,   401,
     937,   402,   401,   923,   401,     0,     0,     0,   901,     0,
     735,   971,   887,   888,   617,   884,   885,   899,   927,   931,
     929,   487,   522,     0,   935,   940,   578,   969,     0,   152,
       0,   577,     0,   968,   681,   679,     0,     0,   742,    56,
     705,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   401,     0,   110,   109,     0,   106,   105,    26,
       0,    27,     0,     0,     0,     0,     3,    56,    41,    42,
      49,     0,    48,    59,     0,    57,    60,    44,     0,    43,
      47,     0,     0,    40,   112,   114,   115,   116,   118,   119,
     121,   122,   126,   127,   124,   125,   129,   130,   132,   134,
     136,   138,   140,     0,     0,   352,     0,     0,    28,     3,
     633,   146,   401,     0,     0,     0,   863,   864,   860,   861,
     674,   673,     2,   783,   785,   787,     2,   803,   805,   401,
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
     934,   939,     2,    89,   401,     3,   496,     3,   402,     3,
     735,   894,   897,   401,     3,   883,   889,     0,   735,   735,
       0,   581,   566,   582,   968,     0,     2,   739,   741,     0,
      84,   401,     0,    88,    86,   401,     0,   100,     0,     0,
       0,   104,   108,   107,   172,     0,     0,     0,   633,    97,
     165,     0,     0,    73,     0,    73,    73,     0,    61,    63,
      39,     0,     0,    37,     0,    38,   143,     0,     0,     0,
       0,   968,     3,   735,   870,   873,   865,   401,   401,     3,
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
     650,   654,   652,   401,     0,     0,     0,     3,     0,     2,
     890,   892,   893,     0,     0,    89,     0,     3,   968,   571,
       0,   581,   579,     0,   569,   682,   401,   744,     0,     0,
       0,    31,     0,   101,   103,   102,    99,    98,   633,   968,
       0,    55,    70,     0,    64,    71,    72,    50,     0,     0,
       0,    58,    46,     0,   142,   351,    29,     0,     0,     2,
     866,   868,   869,     3,     3,     0,     0,   735,     2,   837,
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
     735,   401,   707,   670,   401,     2,     2,   942,   943,   944,
       0,   895,   401,     3,     3,     0,   903,     0,     0,     0,
       0,   580,   568,     3,    87,     0,    30,   401,     0,   968,
       0,     0,    74,     0,    62,     0,    68,     0,    66,    36,
     147,   871,   401,     0,     0,   776,   794,   401,   401,     0,
       0,     0,   401,   401,     0,     3,   732,   643,   644,     0,
     368,   370,     3,     3,     0,     0,     0,   713,   516,   518,
     514,     0,   910,     0,   556,   915,   558,   907,   735,   735,
     542,   562,   546,     0,   545,     0,     0,     0,   565,     0,
     735,   539,   553,   564,   554,   560,   599,   603,   602,     2,
       0,     0,   227,   208,     0,     0,   210,   355,   209,   481,
     401,   231,     0,   173,   237,     0,   232,   173,   257,     0,
       0,     2,   280,   307,     0,   298,     2,     0,     0,     0,
       0,   285,     0,   281,   188,   369,   685,     0,     0,   945,
       3,     0,     0,   902,   904,   570,     0,   968,     2,    34,
      32,    33,     0,    53,   166,    65,     0,     0,     3,   777,
     795,     3,     3,   842,   857,   372,     2,   589,     3,   588,
     646,     0,     0,     0,   768,   826,   876,     0,     0,     0,
     911,   912,   735,   541,   908,   909,   540,   521,     0,     0,
     279,     0,     0,     2,   219,     2,   202,     0,     0,     2,
     211,   481,   238,     0,   253,   254,   255,   252,   241,     0,
       2,   401,     0,     0,     2,   204,   278,     2,   401,   275,
       0,     0,   323,     2,   283,     0,    56,     0,   295,   690,
     692,     0,   905,   906,   968,     0,   683,    54,    69,    67,
       0,     0,     0,   401,     0,   645,   769,   827,   735,   918,
     920,   913,     0,   551,   212,   215,     0,   214,   218,   401,
     221,   220,   229,     0,     3,   173,   246,     0,   242,     0,
     239,     3,   233,   173,   266,   401,   401,     3,   308,   402,
     312,     0,   316,     0,     0,     0,   324,   325,   206,   286,
       0,     2,     0,     2,     2,   896,     0,   573,   872,   843,
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
   -1697,  6158,  6055, -1697,    -1,   623,  -158, -1697,  1532, -1697,
     317, -1697,  -602,   688,  -852,  -983, -1697,   163,  3967,  1907,
   -1697,  1182, -1697,  1262,   198,   710,   732,   445,   749,  1224,
    1226,  1223,  1227,  1228, -1697,  -111,  -161,  8227,   810, -1697,
    -363, -1697, -1697,  -648,  2431, -1070,   929, -1697,   188, -1697,
     798,    -5, -1697, -1697, -1697,   384,    71, -1697, -1563, -1266,
     260,    58, -1697, -1697, -1697,   170,   116, -1697, -1697, -1697,
   -1697,    15, -1643,   160, -1697, -1697,    18, -1697, -1697, -1697,
      31,   409,   410,   117, -1697, -1697, -1697, -1697,  -875, -1697,
      60,     6, -1697,   123, -1697,  -153, -1697, -1697, -1697,   808,
    -788, -1032, -1330, -1697,    17,     0,   311,  4910,  -907,  -832,
   -1697,  -268, -1697,    27,  -129,   185,   -90,  -222,  3862,  7242,
    -592, -1697,   141,   356,   231,   831, -1697,  1903, -1697,    23,
    4526, -1697, -1697, -1697,    91, -1697, -1697,  1980,   165,  4726,
    3213,   -12,  1705,  -290, -1697, -1697, -1697, -1697, -1697,  -336,
    1789,  5305, -1697,  -353,    72, -1697,   469,   215, -1697,   155,
     658, -1697,   459,   -80, -1697, -1697, -1697,  5555,  -614, -1012,
    -685,  -401,  -396,  1537, -1697, -1208,  -151,  -130,  1281,   830,
    2806,  -213,  -425,  -228,  -190,  -932,   928, -1697,  1196,   237,
    1118,  1405, -1697, -1697, -1697, -1697,   284,  -149,    94,  -831,
   -1697,    67, -1697, -1697,   579,   427, -1697, -1697, -1697,  1978,
    -701,  -483,  -942,    46, -1697, -1697, -1697, -1697, -1697,   -11,
    -765,  -145, -1696,  -164,  6811,   -61,  6550, -1697,  1085, -1697,
    2139,  -193,  -203,  -186,  -183,     1,   -67,   -66,   -64,    26,
     -40,    57,    73,  -176,   -86,  -174,  -171,  -165,  -698,  -637,
    -612,  -609,  -692,  -126,  -575, -1697, -1697,  -654,  1267,  1269,
    1270,  1108,  7488,  -486,  -522,  -519,  -518,  -655, -1697, -1331,
   -1550, -1518, -1513,  -563,   -49,  -200, -1697, -1697,   -59,   200,
     -27, -1697,  8115,   102,  -736,  -549
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1114,   211,   379,   380,   169,   381,   356,   382,  1400,
    1401,   383,   934,   935,  1217,  1218,  1219,  1412,   405,   385,
     386,   387,   651,   652,   388,   389,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   407,  1265,   653,  1339,   712,
     205,   714,   401,   777,  1115,  1116,  1117,  1118,  1119,  1120,
    1121,  1924,  1122,  1123,  1344,  1654,  1805,  1806,  1744,  1745,
    1746,  1899,  1900,  1124,  1665,  1666,  1759,  1125,  1126,  1127,
    1128,  1129,  1130,  1352,  1682,  1844,  1778,  1131,  1132,  1532,
    1910,  1533,  1534,  1827,  1133,  1134,  1135,  1342,  1835,  1836,
    1837,  1955,  1970,  1860,  1861,   282,   283,   838,   839,  1087,
      82,    83,    84,    85,    86,  1657,   438,    89,    90,    91,
      92,    93,   219,   545,   440,   409,   441,    96,   292,    98,
      99,   100,   321,   322,   103,   104,   165,   105,   856,   323,
     151,   108,   239,   109,   152,   248,   325,   326,   327,   153,
     402,   114,   115,   329,   116,   536,   827,   825,   826,  1490,
     330,   331,   119,   120,  1083,  1307,  1496,  1497,  1623,  1624,
    1308,  1485,  1642,  1498,   121,   618,  1569,   332,   616,   891,
    1025,   446,   447,   831,   832,   448,   449,   833,   334,   540,
    1139,   411,   412,   206,   770,   771,   772,   773,   774,   310,
    1158,   311,   854,   852,   569,   312,   350,   313,   314,   413,
     123,   171,   172,   124,  1152,  1153,  1154,  1155,     2,  1072,
    1073,   815,  1291,   125,   302,   250,   260,   519,   126,   209,
     127,   220,  1267,   818,   486,   163,   128,   629,   630,   631,
     129,   222,   223,   224,   225,   297,   131,   132,   133,   196,
     135,   136,   137,   228,   298,   230,   231,   232,   747,   748,
     749,   750,   751,   233,   753,   754,   755,   717,   718,   719,
     720,   487,   138,   593,   594,   595,   596,   597,   598,  1626,
    1627,  1628,  1629,   583,   451,   337,   338,   339,   414,   198,
     140,   141,   142,   341,   888,   599
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,    87,   130,    79,   295,   893,   182,   183,   229,   184,
     333,   925,   180,   544,   472,   462,   400,   503,   355,   656,
    1156,   147,   197,   351,   106,   480,   516,   134,    94,  1333,
     601,   473,   319,   185,   474,  1035,   347,   879,  1231,   822,
    1777,   475,   713,   476,   874,  1807,   477,  1001,  1452,  1453,
    1136,  1302,   478,  1002,    79,    79,   484,    79,    87,   130,
     918,   450,   977,   189,  1312,   865,   399,   207,   866,   867,
     942,  1727,    79,  1140,   492,    57,   811,   813,  1028,   848,
      79,   106,   195,  1313,   134,    94,  1222,   197,    79,   500,
     472,  1005,   110,    79,  1044,   226,    79,  1012,   251,   514,
      79,   480,   261,  1728,   175,   873,   995,   473,  1729,   524,
     474,   287,   336,   417,   418,  1229,   419,   475,   254,   476,
     227,  1078,   477,   252,   551,   553,    57,   262,   478,  -693,
     186,   996,   481,   694,   997,   611,  1294,  -717,    79,   614,
     420,    79,   101,    79,    87,   130,   187,  1148,    79,   110,
     182,   183,   601,   184,    79,    57,   470,  1185,   485,   657,
    1146,    79,  1807,   433,   847,   354,   111,   106,   998,  1392,
     134,    94,  1538,  1315,  1316,   695,  1303,   185,    79,    79,
     195,  1225,  1811,   200,   509,  1656,    95,   920,  1221,   149,
     290,    57,   258,    79,   143,   454,   895,  1871,  1006,   101,
      57,   139,  1009,  1833,   139,   276,  1362,    79,   481,   485,
    1656,  1022,  1023,    62,    63,   491,    79,    79,   496,   195,
     455,   865,  -694,   111,   866,   867,   182,   183,   578,   184,
     154,  1414,   556,    79,   493,   110,   155,   421,   485,  1539,
     513,   509,    79,    95,   195,   609,  1741,  1742,   200,   612,
     523,  1304,    79,   422,   890,    79,   802,   520,   139,   873,
     531,    75,    79,  1302,  1302,  1302,   193,   784,  -338,   584,
     161,   277,    79,    79,   186,    79,  1409,  1777,  1184,   559,
     415,   842,   521,   485,   785,   101,   798,   786,  1317,   601,
     187,  1731,    79,    79,   787,   195,   788,  1189,   981,   789,
      79,   139,  1249,  1319,   106,   790,    79,    79,  1250,   111,
     552,    79,    88,   601,   752,   148,   160,   940,   278,  1536,
     601,    19,  1236,   285,  1203,  1212,  1320,  1727,  1743,    95,
     525,   193,  1136,   264,   590,  1811,  1075,   265,  -338,  1005,
     268,   537,   270,    79,   139,   510,   578,   197,    79,  -339,
    1478,    79,   628,   784,   809,  1140,   995,   102,  1274,  1728,
     814,  1026,  1026,  1811,  1729,   798,  1027,  1027,   821,    88,
     785,   190,   110,   786,  1292,   431,   860,   450,  1026,  1571,
     787,   996,   788,  1027,   997,   789,  1176,   584,  1303,  1303,
    1303,   790,   207,   799,  1452,  1453,  1760,   465,  1537,  1801,
    1522,  1761,   510,   417,   418,   739,   419,   573,   244,  1741,
    1742,  1459,   255,   885,   102,    79,   203,   454,  1241,  -339,
     586,   901,   547,   903,   904,   505,   905,  1460,   508,   907,
     420,   319,   909,   910,   911,  1584,  1586,  1588,    79,    79,
     483,  1419,   455,   204,  1026,   573,   111,   450,  1393,  1027,
      79,    79,   203,   246,  1857,    88,   188,    63,    57,   768,
     208,  1461,  -717,  1304,  1304,  1304,    95,    57,  1255,    79,
     548,   865,   799,  1420,   866,   867,  1579,   264,    79,   889,
     454,   806,  1502,    57,  1941,   508,   858,  1312,  1461,   417,
     418,  1764,   419,   552,    57,  1427,    57,  1880,    79,  1104,
     102,  1503,   920,   817,    79,   455,  1549,   174,   200,   820,
     878,   336,  1929,   824,  1176,  1381,   847,    57,   156,    57,
    1771,   157,   158,   884,   159,  1772,    57,   421,   241,     6,
       7,     8,     9,    10,    11,    12,   176,   730,   586,   966,
     601,   485,    79,   422,    79,  1631,   982,  1731,   177,  1502,
     485,   584,    57,   264,   265,    79,   605,    79,   270,   454,
     655,  1583,  1003,  1388,  1632,    57,   588,   517,  1634,   450,
      79,   601,   106,  1010,    79,  1054,  1423,   588,    57,   485,
    1801,    57,  1036,  1841,   566,  1851,   620,    57,  1640,   622,
    1444,   193,   178,  1753,  1898,  1016,  1058,  1762,  1277,  1351,
     485,   879,   485,   415,   415,  1281,    79,  1641,  1898,   485,
     450,  1314,   518,   567,   568,   178,  1842,   738,    79,   752,
     920,  1207,  1732,  -851,    80,  1885,   201,   145,  1208,   277,
    1886,  1379,   450,   450,  1926,   588,  1046,   277,   258,  1668,
     110,  1733,  1026,  1063,  1429,   399,  1508,  1027,   485,   450,
     684,   685,  1568,    -3,  1510,  1062,   913,  1438,  1635,  1161,
    1442,   485,    79,    79,   588,    79,  1852,   914,   915,    79,
     485,   178,    79,  1580,   214,  -386,   246,   178,  -622,   805,
     234,    80,  1411,  -395,   808,  -517,   531,  1525,  1819,  1221,
     547,  1754,  1755,  1756,   686,   687,    80,    79,  -386,   203,
    1183,   816,   880,   920,    80,   272,    13,    14,    15,    16,
      17,   823,  1507,  1757,   111,   450,  1937,    80,   862,   272,
      80,  1938,  1758,   415,    80,   274,   889,  1655,   149,  1667,
      57,  1640,  1370,  1706,    95,  1707,    13,    14,    15,    16,
      17,   277,    79,   570,    79,  1865,   276,   571,   423,   264,
    1736,   677,  1655,  1873,   189,   276,    79,  1179,   678,   679,
     574,   272,  1581,    79,    57,    79,   276,    80,   423,  1160,
     485,  -338,    80,    13,    14,    15,    16,    17,  -396,    79,
      79,    79,   349,   319,    72,   345,  1669,    13,    14,    15,
      16,    17,  1037,  1038,    57,   847,  1039,   291,   246,    79,
    1905,  1262,    80,    80,   587,   493,   308,   794,   588,   485,
    1077,  1754,  1867,  1756,   352,    77,   589,    80,   890,   836,
     696,   277,   655,   920,   697,   415,   354,   655,   590,   424,
     655,    57,   423,  1868,   485,    79,    79,   768,    87,   353,
      80,    80,  -174,  1702,  1268,    57,   425,   235,   236,   655,
     237,   426,  1226,   427,   238,  -399,   428,    80,   722,  -397,
    1451,   106,   723,   336,   429,    94,    80,  1149,    13,    14,
      15,    16,    17,  1248,   752,   945,   946,   947,   601,    80,
      72,   862,    79,   559,   453,   423,    79,   485,   615,   734,
     835,    79,  -400,   485,   836,   457,   534,   628,  1287,   539,
     715,   459,   246,    72,   485,    13,    14,    15,    16,    17,
     894,    77,    78,   450,   571,   896,    80,    80,    72,   571,
    1003,  1264,   423,   587,   588,   458,    57,   588,   247,   110,
      79,   463,   897,   466,    77,    78,   898,    79,  1621,   267,
     467,   156,   485,   468,   157,   158,  1484,   159,   919,    77,
      78,  1383,   920,   415,   986,   469,  1697,   482,   485,   518,
    1041,  1785,   483,    57,  1042,   501,    79,   276,   493,   768,
     502,   485,   485,  1667,  1068,    18,   541,   847,   920,  1138,
      61,   247,  1402,   167,   168,    64,    65,    66,    67,    68,
      69,    70,    79,  1070,  1066,   512,   559,   920,    79,    79,
     485,   530,    63,   111,   207,  1074,  1363,  1912,  1076,   351,
     351,  1916,  1079,    47,    48,    49,    50,    51,    52,    53,
      54,  1862,    72,    95,  1309,   247,   522,  1220,   890,    74,
      57,  1221,   562,   768,   608,  1151,    79,  1862,   139,    80,
    1369,  1397,   587,  1576,   723,  1221,   588,  1577,  1846,   632,
     139,  1649,  1650,    77,   589,  1221,   920,   680,   681,  1473,
     576,    61,   319,    80,   215,   216,    64,    65,    66,    67,
      68,    69,    70,  1901,   878,  -850,  1670,  1671,  -567,  1464,
     920,  1042,  1784,   769,  1672,  1737,  1813,   247,   920,   723,
     920,    87,   768,    80,  1889,    79,    79,    79,  1221,  1939,
    1966,  1973,    80,   920,  1963,  1974,  1454,   682,   683,  1246,
      74,   619,  1500,   621,   106,   584,  1424,   247,    94,   768,
      87,   633,    80,   247,   636,    79,   450,   450,    80,   952,
     953,   954,   955,    79,   688,   689,    79,    79,   251,   261,
      79,   637,   336,   106,  1618,  1619,  1620,    94,   641,    88,
      79,   247,  1295,  1296,  1297,   254,   190,   659,   576,   659,
    1264,  1150,   676,   252,   262,   659,   145,   663,    80,   665,
     664,   666,   667,   668,  1840,   690,    79,   922,   923,    80,
     691,    80,   110,  1018,  1019,   693,   246,  1020,  1021,  1210,
    1042,    79,  1223,  1224,   102,   920,  1227,   518,   857,   692,
     669,  1020,  1361,   670,   671,   698,  1718,   768,   672,   673,
     724,   110,   725,    79,  1417,  1418,   726,   246,   727,  1501,
    1422,  1418,    13,    14,    15,    16,    17,  1426,  1418,   258,
     882,   728,  1138,   992,  1410,   729,  1309,  1309,  1309,   430,
    1486,  1309,    -3,    61,   319,    79,   778,  1658,    64,    65,
      66,    67,    68,    69,    70,   756,   111,  -145,  -145,   415,
     801,  1138,    79,   992,  1475,  1293,   880,  -398,  1305,  1589,
    1042,   803,  1658,   247,  1607,  1410,    95,   791,  1318,   792,
      57,  1704,  1042,  1705,  1418,   111,   928,   929,   793,   932,
     472,   139,    74,   939,  1500,  1337,   943,  1715,  1716,  1605,
    1606,   795,   480,    18,    79,    95,   796,   473,    79,   284,
     474,    79,  1726,   920,   797,   147,   819,   475,  -515,   476,
     139,   968,   477,  -513,   336,  1775,  1776,   837,   478,  1788,
    1418,   768,  1789,  1418,  1858,  1859,   495,   247,   139,    72,
    1527,  1528,  1529,  1530,  1531,    51,    52,    53,    54,   399,
     399,   768,   828,    79,   849,   520,   851,   247,   855,   715,
    1741,  1742,   868,   485,  1402,   870,    80,   590,    80,   887,
      77,    78,  1963,  1964,   899,  1647,   892,   247,  1415,  1416,
     521,  1679,    13,    14,    15,    16,    17,    61,   921,  1031,
     948,   949,    64,    65,    66,    67,    68,    69,    70,   937,
    1454,  1501,    88,  1045,    80,  1047,   924,   768,   900,   481,
     247,   927,    79,  1149,   950,   951,   965,    79,    79,    79,
    1643,  1643,   319,    80,  -110,  -110,  -110,  -110,  -110,  -110,
     970,    88,  1636,   784,   247,   999,    74,   956,   957,   938,
      57,   247,  1371,  1372,   991,   992,   798,   102,   244,   255,
     785,   -16,  1454,   786,   -17,  1033,  1034,  1048,  1049,  1086,
     787,  1050,   788,  1051,  1052,   789,  1053,  1069,  1071,  -697,
    -598,   790,   450,   450,  1141,  1080,   102,  1081,  1142,  1082,
    1305,  1305,  1305,   149,  1483,  1487,    79,  1157,  1170,  1171,
    1172,  1182,    79,   246,    79,  1187,  1190,  1186,   139,    72,
    1193,    79,   336,  1509,  1511,  1192,  1178,  -109,  -109,  -109,
    -109,  -109,  -109,   768,  1194,   768,  1195,   721,  1196,  1621,
    1770,  1270,  1198,   485,   518,  1199,  1200,  1205,  1206,  1228,
      77,    78,  1233,   732,  1234,  1235,   735,  1242,   106,   601,
     106,  1547,   638,  1780,  1243,  1244,  1500,   768,  1245,  1253,
    -586,  -585,  1276,   799,  1216,  1288,   303,   304,   305,   306,
    1310,  1216,  -698,   106,  1321,  1311,  1324,   674,   675,    79,
    1325,  1149,   241,     6,     7,     8,     9,    10,    11,    12,
    1804,  1151,  1334,   495,    79,  1335,    79,  1336,   674,  1341,
    1216,   415,  1343,   769,    61,  -621,   139,   167,   168,    64,
      65,    66,    67,    68,    69,    70,   110,   920,   110,   148,
    1351,  1345,  1355,  1358,  1834,  1357,    80,  1359,   674,  1365,
    1367,  1394,    80,    80,    13,    14,    15,    16,    17,   917,
     399,   110,  1395,  1408,  1410,    79,  1425,  1437,    79,  1450,
    1455,  1491,  1454,    74,  1477,  1456,   307,  1457,   768,  1458,
    1467,  1476,  1479,  1501,  1489,  1492,  1660,   769,  1660,  1314,
    1031,   247,  1513,   450,   308,   517,  1514,  1516,  1518,  1550,
     768,  1519,   247,  1540,   472,  1521,  1523,   254,  1828,  1542,
     111,  1660,   111,  1879,  1545,   480,    79,    79,  1535,  1552,
    1553,   473,   247,  1543,   474,    79,  1896,  1804,  1555,  1544,
      95,   475,    95,   476,  1556,   111,   477,  1150,  1557,  1830,
     518,  1558,   478,  1559,  1561,   139,  1566,   139,   798,  1673,
    1572,  1570,  1574,  1914,  1834,    95,  1575,  1582,  1834,  1834,
      13,    14,    15,    16,    17,  1202,  1828,  1578,    79,  1151,
     139,  1590,  1591,  1595,   768,  1596,   399,   399,   768,   423,
    1418,   258,  1610,  1608,   139,  1935,  1617,   539,  1494,   768,
      80,    80,  1630,  1651,  1676,  1678,  1683,  1830,  1695,  1709,
    1696,  1689,   768,   399,    80,  1693,  1954,  1694,  1703,  1710,
    1954,  1720,  1713,  1714,   106,  1724,  1725,  1607,  1749,  1763,
     117,   208,   481,   117,  1767,  1965,  1769,  1773,  1932,  1774,
    1104,  1782,  1786,  1783,  1787,  1968,  -587,  1795,  1796,  1797,
     182,   183,  1798,   184,  1799,  1398,   556,  1800,   485,   834,
    1808,  1847,  1845,   721,   721,   799,    88,    79,    88,    79,
    1831,   769,   106,   984,  1812,  1848,   987,  1216,  1849,  1850,
     768,  1815,   768,   768,  1823,   399,  1824,   117,   106,  1832,
    1716,    88,   110,  1863,  -498,  1870,  1882,  1883,   944,  1884,
     139,  1887,  1891,  1888,  1894,  1150,  1909,  1902,  1913,    80,
     117,   102,  1903,   102,  1920,  1915,    79,    79,  1921,   195,
    1927,  1933,  1934,  1936,   106,  1928,  1216,   768,  1945,  1951,
     117,  1960,  1956,  1952,  1700,   495,   102,   768,  1957,  1056,
     110,  1961,  1660,  1060,  1971,  1972,   662,  1975,    81,  1421,
      79,   146,   454,   916,   958,   960,   110,   959,  1347,  1340,
     961,  1946,   962,   768,  1680,   768,   111,   117,    80,  1897,
    1765,  1822,  1907,   117,  1869,   117,  1942,  1843,  1940,  1931,
     768,  1674,  1675,  1875,  1958,   768,    95,  1917,  1874,  1356,
    1660,   166,   110,   511,  1803,   769,  1633,  1856,  1488,   768,
    1644,   139,  1269,    79,  1353,    81,  1660,   117,   247,  1040,
     244,   255,  1159,    79,   111,  1573,   853,    80,     3,   117,
     179,  1686,   973,  1188,   974,   975,     0,     0,    81,     0,
     111,     0,     0,     0,    95,     0,     0,     0,     0,   247,
       0,   218,  1660,     0,   243,     0,     0,     0,    81,   139,
      95,     0,     0,     0,     0,   246,     0,     0,     0,     0,
       0,     0,  1323,     0,     0,   139,   111,     0,     0,   117,
       0,   769,   117,     0,   623,     0,  1216,   117,     0,     0,
       0,  1216,  1216,  1216,     0,   146,    95,     0,     0,     0,
       0,    81,     0,   146,     0,     0,   294,   300,     0,     0,
       0,   139,   517,     0,     0,     0,     0,     0,   318,     0,
     117,     0,    88,     0,     0,     0,   241,     6,     7,     8,
       9,    10,    11,    12,   721,   406,   179,   179,   834,   117,
    1197,     0,     0,     0,     0,  1201,     0,   146,   436,     0,
       0,   243,     0,     0,     0,     0,  1209,   518,   624,     0,
      80,     0,     0,     0,     0,     0,    80,   102,    80,     0,
      88,  1953,     0,   625,   218,   218,   626,   627,    64,    65,
      66,    67,    68,    69,    70,   263,    88,   247,     0,     0,
    1962,   294,     0,     0,     0,   834,     0,     0,     0,     0,
      81,     0,     0,     0,     0,     0,  1279,     0,     0,  1283,
       0,   117,     0,   243,     0,   102,     0,     0,     0,     0,
       0,   444,    88,     0,     0,     0,     0,     0,     0,     0,
       0,   102,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,   300,     0,   117,     0,     0,     0,   300,
     294,   294,     0,     0,     0,     0,     0,   146,  1216,     0,
    1216,     0,   181,     0,     0,     0,     0,   102,     0,     0,
       0,   117,     0,     0,     0,   318,   591,   600,     0,    61,
       0,     0,     0,   221,    64,    65,    66,    67,    68,    69,
      70,  1213,   318,     0,     0,  1214,   318,  1215,     0,     0,
       0,     0,     0,   834,     0,     0,     0,     0,     0,    80,
       0,     0,    80,     0,     0,     0,     0,     0,     0,     0,
     834,   834,     0,   638,  1348,     0,     0,     0,    74,   406,
       0,  1546,     0,     0,     0,     0,     0,     0,   296,     0,
     117,   117,     0,     0,     0,     0,     0,  1180,   581,    61,
       0,   604,   167,   168,    64,    65,    66,    67,    68,    69,
      70,    80,     0,   406,     0,   581,   716,     0,     0,   581,
      61,     0,     0,   179,     0,    64,    65,    66,    67,    68,
      69,    70,   117,     0,     0,     0,   117,     0,   117,   146,
       0,  1431,     0,   436,  1067,     0,   674,   745,    74,   600,
    1440,     0,     0,     0,     0,     0,   471,   221,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,  1246,    74,
       0,     0,  1349,   296,     0,     0,     0,   218,     0,     0,
    1403,  1404,  1405,     0,     0,     0,   218,  1406,  1407,    61,
       0,     0,   167,   168,    64,    65,    66,    67,    68,    69,
      70,  1169,     0,     0,     0,     0,   294,     0,   406,   406,
     117,     0,   294,     0,   318,     0,   581,     0,     0,     0,
       0,     0,     0,   117,     0,   117,   117,     0,   117,     0,
       0,   117,   557,   296,   117,   117,   117,     0,    74,     0,
       0,     0,     0,  1652,     0,  1661,     0,     0,     0,     0,
      61,    80,   294,  1881,     0,    64,    65,    66,    67,    68,
      69,    70,  1213,   294,    61,   294,  1214,   318,  1215,    64,
      65,    66,    67,    68,    69,    70,  1213,  1684,     0,     0,
    1214,     0,  1215,   318,   436,     0,   600,   444,     0,     0,
     247,     0,     0,     0,   591,     0,     0,     0,   591,    74,
      80,    80,  1413,     0,     0,     0,     0,   318,     0,  1232,
       0,     0,   117,    74,     0,     0,  1585,   600,   834,   834,
     318,     0,     0,     0,     0,     0,  1239,  1240,   146,     0,
       0,     0,   834,   834,    80,     0,     0,     0,     0,     0,
     444,   406,     0,   146,   146,     0,   406,     0,     0,   406,
       0,     0,   146,   146,   146,     0,   581,   444,     0,     0,
       0,     0,     0,     0,     0,     0,   834,   834,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,  1740,   746,
     581,     0,   247,     0,     0,     0,     0,  1969,     0,     0,
       0,  1625,     0,   581,     0,     0,     0,  1976,     0,     0,
    1766,     0,     0,     0,     0,    61,   436,   234,     0,   783,
      64,    65,    66,    67,    68,    69,    70,     0,   221,     0,
       0,     0,   716,   716,     0,     0,     0,   117,     0,     0,
     406,     0,     0,    72,     0,     0,     0,     0,   296,   117,
     117,     0,     0,     0,   296,     0,     0,   436,     0,     0,
     745,     0,   745,    73,    74,     0,     0,     0,     0,     0,
     247,     0,     0,     0,    77,    78,     0,     0,     0,   318,
     318,     0,     0,     0,  1810,     0,     0,     0,  1814,   444,
       0,     0,     0,     0,   296,     0,   318,     0,   294,  1821,
       0,     0,     0,     0,     0,   846,     0,   296,     0,     0,
       0,     0,  1838,     0,     0,    61,     0,   294,   215,   216,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
     444,     0,     0,     0,     0,     0,     0,  1681,     0,  1625,
    1625,    61,     0,    72,   167,   168,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   406,     0,     0,     0,     0,
     834,   834,   318,  1493,    74,     0,    57,   146,   406,     0,
    1494,     0,     0,     0,    77,    78,   318,     0,  1164,     0,
    1890,     0,  1892,  1893,  1433,  1434,     0,     0,     0,   591,
      74,     0,     0,     0,     0,     0,  1648,    61,  1448,  1449,
     215,   216,    64,    65,    66,    67,    68,    69,    70,     0,
      61,     0,     0,   654,  1326,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,    72,     0,  1918,     0,   436,
       0,     0,  1471,  1472,     0,     0,     0,  1923,    72,     0,
       0,     0,     0,     0,     0,   217,    74,     0,  1625,   581,
       0,     0,   604,   834,     0,     0,    77,    78,   993,    74,
       0,     0,   588,  1944,     0,  1923,     0,     0,     0,    77,
      78,   170,   173,     0,     0,     0,     0,     0,     0,     0,
    1959,     0,   117,   834,     0,  1944,     0,     0,   834,   834,
     117,     0,   994,   716,   746,     0,     0,     0,     0,  1967,
       0,     0,   444,     0,     0,   210,     0,     0,     0,     0,
     745,     0,     0,     0,     0,     0,  1854,   745,     0,   117,
    1625,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     296,     0,     0,     0,     0,     0,     0,   117,     0,     0,
       0,     0,   810,   812,     0,     0,  1625,     0,    57,   296,
       0,     0,     0,     0,     0,   288,   117,     0,   289,     0,
     318,     0,     0,     0,     0,     0,     0,  1839,     0,     0,
       0,   309,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,   215,   216,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,   117,  1625,  1625,     0,     0,     0,
     146,     0,     0,     0,    61,     0,     0,    72,   406,    64,
      65,    66,    67,    68,    69,    70,  1612,  1613,     0,     0,
       0,     0,     0,     0,   464,     0,     0,  1877,    74,  1625,
       0,   485,     0,     0,     0,     0,     0,   406,    77,    78,
      61,     0,     0,   167,   168,    64,    65,    66,    67,    68,
      69,    70,     0,    74,   243,    81,  1030,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   294,   515,
       0,     0,     0,     0,   146,   654,     0,     0,     0,   170,
     654,   436,     0,   654,     0,     0,     0,     0,     0,    74,
     170,     0,     0,     0,   117,   117,   117,   117,   117,   117,
       0,     0,   654,    13,    14,    15,    16,    17,   436,  1690,
       0,     0,   146,  1328,     0,     0,     0,   561,     0,     0,
       0,     0,     0,   563,   565,     0,     0,     0,   572,     0,
       0,     0,     0,     0,     0,     0,   964,     0,     0,  1708,
       0,     0,   994,     0,  1711,  1712,     0,     0,  1247,   746,
       0,     0,     0,     0,   581,     0,     0,     0,     0,     0,
     617,    57,     0,     0,     0,   309,     0,     0,   309,     0,
       0,     0,     0,   318,   318,     0,     0,     0,     0,     0,
       0,   444,     0,     0,     0,     0,   834,     0,     0,     0,
       0,     0,    61,     0,     0,   117,     0,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,   146,   146,   146,   146,   146,   146,     0,     0,
      72,     0,  1495,   300,   113,     0,     0,   113,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      73,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,    78,    61,     0,   210,   167,   168,    64,    65,
      66,    67,    68,    69,    70,     0,     0,   760,   761,    61,
       0,   243,   530,    63,    64,    65,    66,    67,    68,    69,
      70,   113,     0,     0,     0,     0,     0,     0,   117,     0,
     436,    57,     0,     0,     0,     0,     0,     0,     0,     0,
     296,     0,    74,     0,   113,     0,     0,     0,     0,     0,
       0,     0,  1330,   146,   117,     0,   117,     0,    74,     0,
     249,   967,    61,     0,   113,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    61,     0,   117,
     167,   168,    64,    65,    66,    67,    68,    69,    70,     0,
      72,     0,     0,   117,     0,     0,     0,     0,     0,     0,
       0,   113,     0,   444,     0,     0,     0,   113,     0,   113,
     743,    74,     0,   249,   588,     0,   117,     0,     0,     0,
       0,    77,   744,   315,   113,   346,    74,   309,    13,    14,
      15,    16,    17,   564,     0,  1445,     0,     0,     0,     0,
    1622,   410,     0,     0,  1495,     0,   406,     0,     0,     0,
    1495,     0,  1495,   113,   410,    61,     0,   249,     0,     0,
      64,    65,    66,    67,    68,    69,    70,  1213,     0,     0,
       0,  1214,   406,  1215,   406,   617,     0,     0,     0,     0,
       0,     0,  1922,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,  1499,     0,     0,   406,     0,   117,
       0,     0,     0,   113,    74,     0,   113,  1587,   318,     0,
       0,   146,    13,    14,    15,    16,    17,    61,     0,   249,
     215,   216,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,   146,     0,   535,     0,     0,     0,
       0,     0,     0,     0,   113,    72,     0,     0,     0,   249,
       0,     0,     0,     0,     0,   249,     0,     0,     0,   318,
     318,     0,     0,   113,     0,  1877,    74,     0,     0,   485,
      57,     0,     0,     0,     0,     0,    77,    78,  1622,  1622,
       0,   113,     0,   249,   113,     0,     0,     0,     0,     0,
       0,     0,     0,  1495,     0,     0,  1495,     0,   113,     0,
     117,    61,   113,     0,   215,   216,    64,    65,    66,    67,
      68,    69,    70,  1017,     0,     0,   300,   146,     0,     0,
    1029,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,   410,     0,     0,     0,    13,
      14,    15,    16,    17,     0,   294,     0,     0,   117,   217,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,    78,     0,     0,   117,     0,     0,     0,     0,   410,
       0,     0,     0,     0,     0,     0,  1499,     0,     0,     0,
       0,     0,  1637,     0,  1499,     0,     0,  1622,     0,     0,
       0,     0,     0,  1088,     0,   113,  1495,    57,     0,   410,
     117,     0,     0,     0,     0,   249,    61,     0,   300,   342,
     343,    64,    65,    66,    67,    68,    69,    70,   406,     0,
       0,     0,     0,     0,     0,   146,     0,     0,    61,     0,
       0,   215,   216,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,  1181,     0,     0,     0,     0,   617,     0,
     318,     0,     0,     0,     0,    74,    72,    75,     0,  1622,
       0,     0,   344,     0,   410,   410,   146,     0,     0,   249,
     113,     0,     0,     0,     0,     0,   293,    74,     0,     0,
       0,     0,   146,   146,    61,  1878,   300,    77,    78,    64,
      65,    66,    67,    68,    69,    70,   930,     0,   581,     0,
       0,   113,  1512,     0,     0,     0,   113,     0,     0,   249,
     113,  1520,   113,     0,     0,  1524,     0,  1526,   146,     0,
       0,     0,     0,   113,     0,  1738,     0,     0,  1499,     0,
       0,     0,     0,    74,  1878,  1878,   931,     0,   346,   113,
     410,     0,   249,    13,    14,    15,    16,    17,     0,     0,
       0,     0,     0,     0,     0,     0,   581,     0,     0,     0,
       0,     0,     0,   113,     0,     0,   249,     0,  1878,     0,
     535,     0,     0,   249,     0,     0,   113,   296,   886,     0,
       0,     0,    61,     0,   113,   188,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,   410,     0,   113,
     113,    57,   410,     0,     0,   410,     0,     0,   113,   113,
     113,     0,     0,    97,     0,    61,   150,     0,   215,   216,
      64,    65,    66,    67,    68,    69,    70,     0,  1499,     0,
       0,    74,    61,     0,  1030,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,  1327,  1329,  1331,     0,  1616,     0,     0,     0,
      72,     0,   410,     0,    74,     0,     0,     0,     0,     0,
      97,  1177,     0,     0,     0,     0,     0,     0,     0,     0,
    1493,    74,  1350,     0,     0,     0,   410,     0,     0,     0,
       0,    77,    78,   194,     0,     0,  1653,  1088,  1664,     0,
       0,     0,     0,   410,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   256,     0,     0,     0,   296,     0,     0,
       0,  1653,     0,     0,     0,   113,   113,     0,     0,     0,
       0,     0,     0,   617,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
     286,     0,     0,     0,     0,     0,    97,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   557,   296,     0,     0,
     113,     0,    61,   320,     0,   532,   533,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,   416,     0,   249,     0,     0,     0,     0,     0,     0,
     296,   410,   286,   442,   249,     0,     0,     0,   113,     0,
       0,     0,     0,   113,   410,     0,     0,     0,     0,     0,
       0,    74,   113,    75,  1166,   410,     0,   113,     0,     0,
     479,     0,  1748,     0,     0,     0,  1750,     0,     0,     0,
       0,     0,     0,  1752,     0,     0,   499,     0,     0,     0,
       0,   504,   506,    61,     0,   194,   215,   216,    64,    65,
      66,    67,    68,    69,    70,  1504,     0,     0,  1506,     0,
       0,     0,     0,     0,     0,   410,     0,   526,  1173,     0,
     528,    72,   529,     0,     0,    13,    14,    15,    16,    17,
       0,     0,     0,   546,   384,     0,     0,     0,     0,     0,
       0,  1493,    74,     0,     0,     0,   558,     0,  1494,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,   357,     0,     0,     0,   358,     0,   359,     0,     0,
     579,     0,     0,   603,     0,   113,     0,     0,     0,  1818,
    1820,     0,  1664,    57,   360,     0,     0,   610,     0,     0,
       0,   610,   113,   113,   362,     0,   363,     0,   364,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,   361,   362,     0,   363,     0,   364,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   365,   366,   354,     0,
     367,   368,   369,     0,   370,   371,     0,     0,  1866,     0,
       0,     0,    72,     0,   661,     0,   113,    75,   373,     0,
     700,   701,   702,   703,   704,   705,   706,   707,   708,   709,
     710,     0,   372,     0,   203,    75,   373,     0,     0,     0,
       0,     0,   374,   435,    78,   375,   376,   377,   378,     0,
       0,     0,     0,     0,   286,     0,   113,     0,   579,     0,
    1904,   711,  1906,     0,   410,    57,    61,     0,  1645,   215,
     216,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,  1919,     0,   635,     0,     0,   384,   640,     0,
       0,     0,     0,   410,    72,     0,    61,   643,   644,   215,
     216,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     249,   113,   384,   384,   217,    74,     0,     0,  1947,  1949,
    1950,     0,     0,     0,    72,    77,    78,     0,     0,   442,
     113,     0,     0,   384,     0,     0,   617,   410,     0,     0,
       0,  1166,     0,     0,   293,    74,     0,     0,     0,     0,
       0,     0,     0,  1391,     0,    77,    78,     0,     0,     0,
     830,     0,     0,   384,   410,   506,     0,     0,   113,   841,
      61,   546,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   320,    61,     0,     0,   167,   168,    64,    65,
      66,    67,    68,    69,    70,  1384,     0,     0,   610,   861,
       0,     0,    13,    14,    15,    16,    17,     0,     0,     0,
     113,   113,     0,   872,     0,     0,     0,     0,     0,    74,
       0,    75,   579,     0,   113,   113,     0,   881,     0,   113,
     113,   453,    74,     0,     0,   610,     0,     0,   357,     0,
       0,     0,   358,     0,   359,     0,     0,     0,     0,     0,
       0,     0,     0,  1779,     0,     0,     0,     0,   113,   113,
      57,   360,   617,     0,     0,     0,     0,     0,   113,   113,
     113,   113,   113,   113,     0,     0,     0,     0,     0,   249,
       0,     0,     0,     0,     0,     0,     0,   107,   361,   362,
       0,   363,     0,   364,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   365,   366,   354,     0,   367,   368,   369,
       0,   370,   371,     0,     0,     0,     0,     0,     0,    72,
       0,   442,     0,     0,     0,    61,     0,   249,   167,   168,
      64,    65,    66,    67,    68,    69,    70,     0,   976,   372,
       0,     0,    75,   373,   107,     0,   410,     0,     0,   374,
    1385,    78,   375,   376,   377,   378,     0,     0,     0,     0,
       0,     0,   861,     0,     0,     0,     0,  1000,     0,   113,
       0,     0,     0,   457,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   442,   442,    61,   257,     0,   167,
     168,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,   442,     0,   384,   384,   384,   384,   384,   384,   384,
     384,   384,   384,   384,   384,   384,   384,   384,   384,   384,
     384,   384,     0,     0,     0,     0,     0,     0,     0,   830,
     107,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,   113,   113,     0,     0,     0,   324,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
    1137,     0,   410,     0,     0,     0,     0,   442,     0,     0,
       0,     0,   150,     0,     0,     0,     0,   443,   113,     0,
       0,   610,     0,   384,  1168,     0,   830,   112,   410,    61,
     410,  1174,   215,   216,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,   113,     0,     0,   113,     0,     0,
       0,     0,     0,     0,   320,   113,     0,  1493,    74,     0,
       0,     0,     0,     0,   112,     0,     0,     0,    77,    78,
     113,   527,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   113,     0,   107,     0,     0,
     113,   113,     0,     0,     0,   113,   113,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   259,     0,     0,
       0,     0,     0,     0,   830,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   580,     0,     0,   257,     0,     0,
       0,   830,   830,     0,     0,     0,     0,     0,     0,     0,
       0,   580,     0,     0,     0,   580,     0,     0,     0,     0,
     112,     0,   249,   113,     0,   384,     0,     0,     0,     0,
     384,     0,     0,     0,     0,     0,     0,   328,     0,     0,
       0,   384,    61,     0,     0,   215,   216,    64,    65,    66,
      67,    68,    69,    70,     0,   442,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   445,    61,     0,
      72,   215,   216,    64,    65,    66,    67,    68,    69,    70,
     384,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1877,    74,     0,     0,   485,  1306,    72,     0,     0,     0,
       0,    77,    78,  1137,     0,     0,     0,     0,     0,     0,
       0,     0,   580,     0,   249,     0,   743,    74,     0,     0,
     588,     0,     0,     0,   410,     0,     0,    77,   744,     0,
       0,   113,  1137,     0,     0,     0,     0,     0,     0,     0,
     590,   192,     0,     0,     0,     0,     0,     0,     0,     0,
    1354,     0,     0,     0,     0,     0,   113,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,   579,     0,     0,     0,
       0,     0,     0,   443,     0,   504,     0,     0,   113,   113,
       0,     0,   249,     0,   582,     0,     0,   259,     0,     0,
       0,     0,     0,   320,     0,     0,   192,     0,   384,     0,
       0,   582,     0,     0,   324,   582,     0,     0,     0,     0,
       0,   192,     0,   257,   113,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   443,     0,   192,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   830,
     830,   439,   580,   443,     0,     0,     0,     0,   113,     0,
       0,     0,     0,   830,   830,     0,     0,     0,   442,   442,
       0,     0,     0,     0,     0,     0,   580,     0,     0,     0,
       0,   384,     0,     0,     0,     0,     0,     0,     0,   580,
       0,     0,     0,     0,     0,     0,     0,   830,   830,     0,
       0,     0,     0,   192,     0,     0,     0,  1306,  1306,  1306,
     150,     0,   582,     0,     0,   384,   384,   384,     0,     0,
       0,    61,   384,   384,   215,   216,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   384,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     192,     0,     0,     0,     0,     0,     0,     0,     0,   293,
      74,     0,     0,     0,     0,   443,     0,     0,   192,     0,
      77,    78,     0,   445,     0,   320,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   150,     0,
       0,     0,     0,     0,   328,     0,   443,     0,     0,     0,
       0,     0,     0,   259,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   445,     0,   324,   324,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   582,   445,     0,   324,   118,     0,     0,   118,
       0,     0,     0,     0,     0,     0,   192,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   582,     0,     0,     0,
       0,   830,   830,   324,     0,     0,     0,     0,     0,   582,
       0,     0,     0,     0,     0,     0,   192,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1639,     0,     0,
       0,     0,     0,   118,   107,     0,     0,   830,     0,     0,
       0,   324,     0,     0,     0,     0,     0,  1659,     0,  1659,
       0,     0,     0,     0,     0,   580,   118,     0,   257,     0,
     324,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1659,     0,     0,     0,   118,     0,     0,     0,
       0,   192,   192,   320,     0,     0,   150,   439,     0,     0,
       0,     0,     0,     0,   830,   445,     0,   384,   384,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   443,     0,
       0,     0,     0,   118,     0,     0,     0,     0,     0,   118,
       0,   118,     0,     0,   830,     0,     0,     0,     0,   830,
     830,     0,     0,     0,   442,   442,   445,     0,     0,     0,
     192,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,  1730,     0,     0,   439,   328,   328,
       0,     0,     0,     0,     0,   118,     0,     0,   324,     0,
       0,     0,     0,     0,     0,   328,     0,     0,     0,     0,
     192,     0,   384,     0,     0,   324,   324,     0,     0,     0,
       0,     0,  1751,     0,     0,     0,     0,     0,     0,     0,
       0,   192,     0,   328,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,   118,     0,
       0,     0,     0,   118,     0,     0,   122,     0,     0,   122,
       0,     0,     0,     0,   112,     0,     0,     0,     0,   324,
       0,   328,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   582,   118,     0,   259,     0,
     328,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,   439,
       0,     0,     0,   122,     0,     0,     0,   107,     0,     0,
       0,     0,     0,  1659,     0,     0,     0,     0,     0,     0,
    1829,     0,     0,   192,     0,     0,   122,     0,   445,     0,
       0,     0,     0,     0,     0,     0,   107,     0,     0,     0,
     439,     0,     0,     0,     0,   442,   122,     0,     0,     0,
       0,     0,     0,     0,   257,     0,     0,     0,     0,     0,
       0,  1659,   439,   439,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1659,  1829,   439,
     580,     0,     0,   122,     0,     0,     0,     0,   328,   122,
       0,   122,     0,     0,     0,     0,     0,     0,   384,     0,
       0,   118,     0,     0,     0,   328,   328,   443,     0,     0,
       0,     0,     0,  1659,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,     0,   118,     0,     0,
    1911,     0,   384,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   439,     0,   830,     0,     0,
       0,   192,     0,   324,   324,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,     0,     0,   324,   324,     0,
       0,     0,   324,   324,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,   122,     0,
       0,     0,     0,   122,     0,     0,   118,   118,     0,     0,
       0,   324,   324,     0,     0,     0,     0,   112,     0,     0,
       0,     0,   192,     0,   384,   384,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   112,     0,   118,     0,
       0,   384,   118,     0,   118,   122,     0,     0,     0,     0,
       0,     0,     0,     0,   259,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   384,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     582,     0,     0,     0,     0,     0,     0,     0,     0,   443,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   445,     0,     0,
       0,     0,     0,   384,     0,     0,   118,   122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,   118,   118,     0,   118,     0,     0,   118,     0,     0,
     118,   118,   118,   439,     0,     0,     0,     0,     0,     0,
       0,   122,     0,   328,   328,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   328,   328,     0,
       0,     0,   328,   328,     0,     0,     0,   122,     0,     0,
       0,     0,     0,     0,     0,   324,   324,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   328,   328,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   324,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   107,     0,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,   122,     0,     0,
       0,     0,     0,     0,     0,     0,   107,   192,     0,     0,
       0,     0,     0,     0,   192,     0,     0,   324,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   324,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,   445,
       0,   192,   122,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   324,     0,
       0,     0,     0,   324,   324,     0,     0,     0,   324,   324,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     202,     0,     0,   118,     0,     0,   212,   213,     0,     0,
       0,     0,     0,     0,     0,   118,   118,     0,     1,   357,
       0,   144,     0,   358,     0,   359,   439,   439,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
     275,     0,   360,     0,     0,     0,   257,     0,     0,   122,
       0,   122,   122,     0,   122,   328,   328,   122,     0,     0,
     122,   122,   122,     0,     0,     0,     0,     0,     0,   361,
     362,     0,   363,     0,   364,  1816,    63,    64,    65,    66,
      67,    68,    69,    70,   365,   366,   354,     0,   367,   368,
     369,   328,   370,   371,   191,     0,     0,     0,     0,     0,
      72,   112,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,    75,   373,     0,   112,     0,     0,     0,
     374,    77,    78,   375,   376,   377,   378,   328,   122,     0,
       0,     0,     0,   192,  1817,  -173,     0,   107,   328,     0,
       0,     0,   281,     0,   580,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   328,   324,
       0,     0,     0,   328,   328,     0,     0,     0,   328,   328,
       0,     0,     0,     0,     0,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   554,     0,     0,     0,     0,
       0,   107,   580,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   281,   259,   107,   118,     0,
       0,     0,     0,   122,     0,     0,   118,     0,     0,   192,
     507,     0,     0,     0,     0,   122,   122,     0,     0,     0,
     281,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     281,   324,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,   538,   542,     0,     0,     0,     0,
       0,   549,   550,   118,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   560,     0,     0,
       0,   192,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   577,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,     0,     0,
     741,     0,   742,     0,   582,     0,     0,     0,     0,     0,
     118,   758,   759,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   439,   439,     0,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   660,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   112,   582,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   699,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   112,   737,     0,
       0,     0,   740,     0,     0,     0,     0,   164,     0,   840,
     118,   118,   118,   118,   118,   118,     0,     0,     0,     0,
       0,   762,     0,     0,     0,   763,   764,     0,     0,     0,
       0,   328,     0,   164,   779,   780,   781,   782,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,   804,     0,     0,   122,     0,     0,     0,
       0,   807,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   164,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,   281,
       0,   164,     0,   164,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
       0,   118,     0,   439,     0,     0,     0,     0,     0,     0,
     845,     0,   122,   348,     0,     0,     0,   538,     0,     0,
       0,     0,   850,     0,     0,     0,     0,     0,     0,     0,
     348,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   864,   869,     0,     0,     0,
     122,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   164,     0,     0,
       0,   164,     0,     0,   164,   164,     0,     0,   164,     0,
       0,   164,   164,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   912,     0,     0,     0,
       0,  1015,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,     0,   118,   164,     0,     0,     0,
     122,   122,   122,   122,   122,   122,     0,     0,   162,   118,
       0,     0,     0,     0,     0,     0,   164,     0,     0,     0,
       0,     0,     0,   972,     0,     0,     0,     0,     0,     0,
       0,   164,   118,     0,     0,     0,  1084,  1085,   989,     0,
       0,     0,   990,     0,     0,     0,     0,  1143,  1144,  1145,
       0,   864,  1147,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1032,     0,     0,     0,   273,
       0,     0,     0,     0,     0,     0,  1043,     0,     0,     0,
       0,     0,   279,     0,   280,     0,     0,     0,     0,     0,
       0,   122,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     1,     0,   164,     0,     0,     0,     0,
       1,  1211,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     348,     0,     0,     0,  1230,     0,     0,     0,     0,     0,
       0,     0,   164,     0,     0,   489,   490,     0,     0,   494,
       0,     0,   497,   498,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1191,     0,     0,     0,     0,     0,   118,     0,     0,     0,
     122,  1254,   122,     0,     0,     0,     0,     0,     0,     0,
    1258,  1259,  1260,  1261,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,     0,     0,  1275,
       0,     0,     0,     0,     0,     0,   348,     0,     0,   122,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
    1289,     0,  1290,     0,     0,     0,     0,   575,     0,     0,
     118,     0,   122,     0,     0,     0,     0,     0,     0,     0,
    1237,     0,   607,     0,  1238,   164,   164,     0,     0,     0,
       0,   864,     0,     0,     0,     0,     0,     0,   164,     0,
       0,  1251,     0,     0,     0,     0,   118,     0,  1252,     0,
       0,     0,     0,     0,     0,  1346,     0,  1256,     0,  1257,
       0,     0,     0,     0,     0,     0,     0,     0,  1263,     0,
       0,     0,     0,     0,     0,     0,  1271,     0,     0,     0,
       0,  1360,     0,     0,     0,     0,     0,  1364,     0,  1366,
    1368,     0,  1285,     0,     0,   122,  1286,     0,  1374,     0,
    1375,     0,  1376,     0,  1378,     0,     0,     0,     0,  1386,
     144,     0,     0,     1,     0,     0,   731,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   164,
     164,     0,     0,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1428,     0,     0,
       0,     0,     0,   800,  1435,  1436,   164,     0,     0,   164,
     164,     0,   164,     0,   164,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
    1373,  1462,  1463,     0,     0,     0,     0,     0,     0,  1466,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   245,
       0,   164,     0,     0,  1396,   164,     0,     0,     0,     0,
     266,     0,   269,     0,   271,     0,     0,     0,     0,   212,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   245,     0,   269,   271,     0,     0,     0,  1541,
       0,     0,     0,     0,     0,     0,   875,   876,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,   883,
     164,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1560,     0,     0,  1469,   245,     0,     0,  1470,
    1565,     0,  1567,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1505,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1515,  1517,     0,     0,     0,     0,  1593,  1594,
       0,     0,     0,     0,     0,     0,     0,     0,   245,     0,
     269,   271,     0,  1599,  1600,     0,  1601,     0,     0,     0,
       0,     0,     0,     0,  1604,     0,     0,     0,     0,  1551,
    1609,     0,  1554,     0,     0,     0,  1614,  1615,   245,     0,
     978,   979,     0,     0,   245,     0,   983,  1562,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   164,
       0,     0,     0,     0,     0,     0,     0,  1004,     0,     0,
    1007,  1008,   245,  1011,     0,  1013,  1014,     0,   606,     0,
     271,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1592,     0,     0,
       0,     0,     0,     0,     0,     0,  1597,     0,   164,     0,
    1598,   164,  1055,     0,     0,     0,  1059,     0,     0,     0,
       0,     0,     0,     0,  1602,  1603,     0,     0,  1691,  1692,
       0,     0,     0,     0,     0,     0,     0,     0,  1698,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   335,
       0,     0,     0,     0,     0,     0,     0,     0,   245,     0,
    1721,     0,     0,     0,     0,     0,     0,  1722,  1723,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   432,   335,
       0,  1175,     0,     0,   245,     0,   606,   271,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   488,
       0,     0,     0,  1687,  1688,     0,   488,     0,     0,     0,
     164,   245,     0,     0,     0,     0,     0,     0,   164,   164,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   245,     0,     0,     0,  1781,   245,     0,   245,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1790,     0,     0,  1791,  1792,   245,     0,
     245,   245,     0,  1794,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,   488,     0,     0,     0,   245,     0,
       0,     0,   164,     0,     0,   164,     0,   164,   164,     0,
     245,     0,     0,     0,     0,     0,   335,   592,     0,     0,
    1175,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   245,     0,   606,   271,     0,     0,   613,     0,     0,
       0,     0,     0,     0,  1768,     0,     0,     0,   164,     0,
       0,     0,     0,     0,     0,   245,   606,     0,     0,     0,
       0,     0,   245,     0,     0,     0,  1554,     0,     0,  1278,
       0,     0,  1282,     0,     0,     0,     0,     0,     0,  1864,
       0,     0,     0,     0,  1793,     0,  1872,     0,     0,     0,
       0,     0,  1876,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   488,     0,     0,
       0,  1809,     0,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,   488,   733,     0,   488,   736,     0,     0,
       0,     0,  1825,     0,   335,  1826,     0,     0,   592,     0,
    1908,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1925,     0,     0,     0,     0,   488,
       0,     0,  1930,   488,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1943,     0,     0,
       0,  1380,     0,     0,     0,     0,     0,   164,     0,  1389,
    1390,     0,     0,     0,     0,   335,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1895,     0,     0,     0,     0,     0,     0,   164,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   164,  1430,   488,     0,     0,   335,   164,
       0,     0,     0,  1439,     0,     0,  1443,     0,  1446,  1447,
       0,     0,     0,     0,   859,   335,     0,     0,     0,     0,
       0,     0,   245,     0,     0,   592,     0,     0,     0,   592,
       0,     0,     0,   245,     0,     0,   877,     0,   335,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1474,
     164,     0,     0,   245,     0,     0,   240,     0,     0,     0,
       0,     0,     0,     0,   245,    13,    14,    15,    16,    17,
       0,     0,    19,   245,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -402,
    -402,     0,  -402,    45,    46,     0,  -402,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1548,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,   164,   164,
       0,     0,     0,     0,     0,     0,   348,   335,     0,     0,
     164,     0,     0,     0,     0,     0,   199,     0,     0,     0,
       0,     0,     0,   488,   488,     0,     0,    62,    63,     0,
       0,     0,   253,   488,   985,     0,   488,   988,   245,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   335,     0,
       0,   592,    72,   592,   592,     0,     0,     0,     0,     0,
     592,     0,   245,     0,     0,     0,     0,     0,  1443,     0,
     335,   335,     0,     0,     0,    75,   299,     0,     0,     0,
       0,   199,     0,    77,    78,   301,     0,   335,     0,     0,
       0,   488,     0,     0,     0,   488,   340,     0,  1611,   488,
    1057,     0,   164,   488,  1061,     0,     0,     0,     0,     0,
       0,  1064,     0,   199,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   452,     0,     0,   456,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   335,   488,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   164,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   199,     0,
     592,  1685,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   253,     0,     0,     0,     0,     0,     0,     0,   245,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     335,     0,     0,     0,     0,   408,     0,   456,     0,     0,
     245,     0,     0,     0,     0,   199,   245,     0,   437,     0,
       0,     0,     0,     0,     0,     0,     0,   164,     0,     0,
       0,     0,   461,   585,     0,   602,     0,     0,     0,  1734,
    1735,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1739,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   488,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   592,   592,     0,     0,     0,     0,   658,   592,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   555,   357,
       0,   199,     0,   358,     0,   359,     0,     0,     0,     0,
       0,   335,     0,     0,     0,     0,   488,  1280,     0,   488,
    1284,     0,   360,  1802,     0,     0,     0,     0,   245,     0,
       0,   585,     0,     0,     0,     0,     0,   757,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
     362,     0,   765,     0,   364,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   365,   366,   354,     0,   367,   368,
     369,     0,   370,   371,     0,     0,   245,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,  1853,
       0,     0,     0,     0,     0,     0,   199,   199,     0,     0,
     372,    74,   452,   766,   767,     0,     0,     0,   460,     0,
     374,    77,    78,   375,   376,   377,   378,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   335,     0,     0,     0,     0,     0,   592,  1382,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   340,     0,     0,     0,   335,
       0,     0,     0,     0,     0,     0,     0,   461,     0,     0,
     776,     0,   452,     0,   863,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   585,     0,     0,     0,     0,
       0,   488,  1432,     0,     0,     0,     0,     0,     0,     0,
     488,  1441,     0,   592,     0,     0,   199,     0,     0,     0,
       0,     0,     0,     0,   335,   335,     0,     0,     0,   658,
       0,   658,   658,     0,   658,     0,     0,   658,     0,     0,
     658,   658,   658,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,    19,   844,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,   437,     0,     0,    45,    46,     0,
       0,     0,     0,     0,   452,     0,     0,   871,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   199,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   452,     0,     0,     0,     0,
       0,   335,     0,     0,     0,     0,     0,     0,   906,     0,
       0,     0,     0,     0,     0,     0,     0,   452,   452,     0,
       0,   245,     0,     0,     0,     0,   776,   926,     0,     0,
       0,     0,     0,     0,   452,     0,   936,     0,   941,   936,
       0,   245,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   969,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   971,
       0,     0,     0,     0,     0,   357,     0,     0,     0,   358,
     980,   359,     0,     0,     0,     0,     0,     0,     0,     0,
     452,     0,     0,     0,   437,     0,   199,   969,   360,     0,
       0,   488,   245,     0,     0,     0,   757,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   488,     0,     0,
       0,     0,     0,   245,     0,   361,   362,     0,   363,   461,
     364,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     365,   366,   354,     0,   367,   368,   369,     0,   370,   371,
       0,     0,     0,     0,     0,     0,    72,   340,     0,     0,
       0,  1065,     0,     0,     0,     0,     0,     0,     0,   335,
       0,     0,     0,     0,     0,     0,   372,  1207,     0,    75,
     373,     0,     0,     0,  1208,     0,   374,    77,    78,   375,
     376,   377,   378,     0,     0,     0,     0,     0,     0,     0,
       0,   245,     0,     0,     0,     0,     0,     0,   408,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1165,  1167,
     335,   335,     0,     0,     0,     0,   437,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   488,
     488,     0,     0,     0,     0,     0,     0,   461,     0,     0,
       0,     0,     0,     0,     0,   488,   936,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   969,
       0,     0,     0,     0,     0,     0,     0,  1204,     0,     0,
       0,     0,     0,   245,   936,     0,     0,     0,   452,     0,
       0,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,   461,   658,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,   488,     0,
       0,    57,     0,     0,     0,     0,   488,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   253,     0,     0,    62,    63,     0,     0,  1266,
       0,   461,     0,     0,     0,     0,     0,     0,     0,  1273,
       0,     0,   199,     0,     0,     0,     0,     0,     0,   585,
      72,   335,     0,     0,     0,     0,   488,  1855,     0,     0,
     488,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   908,    75,   902,     0,   340,     0,     0,     0,
     658,    77,    78,     0,     0,     0,   488,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1338,  1338,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   488,   488,     0,     0,     0,
       0,   452,   452,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1377,     0,     0,     0,   488,
       0,  1387,     0,     0,     0,     0,     0,     0,     0,     0,
     658,   658,   658,     0,   658,   658,     0,     0,   437,     0,
     357,   456,     0,     0,   358,     0,   359,     0,     0,     0,
       0,     0,     0,     0,     0,   461,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,     0,   936,     0,
       0,   776,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   253,
     361,   362,     0,   363,     0,   364,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   365,   366,   354,   340,   367,
     368,   369,     0,   370,   371,     0,   461,     0,  1465,     0,
       0,    72,     0,  1468,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   372,     0,     0,    75,   373,     0,     0,     0,   460,
       0,   374,    77,    78,   375,   376,   377,   378,     0,     0,
       0,   936,     0,     0,     0,   240,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,   461,
       0,    19,   776,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
       0,  -402,    45,    46,     0,  -402,     0,     0,     0,     0,
     926,     0,     0,     0,   199,     0,     0,     0,     0,     0,
    1563,  1564,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   461,     0,   776,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   340,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   658,     0,    75,   242,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   408,   452,   452,     0,
       0,  1638,     0,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -401,  -401,     0,
    -401,    45,    46,  1677,  -401,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   253,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,   240,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,  1699,     0,    19,  1701,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   253,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   658,     0,     0,    61,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   452,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,     0,    75,   242,     0,
       0,   658,  -708,     0,   456,    77,    78,     0,     0,     0,
       0,     0,     0,     4,   241,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,  1089,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   357,     0,
      45,    46,   358,   936,   359,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,  1090,
      57,  1091,    -2,     0,  1092,     0,     0,  1093,  1094,  1095,
    1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1104,  -281,
    1105,  1106,  1107,  1108,  1109,     0,  1110,     0,   361,   362,
      60,   765,     0,   364,  1111,  1112,    64,    65,    66,    67,
      68,    69,    70,   365,   366,   354,  1113,   367,   368,   369,
       0,   370,   371,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -3,   372,
      74,     0,    75,   404,     0,     0,     0,   277,     0,   374,
      77,    78,   375,   376,   377,   378,     0,     0,     0,     0,
       0,     0,     0,     0,  -173,     4,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,  1089,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     357,     0,    45,    46,   358,     0,   359,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,  1090,    57,  1091,    -2,     0,  1092,     0,     0,  1093,
    1094,  1095,  1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,
    1104,  -281,  1105,  1106,  1107,  1108,  1109,     0,  1110,     0,
     361,   362,    60,   765,     0,   364,  1111,  1112,    64,    65,
      66,    67,    68,    69,    70,   365,   366,   354,  1113,   367,
     368,   369,     0,   370,   371,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   372,    74,     0,    75,   404,     0,     0,     0,   277,
       0,   374,    77,    78,   375,   376,   377,   378,     0,     0,
       0,     0,     0,     0,     0,     0,  -173,     4,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   357,     0,    45,    46,   358,     0,   359,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   361,   362,    60,   363,     0,   364,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   365,   366,   354,
       0,   367,   368,   369,     0,   370,   371,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   372,     0,     0,    75,   404,     0,     0,
       0,     0,     0,   374,    77,    78,   375,   376,   377,   378,
       0,     0,     0,     0,     0,     0,     0,  1662,  1663,     4,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   357,     0,    45,    46,   358,     0,
     359,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,   360,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   361,   362,    60,   363,     0,   364,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   365,
     366,   354,     0,   367,   368,   369,     0,   370,   371,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   372,     0,     0,    75,   404,
       0,     0,     0,     0,     0,   374,    77,    78,   375,   376,
     377,   378,   241,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   357,     0,    45,    46,
     358,     0,   359,   316,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   361,   362,     0,   363,
       0,   364,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   365,   366,   354,     0,   367,   368,   369,     0,   370,
     371,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   372,     0,     0,
      75,   434,     0,     0,     0,     0,     0,   374,   435,    78,
     375,   376,   377,   378,   241,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   357,     0,
      45,    46,   358,     0,   359,   316,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   360,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   361,   362,
       0,   363,     0,   364,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   365,   366,   354,     0,   367,   368,   369,
       0,   370,   371,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   372,
       0,     0,    75,  1162,     0,     0,     0,     0,     0,   374,
    1163,    78,   375,   376,   377,   378,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     357,     0,    45,    46,   358,     0,   359,   316,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   360,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     361,   362,     0,   363,     0,   364,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   365,   366,   354,     0,   367,
     368,   369,     0,   370,   371,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   372,     0,     0,    75,   373,     0,     0,     0,     0,
       0,   374,    77,    78,   375,   376,   377,   378,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   357,     0,    45,    46,   358,     0,   359,   316,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   361,   362,     0,   363,     0,   364,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   365,   366,   354,
       0,   367,   368,   369,     0,   370,   371,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   372,     0,     0,    75,   434,     0,     0,
       0,     0,     0,   374,    77,    78,   375,   376,   377,   378,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    59,     0,     0,     0,    60,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,     4,
     241,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,     0,     0,     0,
       0,  -334,  -334,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -334,     0,     0,     0,    75,    76,
       0,     0,     0,     0,     0,     0,    77,    78,     4,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,     0,     0,     0,     0,
    -335,  -335,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -335,     0,     0,     0,    75,    76,     0,
       0,     0,     0,     0,     0,    77,    78,   240,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,   242,     0,     0,
    1298,     0,     0,     0,    77,    78,  1299,     0,     0,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,  1300,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1301,     0,     0,     0,    75,
     902,     0,     0,  1298,     0,     0,     0,    77,    78,  1299,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,  1300,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1480,     0,
       0,     0,    75,   902,     0,     0,  1298,     0,     0,     0,
      77,    78,  1299,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,  1300,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1481,     0,     0,     0,    75,   902,     0,     0,  1298,
       0,     0,     0,    77,    78,  1299,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,  1300,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1482,     0,     0,     0,    75,   902,
       0,     0,     0,     0,     0,     0,    77,    78,   241,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1322,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,   242,     0,     0,
       0,  -712,   357,     0,    77,    78,   358,     0,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1090,     0,   360,     0,     0,  1092,  1741,
    1742,  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,  1101,
    1102,  1103,  1104,  -281,  1105,  1106,  1107,  1108,  1109,     0,
    1110,     0,   361,   362,     0,   765,     0,   364,  1111,  1112,
      64,    65,    66,    67,    68,    69,    70,   365,   366,   354,
    1113,   367,   368,   369,     0,   370,   371,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   372,    74,     0,    75,   373,     0,     0,
       0,   277,     0,   374,    77,    78,   375,   376,   377,   378,
       0,     0,     0,     0,     0,     0,     0,     0,  -173,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -402,  -402,     0,  -402,    45,    46,     0,  -402,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,  1322,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,   357,     0,
       0,     0,   358,     0,   359,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,     0,    75,   242,  1090,
       0,   360,    -2,     0,  1092,    77,    78,  1093,  1094,  1095,
    1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1104,  -281,
    1105,  1106,  1107,  1108,  1109,     0,  1110,     0,   361,   362,
       0,   765,     0,   364,  1111,  1112,    64,    65,    66,    67,
      68,    69,    70,   365,   366,   354,  1113,   367,   368,   369,
       0,   370,   371,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,  1322,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   372,
      74,     0,    75,   373,     0,     0,     0,   277,     0,   374,
      77,    78,   375,   376,   377,   378,   357,     0,     0,     0,
     358,     0,   359,     0,  -173,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1090,     0,   360,
       0,     0,  1092,     0,     0,  1093,  1094,  1095,  1096,  1097,
    1098,  1099,  1100,  1101,  1102,  1103,  1104,  -281,  1105,  1106,
    1107,  1108,  1109,     0,  1110,     0,   361,   362,     0,   765,
       0,   364,  1111,  1112,    64,    65,    66,    67,    68,    69,
      70,   365,   366,   354,  1113,   367,   368,   369,     0,   370,
     371,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   372,    74,     0,
      75,   373,     0,     0,     0,   277,     0,   374,    77,    78,
     375,   376,   377,   378,     0,     0,     0,     0,     0,     0,
       0,     0,  -173,   241,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   316,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,    62,    63,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
    1024,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -583,    75,   317,     0,     0,    62,    63,     0,     0,    77,
      78,   241,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    75,     0,     0,     0,    45,    46,     0,
       0,     0,   316,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,   316,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,  1717,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     317,     0,     0,    62,    63,     0,     0,    77,    78,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    75,     0,     0,     0,    45,    46,     0,     0,     0,
     316,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,  1719,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   317,     0,
       0,     0,     0,     0,     0,    77,    78,   241,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,   316,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   317,     0,     0,     0,
       0,     0,     0,    77,    78,   241,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   316,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   299,     0,     0,     0,     0,     0,
       0,    77,    78,   241,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -402,  -402,     0,  -402,    45,
      46,     0,  -402,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,    19,    57,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -402,  -402,     0,  -402,    45,
      46,     0,  -402,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   242,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,   645,    19,   646,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   357,     0,    45,
      46,   358,     0,   359,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     360,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   647,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,   362,     0,
     363,     0,   364,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   365,   366,   354,     0,   367,   368,   369,     0,
     370,   371,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,    75,   648,     0,     0,     0,   277,     0,   374,    77,
      78,   649,   650,   377,   378,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   357,     0,    45,    46,   358,     0,   359,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   361,   362,     0,   363,     0,   364,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   365,   366,   354,     0,
     367,   368,   369,     0,   370,   371,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,     0,   403,    75,   404,     0,     0,     0,
       0,     0,   374,    77,    78,   375,   376,   377,   378,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   357,     0,    45,    46,   358,
       0,   359,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   360,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   361,   362,     0,   363,     0,
     364,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     365,   366,   354,     0,   367,   368,   369,     0,   370,   371,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   372,     0,     0,    75,
     648,     0,     0,     0,   277,     0,   374,    77,    78,   375,
     376,   377,   378,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   357,
       0,    45,    46,   358,     0,   359,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
     362,     0,   363,     0,   364,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   365,   366,   354,     0,   367,   368,
     369,     0,   370,   371,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,    75,   404,     0,     0,     0,     0,     0,
     374,    77,    78,   375,   376,   377,   378,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   357,     0,    45,    46,   358,     0,   359,
     316,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   360,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   361,   362,     0,   363,     0,   364,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   365,   366,
     354,     0,   367,   368,   369,     0,   370,   371,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   372,     0,     0,    75,   434,     0,
       0,     0,     0,     0,   374,    77,    78,   375,   376,   377,
     378,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   357,     0,    45,
      46,   358,     0,   359,   316,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     360,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,   362,     0,
     363,     0,   364,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   365,   366,   354,     0,   367,   368,   369,     0,
     370,   371,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   372,     0,
       0,    75,   373,     0,     0,     0,     0,     0,   374,    77,
      78,   375,   376,   377,   378,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,    76,     0,     0,     0,
    -710,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,    76,     0,     0,     0,
       0,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    75,    76,     0,    13,    14,
      15,    16,    17,    77,    78,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -402,  -402,     0,  -402,    45,    46,     0,  -402,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,     0,    75,   299,
       0,     0,     0,     0,     0,     0,    77,    78,   543,   241,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,  1399,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     902,     0,     0,     0,     0,     0,     0,    77,    78,    13,
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
     284,     0,     0,    62,    63,     0,     0,    77,    78,     0,
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
       0,    45,    46,    62,    63,     0,   316,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   430,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   317,     0,     0,     0,     0,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   316,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   316,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   284,     0,     0,    62,    63,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   430,     0,     0,     0,
       0,     0,     0,    77,    78,   240,   241,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
       0,  -402,    45,    46,     0,  -402,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,   316,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,    75,     0,    45,    46,    62,    63,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   299,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   902,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   316,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   316,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   902,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,    13,    14,    15,    16,    17,    77,    78,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -402,  -402,     0,
    -402,    45,    46,     0,  -402,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,    57,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -402,  -402,     0,
    -402,    45,    46,     0,  -402,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   299,    62,    63,     0,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,    77,    78,   241,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   316,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     829,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -596,    75,   241,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   316,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1646,
       0,     0,     0,     0,   241,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
      75,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   316,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   241,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    62,    63,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -402,  -402,     0,  -402,    45,    46,     0,
    -402,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,    74,    19,    75,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -402,  -402,     0,  -402,    45,    46,     0,
    -402,     0,     0,   357,     0,     0,     0,   358,     0,   359,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,   361,   362,     0,   363,     0,   364,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   365,   366,
     354,     0,   367,   368,   369,   357,   370,   371,     0,   358,
       0,   359,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,    75,
       0,     0,     0,     0,   372,   775,     0,    75,   373,     0,
       0,     0,     0,     0,   374,    77,    78,   375,   376,   377,
     378,     0,     0,     0,     0,   361,   362,     0,   363,     0,
     364,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     365,   366,   354,     0,   367,   368,   369,   357,   370,   371,
       0,   358,     0,   359,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,     0,   372,     0,     0,    75,
     373,     0,     0,     0,   277,     0,   374,    77,    78,   375,
     376,   377,   378,     0,     0,     0,     0,   361,   362,     0,
     363,     0,   364,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   365,   366,   354,     0,   367,   368,   369,   357,
     370,   371,     0,   358,     0,   359,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,   372,   933,
       0,    75,   373,     0,     0,     0,     0,     0,   374,    77,
      78,   375,   376,   377,   378,     0,     0,     0,     0,   361,
     362,     0,   363,     0,   364,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   365,   366,   354,     0,   367,   368,
     369,   357,   370,   371,     0,   358,     0,   359,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   360,     0,     0,     0,     0,     0,
     372,     0,     0,    75,   373,     0,     0,   963,     0,     0,
     374,    77,    78,   375,   376,   377,   378,     0,     0,     0,
       0,   361,   362,     0,   363,     0,   364,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   365,   366,   354,     0,
     367,   368,   369,   357,   370,   371,     0,   358,     0,   359,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
       0,     0,   372,  1272,     0,    75,   373,     0,     0,     0,
       0,     0,   374,    77,    78,   375,   376,   377,   378,     0,
       0,     0,     0,   361,   362,     0,   363,     0,   364,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   365,   366,
     354,     0,   367,   368,   369,   357,   370,   371,     0,   358,
       0,   359,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,     0,
       0,     0,     0,     0,   372,     0,     0,    75,   373,     0,
       0,     0,  1332,     0,   374,    77,    78,   375,   376,   377,
     378,     0,     0,     0,     0,   361,   362,     0,   363,     0,
     364,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     365,   366,   354,     0,   367,   368,   369,   357,   370,   371,
       0,   358,     0,   359,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,     0,   372,     0,  1747,    75,
     373,     0,     0,     0,     0,     0,   374,    77,    78,   375,
     376,   377,   378,     0,     0,     0,     0,   361,   362,     0,
     363,     0,   364,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   365,   366,   354,     0,   367,   368,   369,   357,
     370,   371,     0,   358,     0,   359,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,   372,  1948,
       0,    75,   373,     0,     0,     0,     0,     0,   374,    77,
      78,   375,   376,   377,   378,     0,     0,     0,     0,   361,
     362,     0,   363,     0,   364,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   365,   366,   354,     0,   367,   368,
     369,   357,   370,   371,     0,   358,     0,   359,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   360,     0,     0,     0,     0,     0,
     372,     0,     0,    75,   373,     0,     0,     0,     0,     0,
     374,    77,    78,   375,   376,   377,   378,     0,     0,     0,
       0,   361,   362,     0,   363,     0,   364,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   365,   366,   354,     0,
     367,   368,   369,   357,   370,   371,     0,   358,     0,   359,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
       0,     0,   634,     0,     0,    75,   373,     0,     0,     0,
       0,     0,   374,    77,    78,   375,   376,   377,   378,     0,
       0,     0,     0,   361,   362,     0,   363,     0,   364,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   365,   366,
     354,     0,   367,   368,   369,   357,   370,   371,     0,   358,
       0,   359,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   360,     0,
       0,     0,     0,     0,   639,     0,     0,    75,   373,     0,
       0,     0,     0,     0,   374,    77,    78,   375,   376,   377,
     378,     0,     0,     0,     0,   361,   362,     0,   363,     0,
     364,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     365,   366,   354,     0,   367,   368,   369,   357,   370,   371,
       0,   358,     0,   359,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,     0,     0,   642,     0,     0,    75,
     373,     0,     0,     0,     0,     0,   374,    77,    78,   375,
     376,   377,   378,     0,     0,     0,     0,   361,   362,     0,
     363,     0,   364,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   365,   366,   354,     0,   367,   368,   369,   357,
     370,   371,     0,   358,     0,   359,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,   372,     0,
       0,    75,   373,     0,     0,     0,     0,     0,   374,   843,
      78,   375,   376,   377,   378,     0,     0,     0,     0,   361,
     362,     0,   363,     0,   364,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   365,   366,   354,     0,   367,   368,
     369,     0,   370,   371,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,     0,     0,    75,   373,     0,     0,     0,     0,     0,
     374,   435,    78,   375,   376,   377,   378
};

static const yytype_int16 yycheck[] =
{
       1,     1,     1,     4,   149,   619,    73,    73,    94,    73,
     161,   659,    73,   281,   217,   205,   177,   239,   176,   372,
     851,     4,    81,   172,     1,   218,   254,     1,     1,  1099,
     320,   217,   161,    73,   217,   771,   162,   600,   970,   522,
    1683,   217,   405,   217,   593,  1741,   217,   745,  1256,  1257,
     838,  1083,   217,   745,    55,    56,   220,    58,    58,    58,
     652,   191,   716,    75,   155,   587,   177,    87,   587,   587,
     672,  1621,    73,   838,   227,    70,   501,   502,   763,   562,
      81,    58,    81,   174,    58,    58,   938,   146,    89,   234,
     293,   746,     1,    94,   779,    94,    97,   752,    97,   252,
     101,   294,   101,  1621,    58,   591,   743,   293,  1621,   262,
     293,   138,   161,   180,   180,   967,   180,   293,   101,   293,
      94,   822,   293,    97,   288,   289,    70,   101,   293,     0,
      73,   743,   218,   130,   743,   335,  1078,   157,   139,   339,
     180,   142,     1,   144,   144,   144,    73,   848,   149,    58,
     217,   217,   442,   217,   155,    70,   217,   893,   153,   372,
     845,   162,  1858,   190,   560,   115,     1,   144,   743,  1181,
     144,   144,    82,    59,    60,   172,  1083,   217,   179,   180,
     179,   148,  1745,    81,   243,  1515,     1,   155,   155,     4,
     144,    70,   101,   194,     0,   194,   621,   165,   747,    58,
      70,     1,   751,   153,     4,   149,  1148,   208,   294,   153,
    1540,   760,   761,   104,   105,   226,   217,   218,   229,   218,
     194,   743,     0,    58,   743,   743,   293,   293,   318,   293,
     115,  1214,   293,   234,   149,   144,   149,   180,   153,   149,
     251,   300,   243,    58,   243,   335,    75,    76,   146,   339,
     261,  1083,   253,   180,   617,   256,   484,   256,    58,   745,
     272,   152,   263,  1295,  1296,  1297,    81,   470,    87,   318,
     149,   157,   273,   274,   217,   276,  1208,  1920,   892,   149,
     178,   549,   256,   153,   470,   144,   479,   470,   174,   579,
     217,  1622,   293,   294,   470,   294,   470,   899,   723,   470,
     301,   101,  1000,  1091,   281,   470,   307,   308,  1000,   144,
     149,   312,     1,   603,   440,     4,   149,   670,   130,    95,
     610,    19,   976,   138,   916,   927,  1091,  1877,   157,   144,
     263,   146,  1120,   102,   173,  1898,   819,   106,   157,   994,
     109,   274,   111,   344,   144,   243,   436,   406,   349,    87,
    1292,   352,   353,   556,   499,  1120,   993,     1,  1043,  1877,
     505,   762,   763,  1926,  1877,   558,   762,   763,   521,    58,
     556,   154,   281,   556,  1075,   190,   576,   507,   779,  1391,
     556,   993,   556,   779,   993,   556,   872,   436,  1295,  1296,
    1297,   556,    87,   479,  1602,  1603,   151,   209,   174,  1730,
    1332,   156,   300,   470,   470,   432,   470,   313,    97,    75,
      76,  1263,   101,   613,    58,   416,   146,   416,   993,   157,
     318,   634,   281,   636,   637,   240,   639,   131,   243,   642,
     470,   560,   645,   646,   647,  1418,  1419,  1420,   439,   440,
      96,   120,   416,   173,   845,   351,   281,   577,  1184,   845,
     451,   452,   146,    97,   131,   144,   104,   105,    70,   460,
     155,   165,   157,  1295,  1296,  1297,   281,    70,  1017,   470,
     282,   993,   558,   152,   993,   993,  1408,   246,   479,   173,
     479,   492,   155,    70,    73,   300,   576,   155,   165,   556,
     556,   157,   556,   149,    70,  1231,    70,  1828,   499,    88,
     144,   174,   155,   514,   505,   479,   174,   149,   406,   520,
     600,   560,   165,   524,  1000,  1170,   912,    70,    56,    70,
     151,    59,    60,   613,    62,   156,    70,   470,     4,     5,
       6,     7,     8,     9,    10,    11,   149,   149,   436,   697,
     830,   153,   543,   470,   545,   155,   149,  1878,   149,   155,
     153,   600,    70,   322,   323,   556,   325,   558,   327,   558,
     372,  1413,   149,  1177,   174,    70,   153,   256,   174,   699,
     571,   861,   549,   149,   575,   149,  1224,   153,    70,   153,
    1911,    70,   772,    73,   131,  1793,   349,    70,   155,   352,
    1245,   406,   149,  1663,  1860,   756,   149,  1667,   149,    89,
     153,  1164,   153,   501,   502,   149,   607,   174,  1874,   153,
     740,   149,   256,   160,   161,   149,   106,   432,   619,   745,
     155,   150,   155,   157,     1,   151,   174,     4,   157,   157,
     156,   149,   762,   763,  1900,   153,   781,   157,   547,   174,
     549,   174,  1043,   801,   149,   756,   174,  1043,   153,   779,
     125,   126,  1388,   155,   174,   800,   151,   149,  1489,   859,
     149,   153,   663,   664,   153,   666,   149,   162,   163,   670,
     153,   149,   673,  1409,   174,   151,   320,   149,   156,   491,
       3,    58,   148,     3,   496,   157,   698,  1335,  1758,   155,
     549,   143,   144,   145,   169,   170,    73,   698,   174,   146,
     890,   513,   600,   155,    81,   152,    12,    13,    14,    15,
      16,   523,  1314,   165,   549,   845,   151,    94,   577,   152,
      97,   156,   174,   621,   101,   155,   173,  1515,   543,  1517,
      70,   155,  1157,  1585,   549,  1587,    12,    13,    14,    15,
      16,   157,   743,   151,   745,  1815,   149,   155,   151,   518,
     174,   160,  1540,  1823,   766,   149,   757,   883,   167,   168,
     151,   152,  1410,   764,    70,   766,   149,   144,   151,   859,
     153,   157,   149,    12,    13,    14,    15,    16,     3,   780,
     781,   782,   149,   912,   129,   162,  1522,    12,    13,    14,
      15,    16,   152,   153,    70,  1191,   156,   173,   442,   800,
    1870,  1029,   179,   180,   149,   149,   171,   151,   153,   153,
     821,   143,   144,   145,   149,   160,   161,   194,  1181,   155,
     151,   157,   634,   155,   155,   723,   115,   639,   173,   151,
     642,    70,   151,   165,   153,   836,   837,   838,   838,   149,
     217,   218,   174,  1579,  1034,    70,   151,    46,    47,   661,
      49,   151,   963,   151,    53,   131,   151,   234,   151,     3,
    1256,   838,   155,   912,   151,   838,   243,   850,    12,    13,
      14,    15,    16,   999,  1000,   677,   678,   679,  1168,   256,
     129,   740,   883,   149,   149,   151,   887,   153,     9,   149,
     151,   892,   131,   153,   155,   149,   273,   898,  1062,   276,
     149,    21,   546,   129,   153,    12,    13,    14,    15,    16,
     151,   160,   161,  1043,   155,   151,   293,   294,   129,   155,
     149,  1032,   151,   149,   153,   155,    70,   153,    97,   838,
     931,   149,   151,   149,   160,   161,   155,   938,   149,   108,
     155,    56,   153,   155,    59,    60,  1299,    62,   151,   160,
     161,  1173,   155,   851,   149,   155,  1570,   155,   153,   603,
     151,  1697,    96,    70,   155,   149,   967,   149,   149,   970,
     149,   153,   153,  1761,   151,    17,   148,  1373,   155,   838,
     101,   150,  1195,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   993,   151,   806,   157,   149,   155,   999,  1000,
     153,   104,   105,   838,    87,   817,  1151,  1882,   820,  1158,
    1159,  1886,   824,    55,    56,    57,    58,    59,    60,    61,
      62,  1809,   129,   838,  1083,   194,   157,   151,  1391,   150,
      70,   155,   157,  1034,   151,   850,  1037,  1825,   838,   416,
     151,   151,   149,   151,   155,   155,   153,   155,  1784,   151,
     850,   151,   151,   160,   161,   155,   155,   162,   163,  1287,
     154,   101,  1191,   440,   104,   105,   106,   107,   108,   109,
     110,   111,   112,  1861,  1164,   157,   151,   151,   157,  1269,
     155,   155,  1696,   460,   151,   151,   151,   256,   155,   155,
     155,  1091,  1093,   470,   151,  1096,  1097,  1098,   155,   151,
     151,   151,   479,   155,   155,   155,  1257,   123,   124,   149,
     150,   157,  1305,   173,  1091,  1164,  1227,   286,  1091,  1120,
    1120,   115,   499,   292,   149,  1126,  1256,  1257,   505,   684,
     685,   686,   687,  1134,   127,   128,  1137,  1138,  1137,  1138,
    1141,   149,  1191,  1120,  1480,  1481,  1482,  1120,   149,   838,
    1151,   320,  1080,  1081,  1082,  1138,   154,   155,   154,   155,
    1271,   850,   166,  1137,  1138,   155,   543,   158,   545,   118,
     158,   120,   121,   122,  1776,   161,  1177,   160,   161,   556,
     159,   558,  1091,   154,   155,   129,   830,   154,   155,   154,
     155,  1192,   154,   155,   838,   155,   156,   841,   575,   171,
     149,   154,   155,   152,   153,   152,  1602,  1208,   157,   158,
     151,  1120,   151,  1214,   154,   155,   151,   861,   151,  1305,
     154,   155,    12,    13,    14,    15,    16,   154,   155,  1138,
     607,   151,  1091,   154,   155,   151,  1295,  1296,  1297,   153,
    1299,  1300,   154,   101,  1373,  1246,   149,  1515,   106,   107,
     108,   109,   110,   111,   112,   131,  1091,   154,   155,  1157,
     149,  1120,  1263,   154,   155,  1077,  1164,   131,  1083,   154,
     155,   154,  1540,   442,   154,   155,  1091,   151,  1090,   151,
      70,   154,   155,   154,   155,  1120,   663,   664,   151,   666,
    1493,  1091,   150,   670,  1487,  1107,   673,   154,   155,  1460,
    1461,   151,  1495,    17,  1305,  1120,   151,  1493,  1309,   153,
    1493,  1312,   154,   155,   151,  1298,   157,  1493,   157,  1493,
    1120,   698,  1493,   157,  1373,   155,   156,    68,  1493,   154,
     155,  1332,   154,   155,   155,   156,   228,   506,  1138,   129,
     108,   109,   110,   111,   112,    59,    60,    61,    62,  1460,
    1461,  1352,   157,  1354,   154,  1354,   149,   526,    76,   149,
      75,    76,   154,   153,  1577,    17,   743,   173,   745,   155,
     160,   161,   155,   156,   149,  1505,   157,   546,  1215,  1216,
    1354,  1539,    12,    13,    14,    15,    16,   101,   151,   766,
     680,   681,   106,   107,   108,   109,   110,   111,   112,   113,
    1551,  1487,  1091,   780,   781,   782,   151,  1408,   174,  1495,
     579,   157,  1413,  1396,   682,   683,   174,  1418,  1419,  1420,
    1500,  1501,  1551,   800,    12,    13,    14,    15,    16,    17,
     157,  1120,  1493,  1636,   603,    17,   150,   688,   689,   153,
      70,   610,  1158,  1159,   154,   154,  1639,  1091,  1137,  1138,
    1636,   156,  1603,  1636,   156,   156,   155,   151,   151,   836,
    1636,   151,  1636,   151,   151,  1636,   151,   151,   151,   148,
     151,  1636,  1602,  1603,    68,   157,  1120,   157,   174,   157,
    1295,  1296,  1297,  1298,  1299,  1300,  1487,   173,   151,   151,
     151,   148,  1493,  1137,  1495,   151,   151,   157,  1298,   129,
     151,  1502,  1551,  1315,  1316,   155,   883,    12,    13,    14,
      15,    16,    17,  1514,   151,  1516,   155,   409,   151,   149,
    1678,   148,   151,   153,  1168,   151,   151,   151,   151,   151,
     160,   161,   151,   425,   151,   151,   428,   151,  1515,  1829,
    1517,  1353,   360,  1688,   151,   151,  1739,  1548,   151,   154,
     151,   151,   173,  1639,   931,   151,    63,    64,    65,    66,
     151,   938,   148,  1540,   149,   155,   149,   385,   386,  1570,
     149,  1554,     4,     5,     6,     7,     8,     9,    10,    11,
    1741,  1396,   149,   475,  1585,   149,  1587,   149,   406,    13,
     967,  1489,    72,   970,   101,   156,  1396,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1515,   155,  1517,  1298,
      89,   174,   156,   154,  1772,   174,   993,   154,   436,   174,
     174,   148,   999,  1000,    12,    13,    14,    15,    16,    17,
    1741,  1540,   148,   157,   155,  1636,   174,   151,  1639,   154,
     151,   174,  1793,   150,   148,   155,   153,   155,  1649,   151,
     151,   151,   148,  1739,   149,   174,  1515,  1034,  1517,   149,
    1037,   830,    78,  1793,   171,  1354,   151,   151,   174,   151,
    1671,   174,   841,   149,  1877,   174,   174,  1660,  1768,   148,
    1515,  1540,  1517,  1828,   149,  1878,  1687,  1688,   174,   148,
     148,  1877,   861,   174,  1877,  1696,  1857,  1858,   155,   174,
    1515,  1877,  1517,  1877,   155,  1540,  1877,  1396,   154,  1768,
    1354,   154,  1877,   154,   151,  1515,   154,  1517,  1911,  1531,
     148,   157,   151,  1884,  1882,  1540,   156,   118,  1886,  1887,
      12,    13,    14,    15,    16,    17,  1826,   156,  1739,  1554,
    1540,   148,   151,   151,  1745,   151,  1857,  1858,  1749,   151,
     155,  1660,   151,   154,  1554,  1913,   148,  1134,   156,  1760,
    1137,  1138,   174,   149,   149,   149,   107,  1826,   148,   151,
     157,   154,  1773,  1884,  1151,   154,  1934,   154,   148,   151,
    1938,   154,   151,   151,  1761,   151,   151,   154,   151,   148,
       1,   155,  1878,     4,   149,  1956,   174,   151,  1909,   151,
      88,   154,   148,   154,   148,  1963,   151,   154,   151,   151,
    1877,  1877,   151,  1877,   151,  1192,  1877,   151,   153,   538,
     156,   148,   151,   715,   716,  1911,  1515,  1828,  1517,  1830,
     156,  1208,  1809,   725,   174,   151,   728,  1214,   151,   151,
    1841,   174,  1843,  1844,   174,  1956,   174,    58,  1825,   174,
     155,  1540,  1761,   174,   152,   174,   156,   101,   676,   149,
    1660,   155,   149,    73,   148,  1554,   174,   150,   154,  1246,
      81,  1515,   150,  1517,   107,   174,  1877,  1878,   107,  1878,
     165,   151,   156,   151,  1861,   165,  1263,  1888,   148,   148,
     101,    73,   149,   151,  1577,   787,  1540,  1898,   174,   791,
    1809,   151,  1761,   795,   151,   174,   374,   174,     1,  1221,
    1911,     4,  1911,   651,   690,   692,  1825,   691,  1120,  1109,
     693,  1926,   694,  1924,  1540,  1926,  1761,   138,  1305,  1858,
    1670,  1761,  1874,   144,  1818,   146,  1921,  1777,  1920,  1908,
    1941,  1532,  1532,  1826,  1938,  1946,  1761,  1887,  1825,  1141,
    1809,    48,  1861,   248,  1739,  1332,  1487,  1802,  1300,  1960,
    1501,  1761,  1034,  1964,  1134,    58,  1825,   178,  1137,   773,
    1659,  1660,   854,  1974,  1809,  1396,   571,  1354,     0,   190,
      73,  1554,   715,   898,   715,   715,    -1,    -1,    81,    -1,
    1825,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,  1168,
      -1,    94,  1861,    -1,    97,    -1,    -1,    -1,   101,  1809,
    1825,    -1,    -1,    -1,    -1,  1659,    -1,    -1,    -1,    -1,
      -1,    -1,  1093,    -1,    -1,  1825,  1861,    -1,    -1,   240,
      -1,  1408,   243,    -1,    12,    -1,  1413,   248,    -1,    -1,
      -1,  1418,  1419,  1420,    -1,   138,  1861,    -1,    -1,    -1,
      -1,   144,    -1,   146,    -1,    -1,   149,   150,    -1,    -1,
      -1,  1861,  1751,    -1,    -1,    -1,    -1,    -1,   161,    -1,
     281,    -1,  1761,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,   976,   178,   179,   180,   807,   300,
     908,    -1,    -1,    -1,    -1,   913,    -1,   190,   191,    -1,
      -1,   194,    -1,    -1,    -1,    -1,   924,  1751,    86,    -1,
    1487,    -1,    -1,    -1,    -1,    -1,  1493,  1761,  1495,    -1,
    1809,  1933,    -1,   101,   217,   218,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    63,  1825,  1306,    -1,    -1,
    1952,   234,    -1,    -1,    -1,   864,    -1,    -1,    -1,    -1,
     243,    -1,    -1,    -1,    -1,    -1,  1048,    -1,    -1,  1051,
      -1,   372,    -1,   256,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,   191,  1861,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1825,    -1,    -1,    -1,  1354,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   286,    -1,   406,    -1,    -1,    -1,   292,
     293,   294,    -1,    -1,    -1,    -1,    -1,   300,  1585,    -1,
    1587,    -1,    73,    -1,    -1,    -1,    -1,  1861,    -1,    -1,
      -1,   432,    -1,    -1,    -1,   318,   319,   320,    -1,   101,
      -1,    -1,    -1,    94,   106,   107,   108,   109,   110,   111,
     112,   113,   335,    -1,    -1,   117,   339,   119,    -1,    -1,
      -1,    -1,    -1,   972,    -1,    -1,    -1,    -1,    -1,  1636,
      -1,    -1,  1639,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     989,   990,    -1,  1091,    76,    -1,    -1,    -1,   150,   372,
      -1,  1352,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
     501,   502,    -1,    -1,    -1,    -1,    -1,     9,   318,   101,
      -1,   321,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1688,    -1,   406,    -1,   335,   409,    -1,    -1,   339,
     101,    -1,    -1,   416,    -1,   106,   107,   108,   109,   110,
     111,   112,   543,    -1,    -1,    -1,   547,    -1,   549,   432,
      -1,  1233,    -1,   436,   807,    -1,  1164,   440,   150,   442,
    1242,    -1,    -1,    -1,    -1,    -1,   217,   218,    -1,    -1,
      -1,    -1,  1739,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,   174,   234,    -1,    -1,    -1,   470,    -1,    -1,
    1198,  1199,  1200,    -1,    -1,    -1,   479,  1205,  1206,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   864,    -1,    -1,    -1,    -1,   499,    -1,   501,   502,
     621,    -1,   505,    -1,   507,    -1,   436,    -1,    -1,    -1,
      -1,    -1,    -1,   634,    -1,   636,   637,    -1,   639,    -1,
      -1,   642,   293,   294,   645,   646,   647,    -1,   150,    -1,
      -1,    -1,    -1,  1514,    -1,  1516,    -1,    -1,    -1,    -1,
     101,  1828,   545,  1830,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   556,   101,   558,   117,   560,   119,   106,
     107,   108,   109,   110,   111,   112,   113,  1548,    -1,    -1,
     117,    -1,   119,   576,   577,    -1,   579,   507,    -1,    -1,
    1659,    -1,    -1,    -1,   587,    -1,    -1,    -1,   591,   150,
    1877,  1878,   153,    -1,    -1,    -1,    -1,   600,    -1,   972,
      -1,    -1,   723,   150,    -1,    -1,   153,   610,  1237,  1238,
     613,    -1,    -1,    -1,    -1,    -1,   989,   990,   621,    -1,
      -1,    -1,  1251,  1252,  1911,    -1,    -1,    -1,    -1,    -1,
     560,   634,    -1,   636,   637,    -1,   639,    -1,    -1,   642,
      -1,    -1,   645,   646,   647,    -1,   576,   577,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1285,  1286,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,  1649,   440,
     600,    -1,  1751,    -1,    -1,    -1,    -1,  1964,    -1,    -1,
      -1,  1483,    -1,   613,    -1,    -1,    -1,  1974,    -1,    -1,
    1671,    -1,    -1,    -1,    -1,   101,   699,     3,    -1,   470,
     106,   107,   108,   109,   110,   111,   112,    -1,   479,    -1,
      -1,    -1,   715,   716,    -1,    -1,    -1,   838,    -1,    -1,
     723,    -1,    -1,   129,    -1,    -1,    -1,    -1,   499,   850,
     851,    -1,    -1,    -1,   505,    -1,    -1,   740,    -1,    -1,
     743,    -1,   745,   149,   150,    -1,    -1,    -1,    -1,    -1,
    1829,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,   762,
     763,    -1,    -1,    -1,  1745,    -1,    -1,    -1,  1749,   699,
      -1,    -1,    -1,    -1,   545,    -1,   779,    -1,   781,  1760,
      -1,    -1,    -1,    -1,    -1,   556,    -1,   558,    -1,    -1,
      -1,    -1,  1773,    -1,    -1,   101,    -1,   800,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
     740,    -1,    -1,    -1,    -1,    -1,    -1,  1545,    -1,  1621,
    1622,   101,    -1,   129,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   838,    -1,    -1,    -1,    -1,
    1469,  1470,   845,   149,   150,    -1,    70,   850,   851,    -1,
     156,    -1,    -1,    -1,   160,   161,   859,    -1,   861,    -1,
    1841,    -1,  1843,  1844,  1237,  1238,    -1,    -1,    -1,   872,
     150,    -1,    -1,    -1,    -1,    -1,  1505,   101,  1251,  1252,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     101,    -1,    -1,   372,   174,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   129,    -1,  1888,    -1,   912,
      -1,    -1,  1285,  1286,    -1,    -1,    -1,  1898,   129,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,  1730,   859,
      -1,    -1,   862,  1562,    -1,    -1,   160,   161,   149,   150,
      -1,    -1,   153,  1924,    -1,  1926,    -1,    -1,    -1,   160,
     161,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1941,    -1,  1083,  1592,    -1,  1946,    -1,    -1,  1597,  1598,
    1091,    -1,   743,   976,   745,    -1,    -1,    -1,    -1,  1960,
      -1,    -1,   912,    -1,    -1,    89,    -1,    -1,    -1,    -1,
     993,    -1,    -1,    -1,    -1,    -1,  1798,  1000,    -1,  1120,
    1802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     781,    -1,    -1,    -1,    -1,    -1,    -1,  1138,    -1,    -1,
      -1,    -1,   501,   502,    -1,    -1,  1828,    -1,    70,   800,
      -1,    -1,    -1,    -1,    -1,   139,  1157,    -1,   142,    -1,
    1043,    -1,    -1,    -1,    -1,    -1,    -1,  1775,    -1,    -1,
      -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,  1195,  1877,  1878,    -1,    -1,    -1,
    1083,    -1,    -1,    -1,   101,    -1,    -1,   129,  1091,   106,
     107,   108,   109,   110,   111,   112,  1469,  1470,    -1,    -1,
      -1,    -1,    -1,    -1,   208,    -1,    -1,   149,   150,  1911,
      -1,   153,    -1,    -1,    -1,    -1,    -1,  1120,   160,   161,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   150,  1137,  1138,   153,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1151,   253,
      -1,    -1,    -1,    -1,  1157,   634,    -1,    -1,    -1,   263,
     639,  1164,    -1,   642,    -1,    -1,    -1,    -1,    -1,   150,
     274,    -1,    -1,    -1,  1295,  1296,  1297,  1298,  1299,  1300,
      -1,    -1,   661,    12,    13,    14,    15,    16,  1191,  1562,
      -1,    -1,  1195,   174,    -1,    -1,    -1,   301,    -1,    -1,
      -1,    -1,    -1,   307,   308,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   695,    -1,    -1,  1592,
      -1,    -1,   993,    -1,  1597,  1598,    -1,    -1,   999,  1000,
      -1,    -1,    -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,
     344,    70,    -1,    -1,    -1,   349,    -1,    -1,   352,    -1,
      -1,    -1,    -1,  1256,  1257,    -1,    -1,    -1,    -1,    -1,
      -1,  1191,    -1,    -1,    -1,    -1,  1895,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,  1396,    -1,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1295,  1296,  1297,  1298,  1299,  1300,    -1,    -1,
     129,    -1,  1305,  1306,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,   101,    -1,   439,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,   451,   452,   101,
      -1,  1354,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    58,    -1,    -1,    -1,    -1,    -1,    -1,  1489,    -1,
    1373,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1151,    -1,   150,    -1,    81,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,  1396,  1515,    -1,  1517,    -1,   150,    -1,
      97,   153,   101,    -1,   101,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   101,    -1,  1540,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     129,    -1,    -1,  1554,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   138,    -1,  1373,    -1,    -1,    -1,   144,    -1,   146,
     149,   150,    -1,   150,   153,    -1,  1577,    -1,    -1,    -1,
      -1,   160,   161,   160,   161,   162,   150,   571,    12,    13,
      14,    15,    16,   157,    -1,  1246,    -1,    -1,    -1,    -1,
    1483,   178,    -1,    -1,  1487,    -1,  1489,    -1,    -1,    -1,
    1493,    -1,  1495,   190,   191,   101,    -1,   194,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
      -1,   117,  1515,   119,  1517,   619,    -1,    -1,    -1,    -1,
      -1,    -1,  1895,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1305,    -1,    -1,  1540,    -1,  1660,
      -1,    -1,    -1,   240,   150,    -1,   243,   153,  1551,    -1,
      -1,  1554,    12,    13,    14,    15,    16,   101,    -1,   256,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,  1577,    -1,   273,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   281,   129,    -1,    -1,    -1,   286,
      -1,    -1,    -1,    -1,    -1,   292,    -1,    -1,    -1,  1602,
    1603,    -1,    -1,   300,    -1,   149,   150,    -1,    -1,   153,
      70,    -1,    -1,    -1,    -1,    -1,   160,   161,  1621,  1622,
      -1,   318,    -1,   320,   321,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1636,    -1,    -1,  1639,    -1,   335,    -1,
    1761,   101,   339,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   757,    -1,    -1,  1659,  1660,    -1,    -1,
     764,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,   372,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,  1688,    -1,    -1,  1809,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,  1825,    -1,    -1,    -1,    -1,   406,
      -1,    -1,    -1,    -1,    -1,    -1,  1487,    -1,    -1,    -1,
      -1,    -1,  1493,    -1,  1495,    -1,    -1,  1730,    -1,    -1,
      -1,    -1,    -1,   837,    -1,   432,  1739,    70,    -1,   436,
    1861,    -1,    -1,    -1,    -1,   442,   101,    -1,  1751,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1761,    -1,
      -1,    -1,    -1,    -1,    -1,  1768,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,   887,    -1,    -1,    -1,    -1,   892,    -1,
    1793,    -1,    -1,    -1,    -1,   150,   129,   152,    -1,  1802,
      -1,    -1,   157,    -1,   501,   502,  1809,    -1,    -1,   506,
     507,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
      -1,    -1,  1825,  1826,   101,  1828,  1829,   160,   161,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,  1768,    -1,
      -1,   538,  1321,    -1,    -1,    -1,   543,    -1,    -1,   546,
     547,  1330,   549,    -1,    -1,  1334,    -1,  1336,  1861,    -1,
      -1,    -1,    -1,   560,    -1,  1636,    -1,    -1,  1639,    -1,
      -1,    -1,    -1,   150,  1877,  1878,   153,    -1,   575,   576,
     577,    -1,   579,    12,    13,    14,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1826,    -1,    -1,    -1,
      -1,    -1,    -1,   600,    -1,    -1,   603,    -1,  1911,    -1,
     607,    -1,    -1,   610,    -1,    -1,   613,  1688,   615,    -1,
      -1,    -1,   101,    -1,   621,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   634,    -1,   636,
     637,    70,   639,    -1,    -1,   642,    -1,    -1,   645,   646,
     647,    -1,    -1,     1,    -1,   101,     4,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,  1739,    -1,
      -1,   150,   101,    -1,   153,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1096,  1097,  1098,    -1,  1475,    -1,    -1,    -1,
     129,    -1,   699,    -1,   150,    -1,    -1,    -1,    -1,    -1,
      58,   157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,  1126,    -1,    -1,    -1,   723,    -1,    -1,    -1,
      -1,   160,   161,    81,    -1,    -1,  1515,  1141,  1517,    -1,
      -1,    -1,    -1,   740,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,  1828,    -1,    -1,
      -1,  1540,    -1,    -1,    -1,   762,   763,    -1,    -1,    -1,
      -1,    -1,    -1,  1177,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     138,    -1,    -1,    -1,    -1,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1877,  1878,    -1,    -1,
     807,    -1,   101,   161,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,    -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,
    1911,   838,   190,   191,   841,    -1,    -1,    -1,   845,    -1,
      -1,    -1,    -1,   850,   851,    -1,    -1,    -1,    -1,    -1,
      -1,   150,   859,   152,   861,   862,    -1,   864,    -1,    -1,
     218,    -1,  1651,    -1,    -1,    -1,  1655,    -1,    -1,    -1,
      -1,    -1,    -1,  1662,    -1,    -1,   234,    -1,    -1,    -1,
      -1,   239,   240,   101,    -1,   243,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1309,    -1,    -1,  1312,    -1,
      -1,    -1,    -1,    -1,    -1,   912,    -1,   265,     5,    -1,
     268,   129,   270,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    -1,   281,   177,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,   294,    -1,   156,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
     318,    -1,    -1,   321,    -1,   972,    -1,    -1,    -1,  1758,
    1759,    -1,  1761,    70,    71,    -1,    -1,   335,    -1,    -1,
      -1,   339,   989,   990,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,  1817,    -1,
      -1,    -1,   129,    -1,   149,    -1,  1043,   152,   153,    -1,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,    -1,   149,    -1,   146,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,   432,    -1,  1083,    -1,   436,    -1,
    1869,   173,  1871,    -1,  1091,    70,   101,    -1,  1502,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,  1891,    -1,   357,    -1,    -1,   360,   361,    -1,
      -1,    -1,    -1,  1120,   129,    -1,   101,   370,   371,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
    1137,  1138,   385,   386,   149,   150,    -1,    -1,  1927,  1928,
    1929,    -1,    -1,    -1,   129,   160,   161,    -1,    -1,   507,
    1157,    -1,    -1,   406,    -1,    -1,  1570,  1164,    -1,    -1,
      -1,  1168,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1180,    -1,   160,   161,    -1,    -1,    -1,
     538,    -1,    -1,   436,  1191,   543,    -1,    -1,  1195,   547,
     101,   549,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   560,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,     5,    -1,    -1,   576,   577,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
    1237,  1238,    -1,   591,    -1,    -1,    -1,    -1,    -1,   150,
      -1,   152,   600,    -1,  1251,  1252,    -1,   605,    -1,  1256,
    1257,   149,   150,    -1,    -1,   613,    -1,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1687,    -1,    -1,    -1,    -1,  1285,  1286,
      70,    71,  1696,    -1,    -1,    -1,    -1,    -1,  1295,  1296,
    1297,  1298,  1299,  1300,    -1,    -1,    -1,    -1,    -1,  1306,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,   699,    -1,    -1,    -1,   101,    -1,  1354,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   716,   149,
      -1,    -1,   152,   153,    58,    -1,  1373,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,   740,    -1,    -1,    -1,    -1,   745,    -1,  1396,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   762,   763,   101,   101,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,   779,    -1,   676,   677,   678,   679,   680,   681,   682,
     683,   684,   685,   686,   687,   688,   689,   690,   691,   692,
     693,   694,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   807,
     144,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,    -1,
      -1,    -1,  1469,  1470,    -1,    -1,    -1,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
     838,    -1,  1489,    -1,    -1,    -1,    -1,   845,    -1,    -1,
      -1,    -1,   850,    -1,    -1,    -1,    -1,   191,  1505,    -1,
      -1,   859,    -1,   756,   862,    -1,   864,     1,  1515,   101,
    1517,   869,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1540,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,  1551,    -1,    -1,  1554,    -1,    -1,
      -1,    -1,    -1,    -1,   912,  1562,    -1,   149,   150,    -1,
      -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,   160,   161,
    1577,   265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1592,    -1,   281,    -1,    -1,
    1597,  1598,    -1,    -1,    -1,  1602,  1603,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,    -1,   972,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   318,    -1,    -1,   321,    -1,    -1,
      -1,   989,   990,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   335,    -1,    -1,    -1,   339,    -1,    -1,    -1,    -1,
     144,    -1,  1659,  1660,    -1,   908,    -1,    -1,    -1,    -1,
     913,    -1,    -1,    -1,    -1,    -1,    -1,   161,    -1,    -1,
      -1,   924,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,  1043,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,   101,    -1,
     129,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     963,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,    -1,   153,  1083,   129,    -1,    -1,    -1,
      -1,   160,   161,  1091,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   436,    -1,  1751,    -1,   149,   150,    -1,    -1,
     153,    -1,    -1,    -1,  1761,    -1,    -1,   160,   161,    -1,
      -1,  1768,  1120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     173,    81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1138,    -1,    -1,    -1,    -1,    -1,  1793,   281,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1809,    -1,    -1,    -1,  1164,    -1,    -1,    -1,
      -1,    -1,    -1,   507,    -1,  1173,    -1,    -1,  1825,  1826,
      -1,    -1,  1829,    -1,   318,    -1,    -1,   321,    -1,    -1,
      -1,    -1,    -1,  1191,    -1,    -1,   146,    -1,  1091,    -1,
      -1,   335,    -1,    -1,   538,   339,    -1,    -1,    -1,    -1,
      -1,   161,    -1,   547,  1861,   549,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   560,    -1,   178,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1237,
    1238,   191,   576,   577,    -1,    -1,    -1,    -1,  1895,    -1,
      -1,    -1,    -1,  1251,  1252,    -1,    -1,    -1,  1256,  1257,
      -1,    -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,
      -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   613,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1285,  1286,    -1,
      -1,    -1,    -1,   243,    -1,    -1,    -1,  1295,  1296,  1297,
    1298,    -1,   436,    -1,    -1,  1198,  1199,  1200,    -1,    -1,
      -1,   101,  1205,  1206,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1227,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     300,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,   699,    -1,    -1,   318,    -1,
     160,   161,    -1,   507,    -1,  1373,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1396,    -1,
      -1,    -1,    -1,    -1,   538,    -1,   740,    -1,    -1,    -1,
      -1,    -1,    -1,   547,    -1,   549,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   560,    -1,   762,   763,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   576,   577,    -1,   779,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   406,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,
      -1,  1469,  1470,   807,    -1,    -1,    -1,    -1,    -1,   613,
      -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1495,    -1,    -1,
      -1,    -1,    -1,    58,   838,    -1,    -1,  1505,    -1,    -1,
      -1,   845,    -1,    -1,    -1,    -1,    -1,  1515,    -1,  1517,
      -1,    -1,    -1,    -1,    -1,   859,    81,    -1,   862,    -1,
     864,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1540,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,   501,   502,  1551,    -1,    -1,  1554,   507,    -1,    -1,
      -1,    -1,    -1,    -1,  1562,   699,    -1,  1460,  1461,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   912,    -1,
      -1,    -1,    -1,   138,    -1,    -1,    -1,    -1,    -1,   144,
      -1,   146,    -1,    -1,  1592,    -1,    -1,    -1,    -1,  1597,
    1598,    -1,    -1,    -1,  1602,  1603,   740,    -1,    -1,    -1,
     560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   178,  1622,    -1,    -1,   577,   762,   763,
      -1,    -1,    -1,    -1,    -1,   190,    -1,    -1,   972,    -1,
      -1,    -1,    -1,    -1,    -1,   779,    -1,    -1,    -1,    -1,
     600,    -1,  1545,    -1,    -1,   989,   990,    -1,    -1,    -1,
      -1,    -1,  1660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   621,    -1,   807,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   240,    -1,    -1,   243,    -1,
      -1,    -1,    -1,   248,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,   838,    -1,    -1,    -1,    -1,  1043,
      -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   859,   281,    -1,   862,    -1,
     864,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   300,    -1,    -1,    -1,   699,
      -1,    -1,    -1,    58,    -1,    -1,    -1,  1091,    -1,    -1,
      -1,    -1,    -1,  1761,    -1,    -1,    -1,    -1,    -1,    -1,
    1768,    -1,    -1,   723,    -1,    -1,    81,    -1,   912,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,    -1,
     740,    -1,    -1,    -1,    -1,  1793,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1138,    -1,    -1,    -1,    -1,    -1,
      -1,  1809,   762,   763,    -1,    -1,    -1,   372,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1825,  1826,   779,
    1164,    -1,    -1,   138,    -1,    -1,    -1,    -1,   972,   144,
      -1,   146,    -1,    -1,    -1,    -1,    -1,    -1,  1741,    -1,
      -1,   406,    -1,    -1,    -1,   989,   990,  1191,    -1,    -1,
      -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   178,    -1,    -1,    -1,   432,    -1,    -1,
    1878,    -1,  1775,    -1,    -1,   190,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   845,    -1,  1895,    -1,    -1,
      -1,   851,    -1,  1237,  1238,    -1,    -1,    -1,    -1,  1043,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,    -1,
      -1,    -1,  1256,  1257,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   240,    -1,    -1,   243,    -1,
      -1,    -1,    -1,   248,    -1,    -1,   501,   502,    -1,    -1,
      -1,  1285,  1286,    -1,    -1,    -1,    -1,  1091,    -1,    -1,
      -1,    -1,   912,    -1,  1857,  1858,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   281,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1120,    -1,   543,    -1,
      -1,  1884,   547,    -1,   549,   300,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1138,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1909,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1373,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1191,    -1,    -1,
      -1,    -1,    -1,  1956,    -1,    -1,   621,   372,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   634,
      -1,   636,   637,    -1,   639,    -1,    -1,   642,    -1,    -1,
     645,   646,   647,  1043,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   406,    -1,  1237,  1238,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,    -1,
      -1,    -1,  1256,  1257,    -1,    -1,    -1,   432,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1469,  1470,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1285,  1286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   723,    -1,
      -1,  1505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1515,    -1,  1517,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   501,   502,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1540,  1157,    -1,    -1,
      -1,    -1,    -1,    -1,  1164,    -1,    -1,  1551,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1562,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   543,  1373,
      -1,  1191,   547,    -1,   549,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1592,    -1,
      -1,    -1,    -1,  1597,  1598,    -1,    -1,    -1,  1602,  1603,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    -1,    -1,   838,    -1,    -1,    91,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   850,   851,    -1,     0,    48,
      -1,     3,    -1,    52,    -1,    54,  1256,  1257,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   621,    -1,    -1,    -1,
     125,    -1,    71,    -1,    -1,    -1,  1660,    -1,    -1,   634,
      -1,   636,   637,    -1,   639,  1469,  1470,   642,    -1,    -1,
     645,   646,   647,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,  1505,   121,   122,    76,    -1,    -1,    -1,    -1,    -1,
     129,  1515,    -1,  1517,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,  1540,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,  1551,   723,    -1,
      -1,    -1,    -1,  1373,   173,   174,    -1,  1761,  1562,    -1,
      -1,    -1,   134,    -1,  1768,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1592,  1793,
      -1,    -1,    -1,  1597,  1598,    -1,    -1,    -1,  1602,  1603,
      -1,    -1,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   290,    -1,    -1,    -1,    -1,
      -1,  1825,  1826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   227,  1660,  1861,  1083,    -1,
      -1,    -1,    -1,   838,    -1,    -1,  1091,    -1,    -1,  1489,
     242,    -1,    -1,    -1,    -1,   850,   851,    -1,    -1,    -1,
     252,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     262,  1895,    -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   276,   277,    -1,    -1,    -1,    -1,
      -1,   283,   284,  1138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   299,    -1,    -1,
      -1,  1551,  1157,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1761,    -1,    -1,
     435,    -1,   437,    -1,  1768,    -1,    -1,    -1,    -1,    -1,
    1195,   446,   447,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1602,  1603,    -1,    -1,    -1,    -1,    -1,  1793,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   373,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1825,  1826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   404,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1861,   430,    -1,
      -1,    -1,   434,    -1,    -1,    -1,    -1,    47,    -1,   544,
    1295,  1296,  1297,  1298,  1299,  1300,    -1,    -1,    -1,    -1,
      -1,   453,    -1,    -1,    -1,   457,   458,    -1,    -1,    -1,
      -1,  1895,    -1,    73,   466,   467,   468,   469,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1083,    -1,
      -1,    -1,    -1,   485,    -1,    -1,  1091,    -1,    -1,    -1,
      -1,   493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,  1120,    -1,    -1,    -1,   521,
      -1,   131,    -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1138,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1396,    -1,  1793,    -1,    -1,    -1,    -1,    -1,    -1,
     552,    -1,  1157,   163,    -1,    -1,    -1,   559,    -1,    -1,
      -1,    -1,   564,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   587,   588,    -1,    -1,    -1,
    1195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   217,    -1,    -1,
      -1,   221,    -1,    -1,   224,   225,    -1,    -1,   228,    -1,
      -1,   231,   232,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1489,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   648,    -1,    -1,    -1,
      -1,   756,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1515,    -1,  1517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   293,    -1,  1540,   296,    -1,    -1,    -1,
    1295,  1296,  1297,  1298,  1299,  1300,    -1,    -1,    47,  1554,
      -1,    -1,    -1,    -1,    -1,    -1,   316,    -1,    -1,    -1,
      -1,    -1,    -1,   715,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   331,  1577,    -1,    -1,    -1,   831,   832,   730,    -1,
      -1,    -1,   734,    -1,    -1,    -1,    -1,   842,   843,   844,
      -1,   743,   847,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,    -1,   118,
      -1,    -1,    -1,    -1,    -1,    -1,   778,    -1,    -1,    -1,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,    -1,
      -1,  1396,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   815,    -1,   425,    -1,    -1,    -1,    -1,
     822,   926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     470,    -1,    -1,    -1,   969,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   482,    -1,    -1,   224,   225,    -1,    -1,   228,
      -1,    -1,   231,   232,  1489,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     902,    -1,    -1,    -1,    -1,    -1,  1761,    -1,    -1,    -1,
    1515,  1016,  1517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1025,  1026,  1027,  1028,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1540,    -1,    -1,    -1,  1044,
      -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,    -1,  1554,
      -1,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,    -1,
    1065,    -1,  1067,    -1,    -1,    -1,    -1,   316,    -1,    -1,
    1825,    -1,  1577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     982,    -1,   331,    -1,   986,   595,   596,    -1,    -1,    -1,
      -1,   993,    -1,    -1,    -1,    -1,    -1,    -1,   608,    -1,
      -1,  1003,    -1,    -1,    -1,    -1,  1861,    -1,  1010,    -1,
      -1,    -1,    -1,    -1,    -1,  1120,    -1,  1019,    -1,  1021,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1030,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1038,    -1,    -1,    -1,
      -1,  1146,    -1,    -1,    -1,    -1,    -1,  1152,    -1,  1154,
    1155,    -1,  1054,    -1,    -1,  1660,  1058,    -1,  1163,    -1,
    1165,    -1,  1167,    -1,  1169,    -1,    -1,    -1,    -1,  1174,
    1072,    -1,    -1,  1075,    -1,    -1,   425,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   719,
     720,    -1,    -1,    -1,    -1,   725,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1232,    -1,    -1,
      -1,    -1,    -1,   482,  1239,  1240,   746,    -1,    -1,   749,
     750,    -1,   752,    -1,   754,   755,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1761,    -1,    -1,    -1,
    1162,  1266,  1267,    -1,    -1,    -1,    -1,    -1,    -1,  1274,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,
      -1,   791,    -1,    -1,  1186,   795,    -1,    -1,    -1,    -1,
     108,    -1,   110,    -1,   112,    -1,    -1,    -1,    -1,  1304,
      -1,    -1,    -1,    -1,  1809,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,   152,   153,    -1,    -1,    -1,  1344,
      -1,    -1,    -1,    -1,    -1,    -1,   595,   596,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,   608,
     870,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1377,    -1,    -1,  1277,   194,    -1,    -1,  1281,
    1385,    -1,  1387,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1311,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1324,  1325,    -1,    -1,    -1,    -1,  1433,  1434,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   256,    -1,
     258,   259,    -1,  1448,  1449,    -1,  1451,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1459,    -1,    -1,    -1,    -1,  1361,
    1465,    -1,  1364,    -1,    -1,    -1,  1471,  1472,   286,    -1,
     719,   720,    -1,    -1,   292,    -1,   725,  1379,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   999,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   746,    -1,    -1,
     749,   750,   320,   752,    -1,   754,   755,    -1,   326,    -1,
     328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1429,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1438,    -1,  1048,    -1,
    1442,  1051,   791,    -1,    -1,    -1,   795,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1456,  1457,    -1,    -1,  1563,  1564,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,    -1,
    1605,    -1,    -1,    -1,    -1,    -1,    -1,  1612,  1613,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,   191,
      -1,   870,    -1,    -1,   442,    -1,   444,   445,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   221,
      -1,    -1,    -1,  1555,  1556,    -1,   228,    -1,    -1,    -1,
    1170,   479,    -1,    -1,    -1,    -1,    -1,    -1,  1178,  1179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   499,    -1,    -1,    -1,  1690,   504,    -1,   506,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1708,    -1,    -1,  1711,  1712,   526,    -1,
     528,   529,    -1,  1718,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1233,   296,    -1,    -1,    -1,   546,    -1,
      -1,    -1,  1242,    -1,    -1,  1245,    -1,  1247,  1248,    -1,
     558,    -1,    -1,    -1,    -1,    -1,   318,   319,    -1,    -1,
     999,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   579,    -1,   581,   582,    -1,    -1,   339,    -1,    -1,
      -1,    -1,    -1,    -1,  1676,    -1,    -1,    -1,  1288,    -1,
      -1,    -1,    -1,    -1,    -1,   603,   604,    -1,    -1,    -1,
      -1,    -1,   610,    -1,    -1,    -1,  1698,    -1,    -1,  1048,
      -1,    -1,  1051,    -1,    -1,    -1,    -1,    -1,    -1,  1814,
      -1,    -1,    -1,    -1,  1716,    -1,  1821,    -1,    -1,    -1,
      -1,    -1,  1827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   409,    -1,    -1,
      -1,  1743,    -1,    -1,    -1,  1355,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   425,   426,    -1,   428,   429,    -1,    -1,
      -1,    -1,  1764,    -1,   436,  1767,    -1,    -1,   440,    -1,
    1875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1899,    -1,    -1,    -1,    -1,   471,
      -1,    -1,  1907,   475,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1922,    -1,    -1,
      -1,  1170,    -1,    -1,    -1,    -1,    -1,  1437,    -1,  1178,
    1179,    -1,    -1,    -1,    -1,   507,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1852,    -1,    -1,    -1,    -1,    -1,    -1,  1467,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1493,  1233,   557,    -1,    -1,   560,  1499,
      -1,    -1,    -1,  1242,    -1,    -1,  1245,    -1,  1247,  1248,
      -1,    -1,    -1,    -1,   576,   577,    -1,    -1,    -1,    -1,
      -1,    -1,   830,    -1,    -1,   587,    -1,    -1,    -1,   591,
      -1,    -1,    -1,   841,    -1,    -1,   598,    -1,   600,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1288,
    1550,    -1,    -1,   861,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   872,    12,    13,    14,    15,    16,
      -1,    -1,    19,   881,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1355,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,  1628,  1629,
      -1,    -1,    -1,    -1,    -1,    -1,  1636,   699,    -1,    -1,
    1640,    -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,    -1,
      -1,    -1,    -1,   715,   716,    -1,    -1,   104,   105,    -1,
      -1,    -1,    97,   725,   726,    -1,   728,   729,   976,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   740,    -1,
      -1,   743,   129,   745,   746,    -1,    -1,    -1,    -1,    -1,
     752,    -1,  1000,    -1,    -1,    -1,    -1,    -1,  1437,    -1,
     762,   763,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,   146,    -1,   160,   161,   150,    -1,   779,    -1,    -1,
      -1,   783,    -1,    -1,    -1,   787,   161,    -1,  1467,   791,
     792,    -1,  1732,   795,   796,    -1,    -1,    -1,    -1,    -1,
      -1,   803,    -1,   178,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,   194,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   845,   846,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1798,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   243,    -1,
     872,  1550,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   256,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     912,    -1,    -1,    -1,    -1,   178,    -1,   292,    -1,    -1,
    1168,    -1,    -1,    -1,    -1,   300,  1174,    -1,   191,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1877,    -1,    -1,
      -1,    -1,   205,   318,    -1,   320,    -1,    -1,    -1,  1628,
    1629,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   976,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   993,   994,    -1,    -1,    -1,    -1,   372,  1000,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   291,    48,
      -1,   406,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,  1043,    -1,    -1,    -1,    -1,  1048,  1049,    -1,  1051,
    1052,    -1,    71,  1732,    -1,    -1,    -1,    -1,  1306,    -1,
      -1,   436,    -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,  1354,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1798,
      -1,    -1,    -1,    -1,    -1,    -1,   501,   502,    -1,    -1,
     149,   150,   507,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,  1170,  1171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,    -1,  1191,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   460,    -1,    -1,
     463,    -1,   577,    -1,   579,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,    -1,
      -1,  1233,  1234,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1242,  1243,    -1,  1245,    -1,    -1,   621,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1256,  1257,    -1,    -1,    -1,   634,
      -1,   636,   637,    -1,   639,    -1,    -1,   642,    -1,    -1,
     645,   646,   647,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    19,   550,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,   577,    -1,    -1,    50,    51,    -1,
      -1,    -1,    -1,    -1,   699,    -1,    -1,   590,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   723,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   740,    -1,    -1,    -1,    -1,
      -1,  1373,    -1,    -1,    -1,    -1,    -1,    -1,   641,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,   763,    -1,
      -1,  1639,    -1,    -1,    -1,    -1,   659,   660,    -1,    -1,
      -1,    -1,    -1,    -1,   779,    -1,   669,    -1,   671,   672,
      -1,  1659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   712,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,
     723,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     845,    -1,    -1,    -1,   737,    -1,   851,   740,    71,    -1,
      -1,  1483,  1730,    -1,    -1,    -1,   861,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,    -1,
      -1,    -1,    -1,  1751,    -1,    98,    99,    -1,   101,   772,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,   912,    -1,    -1,
      -1,   804,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1551,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,   851,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,   862,
    1602,  1603,    -1,    -1,    -1,    -1,   869,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1621,
    1622,    -1,    -1,    -1,    -1,    -1,    -1,   890,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1637,   899,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   912,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   920,    -1,    -1,
      -1,    -1,    -1,  1911,   927,    -1,    -1,    -1,  1043,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   970,  1083,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,  1730,    -1,
      -1,    70,    -1,    -1,    -1,    -1,  1738,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1137,    -1,    -1,   104,   105,    -1,    -1,  1032,
      -1,  1034,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1042,
      -1,    -1,  1157,    -1,    -1,    -1,    -1,    -1,    -1,  1164,
     129,  1793,    -1,    -1,    -1,    -1,  1798,  1799,    -1,    -1,
    1802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,   152,   153,    -1,  1191,    -1,    -1,    -1,
    1195,   160,   161,    -1,    -1,    -1,  1828,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1108,  1109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1877,  1878,    -1,    -1,    -1,
      -1,  1256,  1257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1168,    -1,    -1,    -1,  1911,
      -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1295,  1296,  1297,    -1,  1299,  1300,    -1,    -1,  1191,    -1,
      48,  1306,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1208,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,  1221,    -1,
      -1,  1224,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1354,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,  1373,   117,
     118,   119,    -1,   121,   122,    -1,  1269,    -1,  1271,    -1,
      -1,   129,    -1,  1276,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,  1314,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,  1332,
      -1,    19,  1335,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
    1373,    -1,    -1,    -1,  1489,    -1,    -1,    -1,    -1,    -1,
    1383,  1384,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1408,    -1,  1410,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1551,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1577,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1489,  1602,  1603,    -1,
      -1,  1494,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,  1536,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1659,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,  1575,    -1,    19,  1578,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1751,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1768,    -1,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1793,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,  1826,   157,    -1,  1829,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,  1776,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    71,    72,    -1,    74,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    -1,    96,    -1,    98,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      48,    -1,    50,    51,    52,    -1,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    67,
      -1,    69,    70,    71,    72,    -1,    74,    -1,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    -1,    96,    -1,
      98,    99,   100,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,     3,     4,     5,
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
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    67,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    -1,   100,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    67,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
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
      -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,     3,
      -1,    -1,    -1,   160,   161,     9,    -1,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
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
      -1,    17,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    48,    -1,   160,   161,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    71,    -1,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    17,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    69,
      -1,    71,    72,    -1,    74,   160,   161,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    -1,    96,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,   174,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,
      -1,    -1,    74,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    -1,    96,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   174,     4,     5,     6,     7,     8,     9,    10,
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
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   152,   153,    -1,    -1,   104,   105,    -1,    -1,   160,
     161,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   152,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    12,    13,    14,    15,    16,    17,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,   104,   105,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,   104,   105,    -1,    -1,   160,   161,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,   152,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      11,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
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
     161,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    48,    -1,    50,
      51,    52,    -1,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
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
      -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,    -1,
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
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,   152,   153,    -1,    12,    13,
      14,    15,    16,   160,   161,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,   150,    19,   152,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,   152,
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
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,    -1,
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
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
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
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165
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
     364,   366,   370,   372,   373,   315,    55,   153,   194,   289,
     293,   297,   298,   304,   305,   311,   312,   313,   314,   318,
     325,   326,   342,   351,   353,   437,   449,   450,   451,   452,
     457,   458,   104,   105,   157,   180,   315,   428,   401,   149,
     371,   372,   149,   149,   115,   181,   182,    48,    52,    54,
      71,    98,    99,   101,   103,   113,   114,   117,   118,   119,
     121,   122,   149,   153,   159,   162,   163,   164,   165,   178,
     179,   181,   183,   186,   193,   194,   195,   196,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   217,   315,   151,   153,   193,   194,   210,   212,   290,
     315,   356,   357,   374,   453,   458,   293,   411,   412,   413,
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
     151,   155,   355,   373,   151,   399,   154,   176,   291,   293,
     305,   312,   314,   448,   449,   457,   458,   149,   153,   161,
     173,   194,   437,   438,   439,   440,   441,   442,   443,   460,
     194,   318,   457,   293,   312,   299,   294,   399,   151,   291,
     293,   450,   291,   437,   450,     9,   343,   355,   340,   157,
     364,   173,   364,    12,    86,   101,   104,   105,   179,   402,
     403,   404,   151,   115,   149,   193,   149,   149,   196,   149,
     193,   149,   149,   193,   193,    18,    20,    83,   153,   162,
     163,   197,   198,   212,   219,   223,   328,   356,   457,   155,
     176,   149,   183,   158,   158,   118,   120,   121,   122,   149,
     152,   153,   157,   158,   196,   196,   166,   160,   167,   168,
     162,   163,   123,   124,   125,   126,   169,   170,   127,   128,
     161,   159,   171,   129,   130,   172,   151,   155,   152,   176,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   173,   214,   215,   216,   149,   194,   432,   433,   434,
     435,   436,   151,   155,   151,   151,   151,   151,   151,   151,
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
     176,   149,   368,   366,   367,    76,   303,   180,   291,   437,
     450,   293,   297,   457,   176,   439,   440,   441,   154,   176,
      17,   212,   293,   438,   460,   399,   399,   437,   291,   448,
     458,   293,   180,   399,   291,   450,   315,   155,   459,   173,
     215,   344,   157,   343,   151,   357,   151,   151,   155,   149,
     174,   356,   153,   356,   356,   356,   212,   356,   151,   356,
     356,   356,   176,   151,   162,   163,   198,    17,   295,   151,
     155,   151,   160,   161,   151,   218,   212,   157,   180,   180,
     113,   153,   180,   150,   187,   188,   212,   113,   153,   180,
     328,   212,   187,   180,   196,   199,   199,   199,   200,   200,
     201,   201,   202,   202,   202,   202,   203,   203,   204,   205,
     206,   207,   208,   156,   219,   174,   181,   153,   180,   212,
     157,   212,   176,   433,   434,   435,   293,   432,   399,   399,
     212,   357,   149,   399,   436,   437,   149,   436,   437,   176,
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
     291,   450,   153,   160,   194,   212,   315,   212,   293,   348,
     151,   151,   151,     5,   293,   399,   438,   157,   180,   428,
       9,   355,   148,   359,   343,   459,   157,   151,   403,   187,
     151,   176,   155,   151,   151,   155,   151,   196,   151,   151,
     151,   196,    17,   295,   212,   151,   151,   150,   157,   196,
     154,   177,   187,   113,   117,   119,   180,   189,   190,   191,
     151,   155,   189,   154,   155,   148,   210,   156,   151,   189,
     177,   360,   348,   151,   151,   151,   432,   176,   176,   348,
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
     357,   371,   371,   176,   177,   177,   177,   212,   177,   149,
     399,   442,   437,   292,     5,   160,   177,   212,   343,   399,
     399,   315,   344,   459,   148,   148,   176,   151,   180,    76,
     184,   185,   356,   196,   196,   196,   196,   196,   157,   360,
     155,   148,   192,   153,   190,   192,   192,   154,   155,   120,
     152,   188,   154,   218,   210,   174,   154,   459,   177,   149,
     399,   436,   437,   348,   348,   177,   177,   151,   149,   399,
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
     151,   176,   148,   148,   176,   155,   155,   154,   154,   154,
     177,   151,   176,   212,   212,   177,   154,   177,   459,   341,
     157,   344,   148,   379,   151,   156,   151,   155,   156,   360,
     459,   218,   118,   189,   190,   153,   190,   153,   190,   154,
     148,   151,   176,   177,   177,   151,   151,   176,   176,   177,
     177,   177,   176,   176,   177,   211,   211,   154,   154,   177,
     151,   399,   348,   348,   177,   177,   219,   148,   324,   324,
     324,   149,   194,   333,   334,   436,   444,   445,   446,   447,
     174,   155,   174,   331,   174,   374,   400,   405,   212,   293,
     155,   174,   337,   338,   337,   355,   131,   352,   353,   151,
     151,   149,   221,   219,   230,   275,   277,   280,   286,   293,
     297,   221,   173,   174,   219,   239,   240,   275,   174,   459,
     151,   151,   151,   223,   256,   257,   149,   212,   149,   181,
     230,   196,   249,   107,   221,   399,   380,   176,   176,   154,
     348,   177,   177,   154,   154,   148,   157,   343,   177,   212,
     185,   212,   459,   148,   154,   154,   189,   189,   348,   151,
     151,   348,   348,   151,   151,   154,   155,   131,   347,   131,
     154,   177,   177,   177,   151,   151,   154,   445,   446,   447,
     293,   444,   155,   174,   399,   399,   174,   151,   405,   399,
     221,    75,    76,   157,   233,   234,   235,   151,   219,   151,
     219,   293,   219,   220,   143,   144,   145,   165,   174,   241,
     151,   156,   220,   148,   157,   235,   221,   149,   176,   174,
     181,   151,   156,   151,   151,   155,   156,   247,   251,   355,
     396,   177,   154,   154,   343,   459,   148,   148,   154,   154,
     177,   177,   177,   176,   177,   154,   151,   151,   151,   151,
     151,   444,   399,   332,   211,   231,   232,   397,   156,   176,
     221,   233,   174,   151,   221,   174,   104,   173,   219,   220,
     219,   221,   240,   174,   174,   176,   176,   258,   291,   293,
     453,   156,   174,   153,   181,   263,   264,   265,   221,   196,
     187,    73,   106,   248,   250,   151,   459,   148,   151,   151,
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
#line 529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 6756 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6762 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6768 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6774 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6780 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6786 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6792 "Parser/parser.cc"
    break;

  case 18:
#line 561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6798 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6804 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6810 "Parser/parser.cc"
    break;

  case 21:
#line 571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6820 "Parser/parser.cc"
    break;

  case 22:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6826 "Parser/parser.cc"
    break;

  case 23:
#line 584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6832 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6838 "Parser/parser.cc"
    break;

  case 26:
#line 591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6844 "Parser/parser.cc"
    break;

  case 27:
#line 593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6850 "Parser/parser.cc"
    break;

  case 28:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6856 "Parser/parser.cc"
    break;

  case 29:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6862 "Parser/parser.cc"
    break;

  case 30:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6872 "Parser/parser.cc"
    break;

  case 32:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6883 "Parser/parser.cc"
    break;

  case 33:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6892 "Parser/parser.cc"
    break;

  case 34:
#line 628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6898 "Parser/parser.cc"
    break;

  case 36:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6904 "Parser/parser.cc"
    break;

  case 37:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6910 "Parser/parser.cc"
    break;

  case 38:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6920 "Parser/parser.cc"
    break;

  case 39:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6926 "Parser/parser.cc"
    break;

  case 40:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6932 "Parser/parser.cc"
    break;

  case 41:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6938 "Parser/parser.cc"
    break;

  case 42:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6944 "Parser/parser.cc"
    break;

  case 43:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6950 "Parser/parser.cc"
    break;

  case 44:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6956 "Parser/parser.cc"
    break;

  case 45:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 6962 "Parser/parser.cc"
    break;

  case 46:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6968 "Parser/parser.cc"
    break;

  case 47:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 6974 "Parser/parser.cc"
    break;

  case 48:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6980 "Parser/parser.cc"
    break;

  case 49:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6986 "Parser/parser.cc"
    break;

  case 50:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6992 "Parser/parser.cc"
    break;

  case 51:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 6998 "Parser/parser.cc"
    break;

  case 52:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7004 "Parser/parser.cc"
    break;

  case 53:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7010 "Parser/parser.cc"
    break;

  case 54:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7016 "Parser/parser.cc"
    break;

  case 55:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7026 "Parser/parser.cc"
    break;

  case 56:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7032 "Parser/parser.cc"
    break;

  case 58:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7038 "Parser/parser.cc"
    break;

  case 59:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7044 "Parser/parser.cc"
    break;

  case 62:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7050 "Parser/parser.cc"
    break;

  case 64:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7056 "Parser/parser.cc"
    break;

  case 65:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7062 "Parser/parser.cc"
    break;

  case 66:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7068 "Parser/parser.cc"
    break;

  case 67:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7074 "Parser/parser.cc"
    break;

  case 68:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7080 "Parser/parser.cc"
    break;

  case 69:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7086 "Parser/parser.cc"
    break;

  case 70:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7092 "Parser/parser.cc"
    break;

  case 71:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7098 "Parser/parser.cc"
    break;

  case 72:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7106 "Parser/parser.cc"
    break;

  case 73:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7112 "Parser/parser.cc"
    break;

  case 74:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7121 "Parser/parser.cc"
    break;

  case 77:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7127 "Parser/parser.cc"
    break;

  case 78:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7133 "Parser/parser.cc"
    break;

  case 79:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7153 "Parser/parser.cc"
    break;

  case 80:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7159 "Parser/parser.cc"
    break;

  case 81:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7165 "Parser/parser.cc"
    break;

  case 82:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7171 "Parser/parser.cc"
    break;

  case 83:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7177 "Parser/parser.cc"
    break;

  case 84:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7183 "Parser/parser.cc"
    break;

  case 85:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7189 "Parser/parser.cc"
    break;

  case 86:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7195 "Parser/parser.cc"
    break;

  case 87:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7201 "Parser/parser.cc"
    break;

  case 88:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7210 "Parser/parser.cc"
    break;

  case 89:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7216 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7222 "Parser/parser.cc"
    break;

  case 91:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7228 "Parser/parser.cc"
    break;

  case 92:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7234 "Parser/parser.cc"
    break;

  case 93:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7240 "Parser/parser.cc"
    break;

  case 94:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7246 "Parser/parser.cc"
    break;

  case 95:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7252 "Parser/parser.cc"
    break;

  case 97:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7258 "Parser/parser.cc"
    break;

  case 98:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7264 "Parser/parser.cc"
    break;

  case 99:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7270 "Parser/parser.cc"
    break;

  case 100:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7276 "Parser/parser.cc"
    break;

  case 101:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7282 "Parser/parser.cc"
    break;

  case 102:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7288 "Parser/parser.cc"
    break;

  case 103:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7294 "Parser/parser.cc"
    break;

  case 104:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7300 "Parser/parser.cc"
    break;

  case 112:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7306 "Parser/parser.cc"
    break;

  case 114:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7312 "Parser/parser.cc"
    break;

  case 115:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7318 "Parser/parser.cc"
    break;

  case 116:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7324 "Parser/parser.cc"
    break;

  case 118:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7330 "Parser/parser.cc"
    break;

  case 119:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7336 "Parser/parser.cc"
    break;

  case 121:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7342 "Parser/parser.cc"
    break;

  case 122:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7348 "Parser/parser.cc"
    break;

  case 124:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7354 "Parser/parser.cc"
    break;

  case 125:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7360 "Parser/parser.cc"
    break;

  case 126:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7366 "Parser/parser.cc"
    break;

  case 127:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7372 "Parser/parser.cc"
    break;

  case 129:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7378 "Parser/parser.cc"
    break;

  case 130:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7384 "Parser/parser.cc"
    break;

  case 132:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7390 "Parser/parser.cc"
    break;

  case 134:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7396 "Parser/parser.cc"
    break;

  case 136:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7402 "Parser/parser.cc"
    break;

  case 138:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7408 "Parser/parser.cc"
    break;

  case 140:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7414 "Parser/parser.cc"
    break;

  case 142:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7420 "Parser/parser.cc"
    break;

  case 143:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7426 "Parser/parser.cc"
    break;

  case 146:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7438 "Parser/parser.cc"
    break;

  case 147:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7444 "Parser/parser.cc"
    break;

  case 148:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7450 "Parser/parser.cc"
    break;

  case 152:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7456 "Parser/parser.cc"
    break;

  case 153:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7462 "Parser/parser.cc"
    break;

  case 154:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7468 "Parser/parser.cc"
    break;

  case 155:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7474 "Parser/parser.cc"
    break;

  case 156:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7480 "Parser/parser.cc"
    break;

  case 157:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7486 "Parser/parser.cc"
    break;

  case 158:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7492 "Parser/parser.cc"
    break;

  case 159:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7498 "Parser/parser.cc"
    break;

  case 160:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7504 "Parser/parser.cc"
    break;

  case 161:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7510 "Parser/parser.cc"
    break;

  case 162:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7516 "Parser/parser.cc"
    break;

  case 163:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7522 "Parser/parser.cc"
    break;

  case 164:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7528 "Parser/parser.cc"
    break;

  case 165:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7534 "Parser/parser.cc"
    break;

  case 166:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7540 "Parser/parser.cc"
    break;

  case 168:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7546 "Parser/parser.cc"
    break;

  case 169:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7552 "Parser/parser.cc"
    break;

  case 170:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7558 "Parser/parser.cc"
    break;

  case 172:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7564 "Parser/parser.cc"
    break;

  case 173:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7570 "Parser/parser.cc"
    break;

  case 185:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7576 "Parser/parser.cc"
    break;

  case 187:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7582 "Parser/parser.cc"
    break;

  case 188:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7588 "Parser/parser.cc"
    break;

  case 189:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7594 "Parser/parser.cc"
    break;

  case 190:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7600 "Parser/parser.cc"
    break;

  case 192:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7606 "Parser/parser.cc"
    break;

  case 193:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7612 "Parser/parser.cc"
    break;

  case 194:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7618 "Parser/parser.cc"
    break;

  case 195:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 196:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 199:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7636 "Parser/parser.cc"
    break;

  case 200:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7642 "Parser/parser.cc"
    break;

  case 201:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7648 "Parser/parser.cc"
    break;

  case 202:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7654 "Parser/parser.cc"
    break;

  case 203:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7668 "Parser/parser.cc"
    break;

  case 204:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7674 "Parser/parser.cc"
    break;

  case 205:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7683 "Parser/parser.cc"
    break;

  case 206:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7689 "Parser/parser.cc"
    break;

  case 207:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7695 "Parser/parser.cc"
    break;

  case 208:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7701 "Parser/parser.cc"
    break;

  case 209:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7707 "Parser/parser.cc"
    break;

  case 210:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7713 "Parser/parser.cc"
    break;

  case 211:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7719 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7725 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7731 "Parser/parser.cc"
    break;

  case 215:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7737 "Parser/parser.cc"
    break;

  case 216:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7743 "Parser/parser.cc"
    break;

  case 217:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7749 "Parser/parser.cc"
    break;

  case 218:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7755 "Parser/parser.cc"
    break;

  case 220:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7761 "Parser/parser.cc"
    break;

  case 221:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7767 "Parser/parser.cc"
    break;

  case 222:
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7773 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7779 "Parser/parser.cc"
    break;

  case 225:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7785 "Parser/parser.cc"
    break;

  case 226:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7791 "Parser/parser.cc"
    break;

  case 227:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7797 "Parser/parser.cc"
    break;

  case 228:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7803 "Parser/parser.cc"
    break;

  case 229:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7809 "Parser/parser.cc"
    break;

  case 230:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7815 "Parser/parser.cc"
    break;

  case 231:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7821 "Parser/parser.cc"
    break;

  case 233:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7840 "Parser/parser.cc"
    break;

  case 234:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7846 "Parser/parser.cc"
    break;

  case 235:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7852 "Parser/parser.cc"
    break;

  case 236:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7858 "Parser/parser.cc"
    break;

  case 237:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7865 "Parser/parser.cc"
    break;

  case 238:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7872 "Parser/parser.cc"
    break;

  case 239:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7878 "Parser/parser.cc"
    break;

  case 240:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7884 "Parser/parser.cc"
    break;

  case 241:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7890 "Parser/parser.cc"
    break;

  case 242:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7897 "Parser/parser.cc"
    break;

  case 243:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7904 "Parser/parser.cc"
    break;

  case 244:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7910 "Parser/parser.cc"
    break;

  case 245:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7916 "Parser/parser.cc"
    break;

  case 246:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7925 "Parser/parser.cc"
    break;

  case 247:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7931 "Parser/parser.cc"
    break;

  case 248:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7937 "Parser/parser.cc"
    break;

  case 249:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7943 "Parser/parser.cc"
    break;

  case 250:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 7949 "Parser/parser.cc"
    break;

  case 251:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 7955 "Parser/parser.cc"
    break;

  case 252:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 7961 "Parser/parser.cc"
    break;

  case 253:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 7967 "Parser/parser.cc"
    break;

  case 254:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 7973 "Parser/parser.cc"
    break;

  case 255:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 7979 "Parser/parser.cc"
    break;

  case 256:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 7985 "Parser/parser.cc"
    break;

  case 257:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 7991 "Parser/parser.cc"
    break;

  case 258:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 7997 "Parser/parser.cc"
    break;

  case 259:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8003 "Parser/parser.cc"
    break;

  case 260:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8009 "Parser/parser.cc"
    break;

  case 261:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8015 "Parser/parser.cc"
    break;

  case 262:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8021 "Parser/parser.cc"
    break;

  case 263:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8027 "Parser/parser.cc"
    break;

  case 264:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8033 "Parser/parser.cc"
    break;

  case 265:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8039 "Parser/parser.cc"
    break;

  case 266:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8045 "Parser/parser.cc"
    break;

  case 267:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8051 "Parser/parser.cc"
    break;

  case 268:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8057 "Parser/parser.cc"
    break;

  case 269:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8063 "Parser/parser.cc"
    break;

  case 270:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8069 "Parser/parser.cc"
    break;

  case 271:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8075 "Parser/parser.cc"
    break;

  case 272:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8081 "Parser/parser.cc"
    break;

  case 273:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8087 "Parser/parser.cc"
    break;

  case 274:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8093 "Parser/parser.cc"
    break;

  case 275:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8099 "Parser/parser.cc"
    break;

  case 278:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 8107 "Parser/parser.cc"
    break;

  case 279:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8113 "Parser/parser.cc"
    break;

  case 280:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8119 "Parser/parser.cc"
    break;

  case 281:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8125 "Parser/parser.cc"
    break;

  case 283:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8131 "Parser/parser.cc"
    break;

  case 284:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8137 "Parser/parser.cc"
    break;

  case 286:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8143 "Parser/parser.cc"
    break;

  case 287:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8149 "Parser/parser.cc"
    break;

  case 288:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8155 "Parser/parser.cc"
    break;

  case 289:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 290:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8167 "Parser/parser.cc"
    break;

  case 291:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8173 "Parser/parser.cc"
    break;

  case 292:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8179 "Parser/parser.cc"
    break;

  case 293:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8185 "Parser/parser.cc"
    break;

  case 294:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8191 "Parser/parser.cc"
    break;

  case 295:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8197 "Parser/parser.cc"
    break;

  case 296:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8203 "Parser/parser.cc"
    break;

  case 297:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 298:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 299:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8221 "Parser/parser.cc"
    break;

  case 300:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8227 "Parser/parser.cc"
    break;

  case 301:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8233 "Parser/parser.cc"
    break;

  case 302:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8239 "Parser/parser.cc"
    break;

  case 303:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8245 "Parser/parser.cc"
    break;

  case 304:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8251 "Parser/parser.cc"
    break;

  case 305:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8257 "Parser/parser.cc"
    break;

  case 306:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8263 "Parser/parser.cc"
    break;

  case 307:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8269 "Parser/parser.cc"
    break;

  case 309:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8275 "Parser/parser.cc"
    break;

  case 310:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8281 "Parser/parser.cc"
    break;

  case 311:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8287 "Parser/parser.cc"
    break;

  case 316:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8293 "Parser/parser.cc"
    break;

  case 317:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8299 "Parser/parser.cc"
    break;

  case 318:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8305 "Parser/parser.cc"
    break;

  case 319:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8311 "Parser/parser.cc"
    break;

  case 320:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8317 "Parser/parser.cc"
    break;

  case 321:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8323 "Parser/parser.cc"
    break;

  case 322:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8329 "Parser/parser.cc"
    break;

  case 323:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8335 "Parser/parser.cc"
    break;

  case 326:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8341 "Parser/parser.cc"
    break;

  case 327:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8347 "Parser/parser.cc"
    break;

  case 328:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8353 "Parser/parser.cc"
    break;

  case 329:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8359 "Parser/parser.cc"
    break;

  case 330:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8365 "Parser/parser.cc"
    break;

  case 331:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8371 "Parser/parser.cc"
    break;

  case 332:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8380 "Parser/parser.cc"
    break;

  case 333:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8389 "Parser/parser.cc"
    break;

  case 334:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8395 "Parser/parser.cc"
    break;

  case 337:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8401 "Parser/parser.cc"
    break;

  case 338:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8407 "Parser/parser.cc"
    break;

  case 340:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8413 "Parser/parser.cc"
    break;

  case 341:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8419 "Parser/parser.cc"
    break;

  case 351:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8425 "Parser/parser.cc"
    break;

  case 352:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8431 "Parser/parser.cc"
    break;

  case 356:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8437 "Parser/parser.cc"
    break;

  case 358:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8443 "Parser/parser.cc"
    break;

  case 359:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8449 "Parser/parser.cc"
    break;

  case 360:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8455 "Parser/parser.cc"
    break;

  case 361:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8461 "Parser/parser.cc"
    break;

  case 362:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8467 "Parser/parser.cc"
    break;

  case 363:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8473 "Parser/parser.cc"
    break;

  case 365:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8479 "Parser/parser.cc"
    break;

  case 366:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8485 "Parser/parser.cc"
    break;

  case 367:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8491 "Parser/parser.cc"
    break;

  case 368:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8502 "Parser/parser.cc"
    break;

  case 369:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8508 "Parser/parser.cc"
    break;

  case 370:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 371:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8520 "Parser/parser.cc"
    break;

  case 372:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8526 "Parser/parser.cc"
    break;

  case 373:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8535 "Parser/parser.cc"
    break;

  case 374:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8544 "Parser/parser.cc"
    break;

  case 375:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8553 "Parser/parser.cc"
    break;

  case 376:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8562 "Parser/parser.cc"
    break;

  case 377:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8571 "Parser/parser.cc"
    break;

  case 378:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8580 "Parser/parser.cc"
    break;

  case 379:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8589 "Parser/parser.cc"
    break;

  case 380:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8598 "Parser/parser.cc"
    break;

  case 381:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8606 "Parser/parser.cc"
    break;

  case 382:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8614 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8620 "Parser/parser.cc"
    break;

  case 387:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8626 "Parser/parser.cc"
    break;

  case 388:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8632 "Parser/parser.cc"
    break;

  case 401:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8638 "Parser/parser.cc"
    break;

  case 404:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8644 "Parser/parser.cc"
    break;

  case 407:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8650 "Parser/parser.cc"
    break;

  case 408:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8656 "Parser/parser.cc"
    break;

  case 409:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8662 "Parser/parser.cc"
    break;

  case 410:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8668 "Parser/parser.cc"
    break;

  case 412:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8674 "Parser/parser.cc"
    break;

  case 414:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8680 "Parser/parser.cc"
    break;

  case 415:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8686 "Parser/parser.cc"
    break;

  case 417:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8692 "Parser/parser.cc"
    break;

  case 418:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8698 "Parser/parser.cc"
    break;

  case 419:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8704 "Parser/parser.cc"
    break;

  case 420:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8710 "Parser/parser.cc"
    break;

  case 421:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8716 "Parser/parser.cc"
    break;

  case 422:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8722 "Parser/parser.cc"
    break;

  case 423:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8728 "Parser/parser.cc"
    break;

  case 424:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8734 "Parser/parser.cc"
    break;

  case 425:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8740 "Parser/parser.cc"
    break;

  case 426:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8746 "Parser/parser.cc"
    break;

  case 427:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8752 "Parser/parser.cc"
    break;

  case 428:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8758 "Parser/parser.cc"
    break;

  case 429:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8764 "Parser/parser.cc"
    break;

  case 430:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8770 "Parser/parser.cc"
    break;

  case 431:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8776 "Parser/parser.cc"
    break;

  case 432:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8782 "Parser/parser.cc"
    break;

  case 433:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8788 "Parser/parser.cc"
    break;

  case 434:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8794 "Parser/parser.cc"
    break;

  case 435:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8800 "Parser/parser.cc"
    break;

  case 436:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8806 "Parser/parser.cc"
    break;

  case 437:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8812 "Parser/parser.cc"
    break;

  case 438:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8818 "Parser/parser.cc"
    break;

  case 439:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8824 "Parser/parser.cc"
    break;

  case 440:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8830 "Parser/parser.cc"
    break;

  case 441:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8836 "Parser/parser.cc"
    break;

  case 442:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8842 "Parser/parser.cc"
    break;

  case 443:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8848 "Parser/parser.cc"
    break;

  case 444:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8854 "Parser/parser.cc"
    break;

  case 445:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8860 "Parser/parser.cc"
    break;

  case 446:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8866 "Parser/parser.cc"
    break;

  case 447:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8872 "Parser/parser.cc"
    break;

  case 448:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8878 "Parser/parser.cc"
    break;

  case 449:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8884 "Parser/parser.cc"
    break;

  case 450:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8890 "Parser/parser.cc"
    break;

  case 451:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8896 "Parser/parser.cc"
    break;

  case 452:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8902 "Parser/parser.cc"
    break;

  case 454:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8908 "Parser/parser.cc"
    break;

  case 456:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 8914 "Parser/parser.cc"
    break;

  case 457:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8920 "Parser/parser.cc"
    break;

  case 458:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8926 "Parser/parser.cc"
    break;

  case 460:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8932 "Parser/parser.cc"
    break;

  case 461:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 462:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8944 "Parser/parser.cc"
    break;

  case 463:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 8950 "Parser/parser.cc"
    break;

  case 465:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8956 "Parser/parser.cc"
    break;

  case 467:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8962 "Parser/parser.cc"
    break;

  case 468:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8968 "Parser/parser.cc"
    break;

  case 469:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 8974 "Parser/parser.cc"
    break;

  case 470:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 8980 "Parser/parser.cc"
    break;

  case 471:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 8986 "Parser/parser.cc"
    break;

  case 472:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 8992 "Parser/parser.cc"
    break;

  case 473:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 8998 "Parser/parser.cc"
    break;

  case 474:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9004 "Parser/parser.cc"
    break;

  case 475:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9010 "Parser/parser.cc"
    break;

  case 477:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9016 "Parser/parser.cc"
    break;

  case 478:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9022 "Parser/parser.cc"
    break;

  case 479:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9028 "Parser/parser.cc"
    break;

  case 481:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9034 "Parser/parser.cc"
    break;

  case 482:
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9040 "Parser/parser.cc"
    break;

  case 483:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9049 "Parser/parser.cc"
    break;

  case 485:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9055 "Parser/parser.cc"
    break;

  case 486:
#line 2010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9061 "Parser/parser.cc"
    break;

  case 487:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9067 "Parser/parser.cc"
    break;

  case 489:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9073 "Parser/parser.cc"
    break;

  case 490:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9079 "Parser/parser.cc"
    break;

  case 492:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9085 "Parser/parser.cc"
    break;

  case 493:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9091 "Parser/parser.cc"
    break;

  case 494:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9097 "Parser/parser.cc"
    break;

  case 496:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9103 "Parser/parser.cc"
    break;

  case 497:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9109 "Parser/parser.cc"
    break;

  case 498:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9115 "Parser/parser.cc"
    break;

  case 499:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9121 "Parser/parser.cc"
    break;

  case 500:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9127 "Parser/parser.cc"
    break;

  case 502:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9133 "Parser/parser.cc"
    break;

  case 503:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9139 "Parser/parser.cc"
    break;

  case 504:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9145 "Parser/parser.cc"
    break;

  case 505:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9151 "Parser/parser.cc"
    break;

  case 506:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9157 "Parser/parser.cc"
    break;

  case 511:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9163 "Parser/parser.cc"
    break;

  case 512:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9169 "Parser/parser.cc"
    break;

  case 513:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9178 "Parser/parser.cc"
    break;

  case 514:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9184 "Parser/parser.cc"
    break;

  case 515:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9193 "Parser/parser.cc"
    break;

  case 516:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9202 "Parser/parser.cc"
    break;

  case 517:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9211 "Parser/parser.cc"
    break;

  case 518:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9220 "Parser/parser.cc"
    break;

  case 520:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9226 "Parser/parser.cc"
    break;

  case 521:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9232 "Parser/parser.cc"
    break;

  case 522:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9242 "Parser/parser.cc"
    break;

  case 523:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9257 "Parser/parser.cc"
    break;

  case 526:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9263 "Parser/parser.cc"
    break;

  case 527:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9269 "Parser/parser.cc"
    break;

  case 528:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9275 "Parser/parser.cc"
    break;

  case 529:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9281 "Parser/parser.cc"
    break;

  case 530:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9287 "Parser/parser.cc"
    break;

  case 531:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9293 "Parser/parser.cc"
    break;

  case 532:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9299 "Parser/parser.cc"
    break;

  case 533:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9305 "Parser/parser.cc"
    break;

  case 534:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9311 "Parser/parser.cc"
    break;

  case 535:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9317 "Parser/parser.cc"
    break;

  case 536:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9323 "Parser/parser.cc"
    break;

  case 537:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9329 "Parser/parser.cc"
    break;

  case 538:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9335 "Parser/parser.cc"
    break;

  case 539:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9341 "Parser/parser.cc"
    break;

  case 540:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9347 "Parser/parser.cc"
    break;

  case 541:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9360 "Parser/parser.cc"
    break;

  case 542:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9366 "Parser/parser.cc"
    break;

  case 545:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9372 "Parser/parser.cc"
    break;

  case 546:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9378 "Parser/parser.cc"
    break;

  case 549:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9384 "Parser/parser.cc"
    break;

  case 551:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9390 "Parser/parser.cc"
    break;

  case 552:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9396 "Parser/parser.cc"
    break;

  case 553:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 554:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9408 "Parser/parser.cc"
    break;

  case 555:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9414 "Parser/parser.cc"
    break;

  case 557:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9420 "Parser/parser.cc"
    break;

  case 559:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 560:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 562:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9438 "Parser/parser.cc"
    break;

  case 563:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9444 "Parser/parser.cc"
    break;

  case 565:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9450 "Parser/parser.cc"
    break;

  case 566:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9456 "Parser/parser.cc"
    break;

  case 567:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9462 "Parser/parser.cc"
    break;

  case 568:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9468 "Parser/parser.cc"
    break;

  case 569:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9474 "Parser/parser.cc"
    break;

  case 570:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9483 "Parser/parser.cc"
    break;

  case 571:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9492 "Parser/parser.cc"
    break;

  case 572:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9500 "Parser/parser.cc"
    break;

  case 573:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9510 "Parser/parser.cc"
    break;

  case 575:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 576:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9522 "Parser/parser.cc"
    break;

  case 577:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9528 "Parser/parser.cc"
    break;

  case 578:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9534 "Parser/parser.cc"
    break;

  case 579:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9540 "Parser/parser.cc"
    break;

  case 580:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9546 "Parser/parser.cc"
    break;

  case 581:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9552 "Parser/parser.cc"
    break;

  case 582:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9558 "Parser/parser.cc"
    break;

  case 583:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9564 "Parser/parser.cc"
    break;

  case 584:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9570 "Parser/parser.cc"
    break;

  case 587:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9576 "Parser/parser.cc"
    break;

  case 588:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9582 "Parser/parser.cc"
    break;

  case 589:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9588 "Parser/parser.cc"
    break;

  case 591:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9594 "Parser/parser.cc"
    break;

  case 592:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9600 "Parser/parser.cc"
    break;

  case 593:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9606 "Parser/parser.cc"
    break;

  case 595:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9612 "Parser/parser.cc"
    break;

  case 596:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9618 "Parser/parser.cc"
    break;

  case 597:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9624 "Parser/parser.cc"
    break;

  case 599:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9630 "Parser/parser.cc"
    break;

  case 602:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9636 "Parser/parser.cc"
    break;

  case 603:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9642 "Parser/parser.cc"
    break;

  case 605:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9648 "Parser/parser.cc"
    break;

  case 606:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9654 "Parser/parser.cc"
    break;

  case 607:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9660 "Parser/parser.cc"
    break;

  case 612:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9666 "Parser/parser.cc"
    break;

  case 614:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9672 "Parser/parser.cc"
    break;

  case 615:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9678 "Parser/parser.cc"
    break;

  case 616:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9684 "Parser/parser.cc"
    break;

  case 617:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9690 "Parser/parser.cc"
    break;

  case 618:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9696 "Parser/parser.cc"
    break;

  case 619:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9702 "Parser/parser.cc"
    break;

  case 625:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9708 "Parser/parser.cc"
    break;

  case 628:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9714 "Parser/parser.cc"
    break;

  case 629:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9720 "Parser/parser.cc"
    break;

  case 630:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9726 "Parser/parser.cc"
    break;

  case 631:
#line 2450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9732 "Parser/parser.cc"
    break;

  case 632:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9738 "Parser/parser.cc"
    break;

  case 633:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9744 "Parser/parser.cc"
    break;

  case 635:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9750 "Parser/parser.cc"
    break;

  case 636:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9756 "Parser/parser.cc"
    break;

  case 637:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9762 "Parser/parser.cc"
    break;

  case 639:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9768 "Parser/parser.cc"
    break;

  case 641:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9774 "Parser/parser.cc"
    break;

  case 642:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9780 "Parser/parser.cc"
    break;

  case 643:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9786 "Parser/parser.cc"
    break;

  case 644:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9792 "Parser/parser.cc"
    break;

  case 645:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9798 "Parser/parser.cc"
    break;

  case 646:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9804 "Parser/parser.cc"
    break;

  case 648:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9810 "Parser/parser.cc"
    break;

  case 649:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9816 "Parser/parser.cc"
    break;

  case 650:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9822 "Parser/parser.cc"
    break;

  case 651:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9833 "Parser/parser.cc"
    break;

  case 652:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 653:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9845 "Parser/parser.cc"
    break;

  case 654:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9851 "Parser/parser.cc"
    break;

  case 655:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9860 "Parser/parser.cc"
    break;

  case 656:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9866 "Parser/parser.cc"
    break;

  case 657:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9872 "Parser/parser.cc"
    break;

  case 658:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9878 "Parser/parser.cc"
    break;

  case 659:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9884 "Parser/parser.cc"
    break;

  case 660:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9890 "Parser/parser.cc"
    break;

  case 661:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9896 "Parser/parser.cc"
    break;

  case 662:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9902 "Parser/parser.cc"
    break;

  case 663:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9908 "Parser/parser.cc"
    break;

  case 664:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9914 "Parser/parser.cc"
    break;

  case 665:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9920 "Parser/parser.cc"
    break;

  case 668:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9926 "Parser/parser.cc"
    break;

  case 669:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9932 "Parser/parser.cc"
    break;

  case 670:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9938 "Parser/parser.cc"
    break;

  case 671:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 9944 "Parser/parser.cc"
    break;

  case 673:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 9950 "Parser/parser.cc"
    break;

  case 674:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9956 "Parser/parser.cc"
    break;

  case 675:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9962 "Parser/parser.cc"
    break;

  case 676:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9968 "Parser/parser.cc"
    break;

  case 677:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 9974 "Parser/parser.cc"
    break;

  case 678:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 9980 "Parser/parser.cc"
    break;

  case 679:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 680:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 9995 "Parser/parser.cc"
    break;

  case 681:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10004 "Parser/parser.cc"
    break;

  case 682:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10010 "Parser/parser.cc"
    break;

  case 683:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10016 "Parser/parser.cc"
    break;

  case 685:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10022 "Parser/parser.cc"
    break;

  case 690:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 691:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10034 "Parser/parser.cc"
    break;

  case 692:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10040 "Parser/parser.cc"
    break;

  case 694:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10046 "Parser/parser.cc"
    break;

  case 695:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10052 "Parser/parser.cc"
    break;

  case 696:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10058 "Parser/parser.cc"
    break;

  case 697:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10064 "Parser/parser.cc"
    break;

  case 699:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10070 "Parser/parser.cc"
    break;

  case 700:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10076 "Parser/parser.cc"
    break;

  case 701:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10082 "Parser/parser.cc"
    break;

  case 704:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10091 "Parser/parser.cc"
    break;

  case 705:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10097 "Parser/parser.cc"
    break;

  case 706:
#line 2714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10106 "Parser/parser.cc"
    break;

  case 707:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10116 "Parser/parser.cc"
    break;

  case 708:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10125 "Parser/parser.cc"
    break;

  case 709:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10135 "Parser/parser.cc"
    break;

  case 710:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10144 "Parser/parser.cc"
    break;

  case 711:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10154 "Parser/parser.cc"
    break;

  case 712:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10163 "Parser/parser.cc"
    break;

  case 713:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10173 "Parser/parser.cc"
    break;

  case 715:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 716:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 717:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10191 "Parser/parser.cc"
    break;

  case 718:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10197 "Parser/parser.cc"
    break;

  case 719:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10208 "Parser/parser.cc"
    break;

  case 720:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10217 "Parser/parser.cc"
    break;

  case 721:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10226 "Parser/parser.cc"
    break;

  case 722:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10232 "Parser/parser.cc"
    break;

  case 723:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10238 "Parser/parser.cc"
    break;

  case 724:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10244 "Parser/parser.cc"
    break;

  case 725:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10253 "Parser/parser.cc"
    break;

  case 726:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10259 "Parser/parser.cc"
    break;

  case 727:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10265 "Parser/parser.cc"
    break;

  case 728:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10271 "Parser/parser.cc"
    break;

  case 732:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10277 "Parser/parser.cc"
    break;

  case 733:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10283 "Parser/parser.cc"
    break;

  case 734:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10293 "Parser/parser.cc"
    break;

  case 735:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10299 "Parser/parser.cc"
    break;

  case 738:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10305 "Parser/parser.cc"
    break;

  case 739:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10311 "Parser/parser.cc"
    break;

  case 741:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10317 "Parser/parser.cc"
    break;

  case 742:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10323 "Parser/parser.cc"
    break;

  case 743:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10329 "Parser/parser.cc"
    break;

  case 744:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10335 "Parser/parser.cc"
    break;

  case 749:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10341 "Parser/parser.cc"
    break;

  case 750:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10347 "Parser/parser.cc"
    break;

  case 751:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10353 "Parser/parser.cc"
    break;

  case 752:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10359 "Parser/parser.cc"
    break;

  case 753:
#line 2928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10365 "Parser/parser.cc"
    break;

  case 755:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10371 "Parser/parser.cc"
    break;

  case 756:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10377 "Parser/parser.cc"
    break;

  case 757:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10383 "Parser/parser.cc"
    break;

  case 758:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10389 "Parser/parser.cc"
    break;

  case 759:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10395 "Parser/parser.cc"
    break;

  case 760:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10401 "Parser/parser.cc"
    break;

  case 761:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10407 "Parser/parser.cc"
    break;

  case 762:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10413 "Parser/parser.cc"
    break;

  case 763:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10419 "Parser/parser.cc"
    break;

  case 764:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10425 "Parser/parser.cc"
    break;

  case 765:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10431 "Parser/parser.cc"
    break;

  case 766:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10437 "Parser/parser.cc"
    break;

  case 767:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10443 "Parser/parser.cc"
    break;

  case 768:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10449 "Parser/parser.cc"
    break;

  case 769:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10455 "Parser/parser.cc"
    break;

  case 770:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10461 "Parser/parser.cc"
    break;

  case 771:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10467 "Parser/parser.cc"
    break;

  case 772:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10473 "Parser/parser.cc"
    break;

  case 774:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10479 "Parser/parser.cc"
    break;

  case 775:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10485 "Parser/parser.cc"
    break;

  case 776:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10491 "Parser/parser.cc"
    break;

  case 777:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10497 "Parser/parser.cc"
    break;

  case 778:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10503 "Parser/parser.cc"
    break;

  case 779:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10509 "Parser/parser.cc"
    break;

  case 780:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10515 "Parser/parser.cc"
    break;

  case 781:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10521 "Parser/parser.cc"
    break;

  case 782:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10527 "Parser/parser.cc"
    break;

  case 783:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10533 "Parser/parser.cc"
    break;

  case 784:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10539 "Parser/parser.cc"
    break;

  case 785:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10545 "Parser/parser.cc"
    break;

  case 786:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10551 "Parser/parser.cc"
    break;

  case 787:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10557 "Parser/parser.cc"
    break;

  case 788:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10563 "Parser/parser.cc"
    break;

  case 789:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10569 "Parser/parser.cc"
    break;

  case 793:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10575 "Parser/parser.cc"
    break;

  case 794:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10581 "Parser/parser.cc"
    break;

  case 795:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10587 "Parser/parser.cc"
    break;

  case 796:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10593 "Parser/parser.cc"
    break;

  case 797:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10599 "Parser/parser.cc"
    break;

  case 798:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10605 "Parser/parser.cc"
    break;

  case 799:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10611 "Parser/parser.cc"
    break;

  case 800:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10617 "Parser/parser.cc"
    break;

  case 801:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10623 "Parser/parser.cc"
    break;

  case 802:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10629 "Parser/parser.cc"
    break;

  case 803:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10635 "Parser/parser.cc"
    break;

  case 804:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 805:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 806:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10653 "Parser/parser.cc"
    break;

  case 807:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10659 "Parser/parser.cc"
    break;

  case 808:
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10668 "Parser/parser.cc"
    break;

  case 809:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10674 "Parser/parser.cc"
    break;

  case 810:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 812:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10686 "Parser/parser.cc"
    break;

  case 813:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 814:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 815:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 816:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10710 "Parser/parser.cc"
    break;

  case 817:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10716 "Parser/parser.cc"
    break;

  case 818:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10722 "Parser/parser.cc"
    break;

  case 819:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10728 "Parser/parser.cc"
    break;

  case 820:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10734 "Parser/parser.cc"
    break;

  case 821:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10740 "Parser/parser.cc"
    break;

  case 822:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10746 "Parser/parser.cc"
    break;

  case 823:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10752 "Parser/parser.cc"
    break;

  case 824:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 825:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 826:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10770 "Parser/parser.cc"
    break;

  case 827:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10776 "Parser/parser.cc"
    break;

  case 828:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10782 "Parser/parser.cc"
    break;

  case 829:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 830:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 831:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 833:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 834:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 835:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 836:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 837:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 838:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 839:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 840:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 841:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10854 "Parser/parser.cc"
    break;

  case 842:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 843:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 844:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10872 "Parser/parser.cc"
    break;

  case 845:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 846:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 848:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 849:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 850:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 851:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 852:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 853:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 854:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 855:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 856:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 857:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 858:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 860:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10956 "Parser/parser.cc"
    break;

  case 861:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 862:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 863:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 864:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 865:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 866:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 868:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10998 "Parser/parser.cc"
    break;

  case 869:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11004 "Parser/parser.cc"
    break;

  case 870:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11010 "Parser/parser.cc"
    break;

  case 871:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11016 "Parser/parser.cc"
    break;

  case 872:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 873:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11028 "Parser/parser.cc"
    break;

  case 874:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11034 "Parser/parser.cc"
    break;

  case 875:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 876:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 878:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11052 "Parser/parser.cc"
    break;

  case 879:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11058 "Parser/parser.cc"
    break;

  case 880:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 881:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 883:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11076 "Parser/parser.cc"
    break;

  case 884:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 885:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 886:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 887:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 888:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 889:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11112 "Parser/parser.cc"
    break;

  case 890:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11118 "Parser/parser.cc"
    break;

  case 892:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 893:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 894:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11136 "Parser/parser.cc"
    break;

  case 895:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11142 "Parser/parser.cc"
    break;

  case 896:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 897:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11154 "Parser/parser.cc"
    break;

  case 899:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 901:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11166 "Parser/parser.cc"
    break;

  case 902:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11172 "Parser/parser.cc"
    break;

  case 903:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11178 "Parser/parser.cc"
    break;

  case 904:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11184 "Parser/parser.cc"
    break;

  case 905:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11190 "Parser/parser.cc"
    break;

  case 906:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11196 "Parser/parser.cc"
    break;

  case 908:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 909:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 910:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11214 "Parser/parser.cc"
    break;

  case 911:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11220 "Parser/parser.cc"
    break;

  case 912:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11226 "Parser/parser.cc"
    break;

  case 913:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11232 "Parser/parser.cc"
    break;

  case 914:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11238 "Parser/parser.cc"
    break;

  case 916:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11244 "Parser/parser.cc"
    break;

  case 917:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11250 "Parser/parser.cc"
    break;

  case 918:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11256 "Parser/parser.cc"
    break;

  case 919:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 920:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11268 "Parser/parser.cc"
    break;

  case 923:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 926:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11280 "Parser/parser.cc"
    break;

  case 927:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11286 "Parser/parser.cc"
    break;

  case 928:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 929:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 930:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 931:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 932:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 933:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 934:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 935:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11334 "Parser/parser.cc"
    break;

  case 936:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 937:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 938:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 939:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 940:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 941:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 942:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 943:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11382 "Parser/parser.cc"
    break;

  case 944:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11388 "Parser/parser.cc"
    break;

  case 945:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11394 "Parser/parser.cc"
    break;

  case 947:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11400 "Parser/parser.cc"
    break;

  case 951:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11406 "Parser/parser.cc"
    break;

  case 952:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11412 "Parser/parser.cc"
    break;

  case 953:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11418 "Parser/parser.cc"
    break;

  case 954:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11424 "Parser/parser.cc"
    break;

  case 955:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11430 "Parser/parser.cc"
    break;

  case 956:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11436 "Parser/parser.cc"
    break;

  case 957:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11442 "Parser/parser.cc"
    break;

  case 958:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11448 "Parser/parser.cc"
    break;

  case 959:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11454 "Parser/parser.cc"
    break;

  case 960:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11460 "Parser/parser.cc"
    break;

  case 961:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11466 "Parser/parser.cc"
    break;

  case 962:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11472 "Parser/parser.cc"
    break;

  case 963:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11478 "Parser/parser.cc"
    break;

  case 964:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11484 "Parser/parser.cc"
    break;

  case 965:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11490 "Parser/parser.cc"
    break;

  case 966:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11496 "Parser/parser.cc"
    break;

  case 967:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11502 "Parser/parser.cc"
    break;

  case 970:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11508 "Parser/parser.cc"
    break;

  case 971:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11514 "Parser/parser.cc"
    break;


#line 11518 "Parser/parser.cc"

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
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
