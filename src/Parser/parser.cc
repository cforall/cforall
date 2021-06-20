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
    TYPEDIMname = 355,
    TYPEDEFname = 356,
    TYPEGENname = 357,
    TIMEOUT = 358,
    WOR = 359,
    CATCH = 360,
    RECOVER = 361,
    CATCHRESUME = 362,
    FIXUP = 363,
    FINALLY = 364,
    INTEGERconstant = 365,
    CHARACTERconstant = 366,
    STRINGliteral = 367,
    DIRECTIVE = 368,
    FLOATING_DECIMALconstant = 369,
    FLOATING_FRACTIONconstant = 370,
    FLOATINGconstant = 371,
    ARROW = 372,
    ICR = 373,
    DECR = 374,
    LS = 375,
    RS = 376,
    LE = 377,
    GE = 378,
    EQ = 379,
    NE = 380,
    ANDAND = 381,
    OROR = 382,
    ELLIPSIS = 383,
    EXPassign = 384,
    MULTassign = 385,
    DIVassign = 386,
    MODassign = 387,
    PLUSassign = 388,
    MINUSassign = 389,
    LSassign = 390,
    RSassign = 391,
    ANDassign = 392,
    ERassign = 393,
    ORassign = 394,
    ErangeUpEq = 395,
    ErangeDown = 396,
    ErangeDownEq = 397,
    ATassign = 398,
    THEN = 399
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
#define TYPEDIMname 355
#define TYPEDEFname 356
#define TYPEGENname 357
#define TIMEOUT 358
#define WOR 359
#define CATCH 360
#define RECOVER 361
#define CATCHRESUME 362
#define FIXUP 363
#define FINALLY 364
#define INTEGERconstant 365
#define CHARACTERconstant 366
#define STRINGliteral 367
#define DIRECTIVE 368
#define FLOATING_DECIMALconstant 369
#define FLOATING_FRACTIONconstant 370
#define FLOATINGconstant 371
#define ARROW 372
#define ICR 373
#define DECR 374
#define LS 375
#define RS 376
#define LE 377
#define GE 378
#define EQ 379
#define NE 380
#define ANDAND 381
#define OROR 382
#define ELLIPSIS 383
#define EXPassign 384
#define MULTassign 385
#define DIVassign 386
#define MODassign 387
#define PLUSassign 388
#define MINUSassign 389
#define LSassign 390
#define RSassign 391
#define ANDassign 392
#define ERassign 393
#define ORassign 394
#define ErangeUpEq 395
#define ErangeDown 396
#define ErangeDownEq 397
#define ATassign 398
#define THEN 399

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

#line 620 "Parser/parser.cc"

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
#define YYLAST   18633

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  172
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  286
/* YYNRULES -- Number of rules.  */
#define YYNRULES  968
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1974

#define YYUNDEFTOK  2
#define YYMAXUTOK   399


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
       2,     2,     2,   161,     2,     2,     2,   165,   158,     2,
     146,   148,   157,   159,   152,   160,   149,   164,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   153,   171,
     166,   170,   167,   169,   147,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   150,   163,   151,   156,     2,   155,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   154,   168,   145,   162,     2,     2,     2,
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
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   532,   532,   536,   543,   544,   545,   546,   547,   551,
     552,   553,   554,   555,   556,   557,   561,   562,   563,   568,
     572,   573,   584,   586,   588,   592,   593,   595,   597,   599,
     601,   614,   615,   625,   630,   635,   636,   639,   645,   651,
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
    1909,  1914,  1915,  1919,  1925,  1926,  1932,  1933,  1935,  1937,
    1939,  1944,  1946,  1951,  1952,  1954,  1956,  1961,  1963,  1965,
    1967,  1969,  1971,  1976,  1977,  1979,  1981,  1986,  1988,  1987,
    1991,  1999,  2000,  2002,  2004,  2009,  2010,  2012,  2017,  2018,
    2020,  2022,  2027,  2028,  2030,  2035,  2037,  2039,  2041,  2042,
    2044,  2049,  2051,  2053,  2058,  2059,  2063,  2064,  2069,  2068,
    2073,  2072,  2080,  2079,  2090,  2089,  2099,  2104,  2105,  2110,
    2116,  2130,  2131,  2135,  2137,  2139,  2145,  2147,  2149,  2151,
    2153,  2155,  2157,  2159,  2165,  2166,  2171,  2173,  2175,  2184,
    2186,  2187,  2188,  2190,  2192,  2193,  2198,  2199,  2200,  2205,
    2207,  2210,  2217,  2218,  2219,  2225,  2230,  2232,  2238,  2239,
    2245,  2246,  2250,  2255,  2258,  2257,  2261,  2264,  2270,  2269,
    2278,  2284,  2288,  2290,  2295,  2297,  2299,  2301,  2307,  2310,
    2316,  2317,  2319,  2320,  2321,  2323,  2325,  2332,  2333,  2335,
    2337,  2342,  2343,  2349,  2350,  2352,  2353,  2358,  2359,  2360,
    2362,  2370,  2371,  2373,  2376,  2378,  2382,  2383,  2384,  2386,
    2388,  2393,  2395,  2400,  2402,  2411,  2413,  2418,  2419,  2420,
    2424,  2425,  2426,  2431,  2432,  2437,  2438,  2439,  2443,  2444,
    2449,  2450,  2451,  2452,  2453,  2468,  2469,  2474,  2475,  2481,
    2483,  2486,  2488,  2490,  2513,  2514,  2520,  2521,  2527,  2526,
    2536,  2535,  2539,  2545,  2551,  2552,  2554,  2556,  2561,  2563,
    2565,  2567,  2573,  2574,  2578,  2579,  2584,  2586,  2593,  2595,
    2596,  2598,  2603,  2605,  2607,  2612,  2614,  2619,  2624,  2632,
    2634,  2639,  2640,  2645,  2646,  2650,  2651,  2652,  2657,  2659,
    2665,  2667,  2672,  2674,  2680,  2681,  2685,  2689,  2693,  2695,
    2696,  2697,  2702,  2705,  2704,  2716,  2715,  2727,  2726,  2738,
    2737,  2751,  2757,  2759,  2765,  2766,  2771,  2778,  2783,  2789,
    2792,  2795,  2799,  2805,  2808,  2811,  2816,  2817,  2818,  2822,
    2828,  2829,  2839,  2840,  2844,  2845,  2850,  2855,  2856,  2862,
    2863,  2865,  2870,  2871,  2872,  2873,  2874,  2876,  2911,  2913,
    2918,  2920,  2921,  2923,  2928,  2930,  2932,  2934,  2939,  2941,
    2943,  2945,  2947,  2949,  2951,  2956,  2958,  2960,  2962,  2971,
    2973,  2974,  2979,  2981,  2983,  2985,  2987,  2992,  2994,  2996,
    2998,  3003,  3005,  3007,  3009,  3011,  3013,  3025,  3026,  3027,
    3031,  3033,  3035,  3037,  3039,  3044,  3046,  3048,  3050,  3055,
    3057,  3059,  3061,  3063,  3065,  3080,  3085,  3090,  3092,  3093,
    3095,  3100,  3102,  3104,  3106,  3111,  3113,  3115,  3117,  3119,
    3121,  3123,  3128,  3130,  3132,  3134,  3136,  3146,  3148,  3150,
    3151,  3153,  3158,  3160,  3162,  3167,  3169,  3171,  3173,  3178,
    3180,  3182,  3196,  3198,  3200,  3201,  3203,  3208,  3210,  3215,
    3217,  3219,  3224,  3226,  3231,  3233,  3250,  3251,  3253,  3258,
    3260,  3262,  3264,  3266,  3271,  3272,  3274,  3276,  3281,  3283,
    3285,  3291,  3293,  3295,  3298,  3302,  3304,  3306,  3308,  3342,
    3343,  3345,  3347,  3352,  3354,  3356,  3358,  3360,  3365,  3366,
    3368,  3370,  3375,  3377,  3379,  3385,  3386,  3388,  3397,  3400,
    3402,  3405,  3407,  3409,  3423,  3424,  3426,  3431,  3433,  3435,
    3437,  3439,  3444,  3445,  3447,  3449,  3454,  3456,  3464,  3465,
    3466,  3471,  3472,  3477,  3479,  3481,  3483,  3485,  3487,  3494,
    3496,  3498,  3500,  3502,  3505,  3507,  3509,  3511,  3513,  3518,
    3520,  3522,  3527,  3553,  3554,  3556,  3560,  3561,  3565,  3567,
    3569,  3571,  3573,  3575,  3582,  3584,  3586,  3588,  3590,  3592,
    3597,  3599,  3601,  3608,  3610,  3628,  3630,  3635,  3636
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
     395,   396,   397,   398,   399,   125,    40,    64,    41,    46,
      91,    93,    44,    58,   123,    96,    94,    42,    38,    43,
      45,    33,   126,    92,    47,    37,    60,    62,   124,    63,
      61,    59
};
# endif

#define YYPACT_NINF (-1638)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-849)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     188,  9666,   294,   348, 14986,   247, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638,   179,   789,   283,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638,    88,   475, -1638, -1638, -1638, -1638,
   -1638, -1638,  4975,  4975,   340,  9666,   404,   410, -1638, -1638,
     415, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
    2488, -1638,   708,   419, -1638, -1638, -1638, -1638, 14839, -1638,
   -1638,   421,   459,   230,    70, -1638,  4975,   459,   459,   459,
     456,  4667,   656,   704, 10988, -1638, -1638, -1638, 14692,  2170,
   -1638, -1638, -1638,  2442,   669, 12109,   931,   663,  2442,   965,
     483, -1638, -1638, -1638, -1638,   630, -1638, -1638, -1638, -1638,
     548, -1638, -1638, -1638, -1638, -1638,   576,   571,   630, -1638,
     630,   579, -1638, -1638, -1638, 15492,  4975, -1638, -1638,  4975,
   -1638,  9666,   596, 15550, -1638, -1638,  4912, 16573, -1638,   809,
     809, -1638,  2629, -1638, -1638, -1638, -1638, 14239, 13329,  3570,
     630, -1638, -1638, -1638, -1638, -1638, -1638,   600, -1638,   590,
     628,   646, -1638,   707, 18111, 13937,  2940,  2488,   496,   689,
     692,   731,   741,   750,   752, -1638, -1638, 15697, 10351,   671,
   -1638,  7745, -1638, -1638, -1638, -1638,   706, -1638, -1638,   751,
   -1638,   902, 17463, -1638,   762,  4975,   571,   799,   801,   806,
     840, -1638, -1638, -1638,  3391,  4652,   856,   917,   101, -1638,
   -1638,   630,   630,   123,   151,   166,   123, -1638,   630,   630,
   -1638,  4829, -1638, -1638,   877,   880,   809, 17241, -1638, -1638,
   14839, -1638, -1638,  2442, -1638,  1789,   483,   874,   956,   151,
    4975,   230, -1638, 12554, -1638,   809,   809,   889,   956,   151,
    4975, -1638, 11035, -1638, -1638,   809, -1638,   809, -1638,   727,
    4309,  4975, -1638,  2144,   919, -1638, -1638, -1638, 15142,   571,
     169, -1638, -1638,  8623, -1638,   917,   186, -1638, 18111, 16573,
    3538,  4829, -1638,   321, -1638, -1638, -1638, 15550,  4975,   906,
   -1638, -1638, -1638, -1638,  4975,  3606,   325,   114, -1638,  4975,
     590, -1638, 18183,   923,   958, 18111, 18255,   963, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, 18327, 18327, 13786,   846,
    4209, -1638, -1638, -1638, -1638,   926, -1638,   964,  1015, -1638,
    1434,  4256, 14239, 18111, -1638,   973,   554,   887,   935,   526,
     951,   980,   987,   993,  1037,    62, -1638, -1638, -1638,   511,
    1018, -1638, -1638,   508, -1638, -1638,   630,  1019, 15755,   425,
   13484, 17297,  2442,  2442, -1638,  2442,   809,  2442,   809, -1638,
   -1638,   630, -1638,  1030, -1638, 15902, -1638, -1638, -1638, 15960,
     706, -1638,  1027,    71,  1238,  1029,   483,  1033, -1638,  2629,
    1002,   590,  2629,  2709,  1049,  1051, -1638, 18111, -1638,   642,
    1018, -1638,   647,  2940,  1059,  1068,  1071,  1077,  1083,  1093,
   -1638, -1638,   328,  1100, -1638,   434,  1100, -1638, -1638, 15492,
   -1638,   936,  1101, 14390, -1638, -1638,  4728,  3956,  1130, 13484,
    1132,  1004,  1036, -1638, -1638, -1638, -1638, -1638,  4975,  4842,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638,  9112, -1638, -1638,
   17535,  1118, -1638, -1638, -1638, -1638, -1638,  3391,   364,  1126,
    1135,  1138,   566,  1148,  1154,  1159,  4652, -1638, -1638,   630,
    1120,   230,  1162, -1638, -1638,  1127, -1638, -1638,   571,   956,
   -1638, -1638, -1638,   571, -1638, -1638,  4829, -1638, 14239, 14239,
   -1638,   809,  4912, 12914, 13329, -1638, -1638, -1638, -1638, -1638,
     571,   956,   186, -1638, -1638,  2442,  1163,   956,   151, -1638,
     571,   956, -1638, 11503, -1638,   809,   809, -1638, -1638,  1171,
     298,  1173,   483,  1175, -1638, 16728, -1638,   650, -1638,  1224,
   17143, -1638,  4912, 16116, 17241, -1638, 15142, 18399, -1638, -1638,
   -1638, -1638, -1638,  3538,   611,  4829, -1638, 13329,   917, -1638,
    1184, -1638,  1192, -1638, -1638, -1638, -1638, -1638,  2629, -1638,
   -1638, 14088, -1638, 16172, 16172, -1638, 14088, -1638, 18111, 14088,
   -1638, -1638, 15198, 16172, 16172,   846,  1022,  1177,   535,  1717,
   -1638,   655,  1201,   957,  1203, -1638, 17535, 10351, 17607,  1198,
    2144,  2144, -1638, -1638,  3985, -1638, -1638, 17679,  1978, 18111,
   17679,  2144, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638,  1199, 18111, -1638, -1638, -1638, -1638, 18111,
   18111, 18111, 18111, 18111, 18111, 18111, 18111, 18111, 18111, 18111,
   18111, 18111, 18111, 18111, 18111, 18111, 18111, 18111, 17751,   511,
     915, -1638, -1638,   630,   630, -1638,  3460,  1281, 14239,  4469,
   15960, 10351, -1638, 16319, -1638,   809,   809, -1638, -1638,   706,
   -1638,   606,  1204,  1340, 18111,  1141,  1127,  1188, -1638,   630,
     630, -1638,  1100, -1638, 15755, -1638, -1638, 17003,   809,   809,
   -1638,  4469,   630, -1638, 16524, -1638, -1638, 15902, -1638,   467,
    1209,   285,  1205,  1238,   678, 15550,   703, -1638, -1638, -1638,
   -1638, -1638, -1638,   709, -1638,  1216,  1193, 18111,  1194,   707,
   -1638, -1638,   358,  1100, -1638,   712,  1100, -1638, -1638, -1638,
    1127, -1638, -1638,  1127, 18471, -1638, -1638, 10351,  1217,  1219,
    3296,  1350,  4226,   359,  1188, -1638,   630,   630,  1188,   370,
   -1638,   630,   630, 18111,  4975,   966,   972,  1188,   174, 12864,
   12864,  4975,  1220,  3895,  1051,  1221,  1222, -1638,  1226, 17463,
     618, -1638, -1638, -1638,   711, -1638, 12864,  2144,  4912,  2144,
     720,  1228,  1232,  1233,   721,  1234,  1235,  1239,   372,  1100,
   -1638, -1638,   409,  1100, -1638, -1638, -1638,  4912,   707, -1638,
    1100, 18471, -1638,   571, 16728, -1638, -1638,   725,  1240,   726,
    1245, -1638,  1227, -1638,   571, -1638, -1638,   571,   956,  1227,
   -1638,   571,  1242,  1243,  1244, -1638, -1638, 17003, -1638,  1246,
   -1638, -1638, -1638,  2144,  4975,  9854,  1321,  1229, 17087, -1638,
    1101, -1638, 12864,   755, -1638,  1227, -1638, 15550, 14239,  1248,
   -1638,  1248,  1251,   419,  1250,  1255,  1256,  1253,  1261, 18111,
    1262,  1265,  1267, 10351, 18111, -1638, -1638,  1758, -1638, -1638,
   -1638, 18111, -1638,  1268,  1271, 17391,   990, -1638, 17679, -1638,
   -1638, -1638,  2726, -1638, -1638,   758, -1638, -1638, -1638,  2726,
   -1638, -1638,   994,   537, -1638,  9112, -1638, -1638,   973,   973,
     973,   554,   554,   887,   887,   935,   935,   935,   935,   526,
     526,   951,   980,   987,   993,  1037, 18111,   999, 16728,  1272,
    1274,  1275,   915, -1638, -1638, -1638,  2726, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, 15902, -1638, 10510, 14541, -1638, 16728,
    1280,  1282,  1283, -1638,  5505,   630, -1638,  1141, -1638, -1638,
   -1638, -1638,  1127, -1638, -1638, -1638,   809, -1638,  3693, -1638,
   -1638,   483,  2404,  1287, -1638, 17463, -1638,  1238,  1209, -1638,
   -1638,  1284,  1286,  2709, 17679, -1638, -1638, -1638,  1292, -1638,
   -1638, -1638,  1127, -1638, -1638,  1127, 16728, 16728, -1638, -1638,
    3296,   633,  1293,  1296,  1297,  1300,  3240,  4226, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638,  1295, -1638,  1188, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638,  1302,  1303, -1638,   230, -1638, -1638, 18111,
   -1638,  9112,  1290, -1638,  1509, -1638, -1638, -1638, -1638, 17823,
   12864, -1638, -1638, -1638,  1285,   440,  1100, -1638,   448,  1100,
   -1638, -1638, -1638, -1638,  1127, -1638, -1638, -1638,  1127,   917,
    1306,  1127, -1638, -1638, -1638, -1638, -1638, -1638, -1638,  1311,
   -1638, -1638,  1227, -1638,   571, -1638, -1638, -1638, -1638, -1638,
    8828,  1310,  1307, -1638,   111, -1638,   308,   121, 10192,  1314,
   12386,  1315,  1319,  2810,  3123,  3427, 17895,  1320, -1638, -1638,
    1323,  1326, -1638, -1638,   571, 18111, 18111,  1454,  1324,   578,
   -1638,  1404,  1327,  1305, -1638, -1638, -1638,  9443, -1638, -1638,
   -1638, -1638, -1638,  2680, -1638, -1638, -1638,  1395, -1638, -1638,
   -1638,  2144, -1638, -1638, 11456, 14839,  1331, -1638,  4975, -1638,
    1316,  1334,  1337, -1638,  1008, -1638, -1638, -1638,  4912, -1638,
   -1638,  1325,  1328,   769, 15550,   590,   590,   588, 10351,  2144,
   -1638,   588, 15345,   588, -1638, 18111, 18111, 18111, -1638, -1638,
   -1638, -1638, 18111, 18111,  1336,  9112, -1638, -1638,  1014,   599,
   -1638,  2163, -1638, -1638,  1024, -1638,    56, -1638, 17679,  1054,
   -1638, 18111, -1638,  1226, -1638, 18111, -1638,   458,  1100, -1638,
   -1638,  1058, -1638, -1638,  1051,  1101, 14390, -1638,  1018, -1638,
   10669, -1638,   463,  1100, -1638,   809,  6925, -1638, -1638,  1238,
     630,   630,   467,   285, -1638, -1638,  1209,  1347,  1349, -1638,
   -1638,   770,  1343,  1329, 16728, 16728, -1638, -1638,  1353,   471,
    1100, -1638,   476,  1422,   630,   630, -1638, -1638, 16728, 16728,
   -1638,  1346, -1638, 13329, 13329,  1354,  1352,  1356,  1357, -1638,
    2726,  1060,    38, -1638, -1638, -1638, 17463, -1638, 18111, -1638,
   -1638, -1638,  1358, 18111, -1638, -1638, -1638,  1127, -1638, -1638,
   -1638,  1127, 16728, 16728,   230,   630,  1070,  1361,  1368, -1638,
   -1638,  1369, 11606, 11756, 11906, 15550, 16172, 16172,  1370, -1638,
    1348,  1351,  2336,  6648, -1638,   376,  4975, -1638, -1638,  4975,
   -1638, 17679,   408,   465, -1638, -1638, -1638, -1638, 18111,  1375,
    1448,  1385,  1386, -1638,  1371, -1638,  1372, 18111,  1374,  9112,
    1377, 18111, 17535, 18111,   992, -1638,  1379,    79, -1638,   100,
    1400, -1638, -1638,  1396, -1638,  1387, -1638,  1390,  1410, 12386,
     582, 12709,   630,   393, -1638, -1638, -1638,  1414, -1638,  1418,
   -1638,  1420, -1638,  1415, -1638,  1419, -1638, -1638, -1638, -1638,
    1425,  1417,   798, -1638,  1423, -1638, -1638, -1638, -1638, -1638,
    9112,  1226, 17535, -1638,  1463,  2726, -1638,  1463,  1463, -1638,
    2726,  2826,  3822, -1638, -1638,  1072,  1436, -1638,  1431, -1638,
   -1638, -1638,  1127, -1638, 10828,  1435,  1440,  1441, -1638,  1446,
   -1638, -1638, -1638,  1127, 18111, 18111,  1101,  1444, -1638,  1209,
   -1638,  1428,   273, -1638,  1451, -1638, -1638, 15550, -1638, -1638,
   -1638, -1638,  1450,  1452,   630, -1638, -1638, -1638,  1127, -1638,
   -1638, -1638,  1453, -1638, -1638, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638,  1458, 18111, 18111,  1076,
    1455, -1638, -1638,  1472,   630, -1638, 16728, 16728, -1638, -1638,
   -1638, -1638, 18111, -1638, -1638,  1457, -1638,  1370,  1370,  1370,
     643,  1432,   401, -1638,  4121,   405, 14239, -1638, -1638, -1638,
    3796, 18111,  4898,   426, -1638, -1638,    58,  1470,  1470,  4975,
   -1638, -1638, 16874, -1638,   804, -1638, -1638, -1638, -1638,   807,
    1478, 12386, 10192, 12386, 10023, -1638, -1638,   444, -1638,  1226,
   -1638,   812,   815,   821, -1638, -1638, -1638, -1638,   571,   992,
    1481, -1638, -1638, 18111, -1638,  1482,   707, 10192, -1638, -1638,
   -1638, -1638, 18111,  1525, -1638, 12386, -1638,   630, 13329, -1638,
   -1638, 15550, -1638, -1638, -1638, 18111, -1638, 15345, 18111,  1226,
    1485,  1084, -1638,  1087, -1638,  2726, -1638,  2726, -1638, -1638,
   -1638, -1638, 16728, -1638, -1638, -1638,  1487, -1638, 16728, -1638,
   -1638,  1489, -1638,  1491,  1488,  1490,  1238, -1638, -1638, -1638,
    1499,  1500, -1638, -1638, 16728, 16728,  1502,  1504,  1094, 13019,
   13174,  1503, -1638, -1638, -1638, -1638,  1506, -1638, -1638, -1638,
   -1638,  1505,  1507,  1097, -1638, -1638, -1638, -1638,   643,  1523,
     502, -1638, -1638, -1638, -1638,   630,   630, -1638, -1638, -1638,
     510, -1638,   822,  3796,   634, -1638,  4898,   630, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638, -1638, 12386,   113, 17967, -1638,
    1327,  1513, 18111,   421,  1484,   456, 12062, 15550, -1638, 18111,
   18111,   745,   -19, -1638, 18111, -1638,  1526,   120, 12386, -1638,
   -1638,  1524, -1638, -1638,  1501,   707,   237,  1527,  1528,  1102,
    1589, -1638, -1638, -1638,  4975,  4912, -1638, -1638, -1638,  1534,
   -1638, -1638, -1638,  1105,  1111, -1638, -1638, -1638,  1531,  1538,
   -1638, -1638, -1638,  1238,  1209,  1540, -1638, -1638, -1638, -1638,
   -1638, -1638, -1638, -1638, -1638,  1543, -1638, -1638,  1544,  1549,
    1556, -1638, -1638, -1638,  1559,  1560,  1561,  1523, -1638,   630,
   -1638, -1638, -1638, -1638, -1638,  1551,  4121, -1638, 18111,  1558,
   -1638, -1638, 12150, -1638,  1541,   823, 12386,  1327, 13639,  1327,
    1542, -1638, -1638, -1638, -1638, 17312, 18111, 12386, 10023,  1546,
    1555, -1638, -1638, -1638, -1638, 16377, -1638,  1567,  1565,    30,
   12386, -1638, 18111, 17679,   338, -1638, -1638, -1638, -1638, -1638,
   -1638,  1566,  1576, -1638, -1638,  1209,  1582, -1638,  1590,  1591,
   13329,  1585, -1638, -1638, -1638,   479,  1100, -1638, -1638,   643,
   -1638,   145, -1638,  1116, -1638, -1638, 11144, -1638, -1638, -1638,
    1572, -1638, 18111,  1598, 18111,   772,  1577,   280, -1638, -1638,
   18111, -1638, 11144, 16377, -1638,  4530, 16319,  2144,  1596, -1638,
    1653,  1606,   550,  1601, -1638,  1685, -1638,   851, 12386,  1611,
   12386, 12386, -1638, -1638,  1613, -1638, -1638, -1638, -1638, -1638,
   -1638, -1638,  1127, -1638, 18111, 18111, -1638,  1207, 11300, -1638,
   -1638, -1638, -1638,  1327,  1612,  1615, 18111, 18111, 18111, -1638,
   -1638,  1207, -1638,  1594,  3091,  4408, -1638, -1638, -1638,    30,
    1616, 18111,  1595,    30,    30, 12386, -1638, -1638, 18111,  1664,
    1675, -1638, 16728, -1638, -1638, 12150, -1638,  1207, -1638,  1618,
    1619,   318, -1638,  1327, -1638,  1594, 18111,  1636,  4408,  1632,
     707,  1638, -1638,   560, -1638, -1638,   852,  1589,   432, -1638,
   -1638, 12268,  1642, 12150, 18111, 18039, 18111,  1644,  1648, -1638,
     571,   707,  1646, -1638,  1626,   707, -1638, -1638, 12386,  1730,
    1655, -1638, -1638, 12268,  1327, -1638,  1327,  1327, -1638,   571,
   -1638, -1638,  1129, 18111, -1638,   861, -1638, 12386, -1638, -1638,
     707,  2144,  1657,  1630, -1638, -1638, -1638,   863, -1638, -1638,
    1640,  2144, -1638, -1638
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   401,     0,     2,   401,   418,   419,   420,   421,   422,
     423,   424,   425,   407,   409,   408,   410,     0,     0,     0,
     426,   428,   446,   429,   447,   432,   433,   444,   445,   427,
     442,   443,   430,   431,   434,   435,   436,   437,   438,   439,
     440,   441,   448,   449,   732,   451,   524,   525,   528,   530,
     526,   532,     0,     0,     0,   401,     0,     0,    16,   495,
     501,     9,    10,    11,    12,    13,    14,    15,   698,    91,
       0,    18,     0,     2,    89,    90,    17,   748,   401,   699,
     350,     0,   353,   625,   355,   364,     0,   354,   384,   385,
       0,     0,     0,     0,   478,   403,   405,   411,   401,   413,
     416,   463,   450,   389,   456,   461,   390,   473,   391,   488,
     492,   498,   477,   504,   516,   732,   521,   522,   505,   571,
     356,   357,     3,   700,   711,   406,     0,     0,   732,   770,
     732,     2,   787,   788,   789,   401,     0,   946,   947,     0,
       1,   401,     0,   401,   373,   374,     0,   478,   395,   396,
     397,   703,     0,   527,   529,   531,   533,   401,   401,     0,
     733,   734,   523,   452,   618,   619,   617,   677,   672,   662,
       0,     0,   701,     0,     0,   401,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   496,   499,   401,   401,     0,
     948,   478,   777,   795,   952,   945,   943,   950,   349,     0,
     153,   152,     0,   358,     0,     0,     0,     0,     0,     0,
       0,   348,   847,   848,     0,     0,   383,   730,   732,   726,
     751,   732,   732,   728,     2,   732,   727,   808,   732,   732,
     805,     0,   471,   472,     0,     0,   401,   401,   418,     2,
     401,   365,   404,   414,   464,     0,   493,     0,   714,     2,
       0,   625,   366,   478,   457,   474,   489,     0,   714,     2,
       0,   417,   458,   465,   466,   475,   480,   490,   494,     0,
     508,     0,   692,     2,     2,   712,   769,   771,   401,     0,
       2,     2,   956,   478,   959,   730,   730,     3,     0,   478,
       0,     0,   376,   732,   728,   727,     2,   401,     0,     0,
     658,   660,   659,   661,     0,     0,   654,     0,   644,     0,
     653,   664,     0,     0,     0,     0,     0,     0,    22,    24,
       4,     8,    20,     5,     6,     7,     0,     0,   401,     2,
       0,    92,    93,    94,    95,    76,    23,    77,    19,    35,
      75,    96,   401,     0,   111,   113,   117,   120,   123,   128,
     131,   133,   135,   137,   139,   141,   145,   669,    25,   621,
     492,   623,   668,     0,   620,   624,   732,     2,   401,   967,
     402,   401,   413,   392,   456,   393,   481,   394,   488,   485,
     506,   732,   507,     0,   606,   401,   607,   921,   922,   401,
     608,   610,   495,   501,     0,   572,   573,     0,   735,     0,
     675,   663,     0,   739,     0,     2,    96,     0,   144,     0,
       0,   502,     0,     0,     0,     0,     0,     0,     0,     0,
     749,   775,   732,   785,   793,   797,   803,     2,   954,   401,
     957,     2,    89,   401,     3,   605,     0,   967,     0,   402,
     456,   481,   488,     3,     3,   587,   591,   601,   607,   608,
       2,   778,   796,   944,     2,     2,   627,   630,   628,   626,
       0,     0,   716,     2,     2,     2,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   754,   811,   732,
       0,   625,     2,   750,   758,   874,   752,   753,     0,   714,
       2,   807,   815,     0,   809,   810,     0,   379,   401,   401,
     462,   402,     0,   478,   401,   949,   953,   951,   479,   696,
       0,   714,   730,   359,   367,   415,     0,   714,     2,   696,
       0,   714,   673,   459,   460,   476,   491,   497,   500,   495,
     501,   519,   520,     0,   674,   401,   615,     0,   189,   342,
     401,     3,     0,   478,   401,   713,   401,     0,   361,     2,
     362,   693,   381,     0,     0,     0,     2,   401,   730,   696,
       0,     2,     0,   657,   656,   655,   650,   412,     0,   648,
     665,   401,    83,   401,   401,    78,   401,    85,     0,   401,
      81,    82,   401,   401,   401,     2,    92,    93,     0,     0,
     171,     0,     0,   522,     0,   943,     0,   401,     0,     0,
       0,     0,    21,    45,     0,    51,    52,    56,     0,     0,
      56,     0,   154,   155,   156,   157,   158,   159,   160,   161,
     162,   163,   164,   152,     0,   150,   151,    79,    80,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     2,
     859,   622,   856,   732,   732,   864,     0,   454,   401,     0,
     401,   401,   923,   402,   398,   399,   400,   927,   918,   919,
     925,     2,     2,    90,     0,   883,   897,   967,   879,   732,
     732,   888,   895,   613,   401,   486,   609,   402,   482,   483,
     487,     0,   732,   933,   402,   938,   930,   401,   935,     0,
     965,   578,     0,     0,     0,   401,     0,   747,   746,   742,
     744,   745,   743,     0,   737,   740,     0,     0,     0,     0,
     503,   776,   732,   786,   794,   798,   804,     2,   779,   781,
     783,     2,   799,   801,     0,   955,   958,   401,     0,     0,
       2,    90,   883,   732,   967,   829,   732,   732,   967,   732,
     844,   732,   732,     3,   609,     0,     0,   967,   967,   401,
     401,     0,    22,     0,     2,    23,     0,   631,   965,     0,
       0,   637,   168,   167,     0,     2,   401,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   732,   763,
     767,   806,   732,   820,   825,   755,   812,     0,     0,   387,
     871,     0,   717,     0,   401,   718,   380,     0,     0,     0,
       0,   378,     2,   719,     0,   363,   696,     0,   714,     2,
     720,     0,     0,     0,     0,   534,   594,   402,     3,     3,
     598,   597,   790,     0,     0,   401,   343,     0,   478,     3,
      89,     3,   401,     0,     3,     2,   652,   401,   401,   646,
     645,   646,     0,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   401,     0,   110,   109,     0,   106,   105,
      26,     0,    27,     0,     0,     0,     0,     3,    56,    41,
      42,    49,     0,    48,    59,     0,    57,    60,    44,     0,
      43,    47,     0,     0,    40,   630,   146,   112,   114,   115,
     116,   118,   119,   121,   122,   126,   127,   124,   125,   129,
     130,   132,   134,   136,   138,   140,     0,     0,   401,     0,
       0,     0,   860,   861,   857,   858,     0,    28,   455,   453,
     671,   670,   572,   929,   401,   934,   402,   401,   920,   401,
       0,     0,     0,   898,     0,   732,   968,   884,   885,   614,
     881,   882,   896,   924,   928,   926,   484,   519,     0,   932,
     937,   575,   966,     0,   152,     0,   574,     0,   965,   678,
     676,     0,     0,   739,    56,   702,     3,   352,     0,     2,
     780,   782,   784,     2,   800,   802,   401,   401,   876,   875,
       2,     0,     0,     0,     0,     0,   732,   884,   832,   849,
       2,   827,   835,   611,   830,   831,   612,     2,   842,   852,
     845,   846,     0,     3,   967,   371,     2,   960,     2,   602,
     603,   581,     3,     3,     3,     3,   625,     2,   639,     0,
     636,   966,     0,   632,     0,     2,   635,   638,   715,     0,
     401,     3,   375,   377,     0,   732,   764,   768,   732,   821,
     826,     2,   756,   759,   761,     2,   813,   816,   818,   730,
       0,   872,     3,   722,     3,   468,   467,   470,   469,     2,
     697,   723,     2,   721,     0,   697,   724,   534,   534,   534,
     401,     0,     0,   616,     0,   346,     0,     0,   401,     0,
       2,     0,     0,     0,     0,     0,   173,     0,   276,   277,
       0,     0,   315,   314,     0,   148,   148,   321,   495,   501,
     187,     0,   174,     0,   197,   175,   176,   401,   191,   177,
     178,   179,   180,     0,   181,   182,   282,     0,   183,   184,
     185,     0,   186,   193,   478,   401,     0,   195,     0,   340,
       0,     0,     0,     3,     0,   697,   685,   686,     0,     3,
     681,     3,     3,     0,   401,   662,   662,    84,   401,     0,
      88,    86,   401,     0,   100,     0,     0,     0,   104,   108,
     107,   172,     0,     0,     0,   630,    97,   165,     0,     0,
      73,     0,    73,    73,     0,    61,    63,    39,     0,     0,
      37,     0,    38,   965,   143,     0,     3,   732,   867,   870,
     862,     0,   931,   936,     2,    89,   401,     3,   493,     3,
     402,     3,   732,   891,   894,   401,     3,   880,   886,     0,
     732,   732,     0,   578,   563,   579,   965,     0,     2,   736,
     738,     0,     0,     0,   401,   401,     3,     3,     0,   732,
     838,   841,   732,     0,   732,   732,   833,   850,   401,   401,
     961,     0,   604,   401,   401,     0,     0,     0,     0,   360,
       0,   144,     0,     3,     3,   633,     0,   629,     0,   170,
     169,     3,     0,     0,     2,   757,   760,   762,     2,   814,
     817,   819,   401,   401,   625,   732,     0,     0,     0,   697,
     725,     0,   401,   401,   401,   401,   401,   401,   517,   545,
       3,     3,   546,   478,   535,     0,     0,   772,     2,     0,
     344,    56,     0,     0,   267,   268,   194,   196,     0,     0,
       0,     2,     2,   263,     0,   261,     0,     0,     0,   630,
       0,     0,     0,     0,     0,   149,     0,     0,   322,     0,
       0,     3,   200,     0,   192,     0,   258,     0,     0,     2,
       0,   478,   732,     0,   341,   878,   877,     0,     2,     0,
     688,     2,   683,     0,   684,     0,   666,   647,   651,   649,
       0,     0,     0,    31,     0,   101,   103,   102,    99,    98,
     630,   965,     0,    55,    70,     0,    64,    71,    72,    50,
       0,     0,     0,    58,    46,     0,     0,   142,     0,     2,
     863,   865,   866,    29,   401,     0,     0,     0,     3,     0,
       2,   887,   889,   890,     0,     0,    89,     0,     3,   965,
     568,     0,   578,   576,     0,   566,   679,   401,   741,   351,
       3,     3,     0,     0,   732,     2,   834,   836,   837,     2,
     851,   853,     0,   828,   843,     3,     3,   962,     3,   589,
     588,   592,   964,     2,     2,   963,     3,     0,     0,     0,
       0,   634,     3,     0,   732,   382,   401,   401,     3,     3,
     388,   731,     0,   822,   706,     0,   708,   517,   517,   517,
     552,   522,     0,   558,   546,     0,   401,   509,   544,   540,
       0,     0,     0,     0,   547,   549,   732,   560,   560,     0,
     541,   556,   401,   347,     0,   271,   272,   269,   270,     0,
       0,     2,   401,     2,   401,   264,   262,     0,   256,   965,
     265,     0,     0,     0,   303,   304,   305,   306,     0,   296,
       0,   297,   273,     0,   274,     0,     0,   401,   201,   190,
     260,   259,     0,   294,   313,     2,   345,   732,   401,   704,
     667,   401,     2,     2,    87,     0,    30,   401,     0,   965,
       0,     0,    74,     0,    62,     0,    68,     0,    66,    36,
     147,   868,   401,   939,   940,   941,     0,   892,   401,     3,
       3,     0,   900,     0,     0,     0,     0,   577,   565,     3,
       0,     0,   773,   791,   401,   401,     0,     0,     0,   401,
     401,     0,     3,   729,   640,   641,     0,   368,   370,     3,
       3,     0,     0,     0,   710,   513,   515,   511,     0,   907,
       0,   553,   912,   555,   904,   732,   732,   539,   559,   543,
       0,   542,     0,     0,     0,   562,     0,   732,   536,   550,
     561,   551,   557,   596,   600,   599,     2,     0,     0,   227,
     208,     0,     0,   210,   355,   209,   478,   401,   231,     0,
     173,   237,     0,   232,   173,   257,     0,     0,     2,   280,
     307,     0,   298,     2,     0,     0,     0,     0,   285,     0,
     281,   188,   369,   682,     0,     0,    34,    32,    33,     0,
      53,   166,    65,     0,     0,     3,   942,     3,     0,     0,
     899,   901,   567,     0,   965,     2,   774,   792,     3,     3,
     839,   854,   372,     2,   586,     3,   585,   643,     0,     0,
       0,   765,   823,   873,     0,     0,     0,   908,   909,   732,
     538,   905,   906,   537,   518,     0,     0,   279,     0,     0,
       2,   219,     2,   202,     0,     0,     2,   211,   478,   238,
       0,   253,   254,   255,   252,   241,     0,     2,   401,     0,
       0,     2,   204,   278,     2,   401,   275,     0,     0,   323,
       2,   283,     0,    56,     0,   295,   687,   689,    54,    69,
      67,     0,     0,   902,   903,   965,     0,   680,     0,     0,
     401,     0,   642,   766,   824,   732,   915,   917,   910,     0,
     548,   212,   215,     0,   214,   218,   401,   221,   220,   229,
       0,     3,   173,   246,     0,   242,     0,   239,     3,   233,
     173,   266,   401,   401,     3,   308,   402,   312,     0,   316,
       0,     0,     0,   324,   325,   206,   286,     0,     2,     0,
       2,     2,   869,   893,     0,   570,   840,   855,   590,     2,
     911,   913,   914,   554,     0,     0,   217,   222,   401,   336,
     228,   226,   234,   243,   254,   252,     0,   173,     0,   230,
     236,   222,     3,   301,     0,   907,   309,   310,   311,   323,
       0,     0,     0,   323,     0,     2,   284,   291,     0,   288,
     290,   569,   401,   213,   216,     2,     3,   223,   337,   248,
     247,   244,   235,   240,     3,   301,     0,     0,   908,     0,
       0,     0,   317,     0,   326,   207,     0,   281,     0,     3,
     198,   224,     0,     2,     0,     0,     0,     0,     0,   302,
       0,   329,     0,   327,     0,   329,   287,   289,     2,     0,
       0,   199,   203,   225,   250,   251,   249,   245,   205,     0,
     299,   330,     0,     0,   318,     0,   292,     2,   916,   300,
       0,     0,     0,     0,   293,   331,   332,     0,   328,   319,
       0,     0,   320,   333
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1638,  5893,  6114, -1638,    -1,   629,   272, -1638,  1477, -1638,
     260, -1638,  -585,   635,  -822, -1081, -1638,   110,  5644,  1721,
   -1638,  -179, -1638,  1241,   353,   677,   680,   562,   682,  1179,
    1186,  1183,  1189,  1185, -1638,   337,  -139,  7874,   728, -1638,
    -327, -1638, -1638,  -567,  2100, -1040,   882, -1638,   130, -1638,
     719,   -72, -1638, -1638, -1638,   315,    -2, -1638, -1594, -1544,
     187,   -14, -1638, -1638, -1638,   102,    43, -1638, -1638, -1638,
   -1638,   -49, -1589,    85, -1638, -1638,   -46, -1638, -1638, -1638,
     -35,   347,   354,    54, -1638, -1638, -1638, -1638,  -854, -1638,
       2,   -48, -1638,    68, -1638,   -87, -1638, -1638, -1638,   753,
    -696,  -832, -1118, -1638,     9,    18,   487,  4549,  -647,  -629,
   -1638,  -247, -1638,    33,  -134,   871,  -341,  -208,  3605,  3657,
    -552, -1638,    60,   127,   705,  2425, -1638,  1847, -1638,     3,
    3986, -1638, -1638, -1638,    97, -1638, -1638,  1965,   209,  4606,
    2894,   -25,  1654,  -363, -1638, -1638, -1638, -1638, -1638,  -791,
    1553,  5290, -1638,  -306,     6, -1638,   416,   157, -1638,   105,
     604, -1638,   407,  -167, -1638, -1638, -1638,  5351,  -682, -1104,
    -577,  -633,  -373,  -399, -1638, -1189,  -147,  -125,  1298,   775,
    2857,   -50,  -417,  -239,  -192,  -836,   879, -1638,  1145,   203,
    1065,  1355, -1638, -1638, -1638, -1638,   177,  -154,  -198,  -174,
   -1638,   213, -1638, -1638,   484,   366, -1638, -1638, -1638,  1921,
    -732,  -448,  -918,   115, -1638, -1638, -1638, -1638, -1638,   -32,
    -782,  -126, -1637,  -168,  6889,   -61,  6759, -1638,   952, -1638,
    2142,  -189,  -212,  -206,  -196,     5,   -54,   -53,   -47,   336,
     -27,    -8,    46,  -184,   -59,  -181,  -176,  -169,  -667,  -608,
    -607,  -593,  -663,  -104,  -575, -1638, -1638,  -600,  1277,  1289,
    1291,  1278,  7520,  -601,  -632,  -630,  -591,  -677, -1638, -1530,
   -1548, -1532, -1523,  -648,  -153,  -343, -1638, -1638,   -21,   351,
     -67, -1638,  8030,   206,  -728,  -546
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1111,   208,   335,   336,   166,   337,   338,   339,  1372,
    1373,   340,   885,   886,  1184,  1185,  1186,  1384,   341,   407,
     343,   344,   588,   589,   345,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,  1262,   590,  1336,   624,
     202,   626,   358,   774,  1112,  1113,  1114,  1115,  1116,  1117,
    1118,  1921,  1119,  1120,  1341,  1651,  1802,  1803,  1741,  1742,
    1743,  1896,  1897,  1121,  1662,  1663,  1756,  1122,  1123,  1124,
    1125,  1126,  1127,  1349,  1679,  1841,  1775,  1128,  1129,  1529,
    1907,  1530,  1531,  1824,  1130,  1131,  1132,  1339,  1832,  1833,
    1834,  1952,  1967,  1857,  1858,   279,   280,   835,   836,  1084,
      79,    80,    81,    82,    83,  1654,   435,    86,    87,    88,
      89,    90,   216,   542,   437,   359,   438,    93,   289,    95,
      96,    97,   371,   372,   100,   101,   162,   102,   929,   373,
     148,   105,   236,   106,   149,   245,   375,   376,   377,   150,
     410,   111,   112,   379,   113,   533,   824,   822,   823,  1487,
     380,   381,   116,   117,  1080,  1304,  1493,  1494,  1620,  1621,
    1305,  1482,  1639,  1495,   118,   702,  1585,   382,   700,   966,
    1022,   443,   444,   828,   829,   445,   446,   830,   384,   537,
    1136,   361,   362,   203,   767,   768,   769,   770,   771,   307,
    1155,   308,   851,   849,   566,   309,   400,   310,   311,   363,
     120,   168,   169,   121,  1149,  1150,  1151,  1152,     2,  1069,
    1070,   812,  1288,   122,   299,   247,   257,   516,   123,   206,
     124,   217,  1264,   815,   483,   160,   125,   713,   714,   715,
     126,   219,   220,   221,   222,   294,   128,   129,   130,   193,
     132,   133,   134,   225,   295,   227,   228,   229,   744,   745,
     746,   747,   748,   230,   750,   751,   752,   651,   652,   653,
     654,   484,   135,   677,   678,   679,   680,   681,   682,  1623,
    1624,  1625,  1626,   667,   448,   387,   388,   389,   364,   195,
     137,   138,   139,   391,   963,   683
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      76,   412,   469,    76,   103,   386,   127,   685,   470,   177,
     459,   383,   513,   144,   625,   401,   179,   180,   471,    84,
     292,   968,   593,   181,   369,   893,   477,   662,   500,   876,
     472,   541,   226,   473,    91,   409,   954,   869,   474,   940,
    1032,   941,   695,   182,   693,   475,   698,   186,   696,   481,
     923,    76,    76,  1137,    76,   397,  1330,   194,   103,  1193,
     127,    98,   183,   447,  1449,  1450,  1002,  1189,   284,    76,
    1724,   819,  1009,    84,   948,   998,   685,    76,   469,   999,
     942,   808,   810,   192,   470,    76,  1725,  1075,    91,  1728,
      76,  1774,   662,    76,   471,  1726,   223,    76,   107,   248,
    1386,  1804,   477,   258,  1201,   497,   472,   251,   940,   473,
     941,   845,   570,  1145,   474,    98,   184,   548,   550,  1423,
     430,   475,   194,   414,   415,    54,  1023,  1023,    99,  1757,
     416,   949,   992,   993,  1758,    76,   575,   489,    76,  1133,
      76,   948,   322,  1023,   103,    76,   127,   994,  1808,   942,
     417,    76,   107,   467,   204,    54,   478,  1291,    76,    84,
     179,   180,   511,   627,   628,   995,  1457,   181,    54,   418,
     172,  1533,   521,  1391,    91,    76,    76,  1312,  1313,  1535,
    1830,   192,    99,  1025,   844,  1738,  1739,   182,  -690,   647,
      76,   488,  1738,  1739,   493,   255,   451,  1798,  1003,  1041,
    1458,    98,  1006,   570,    76,  1392,   183,   204,   482,  1023,
     108,  1019,  1020,    76,    76,   668,   510,   175,  1804,   506,
     192,   243,   205,   419,  -714,  -848,   520,  1359,   627,   553,
      76,   648,   478,    54,   158,  -338,   179,   180,   107,    76,
    1227,   931,   799,   181,   528,   192,  1536,   273,  1299,    76,
    1534,   482,    76,  -339,   627,   781,   287,   275,   517,    76,
     184,   782,   567,  1309,   108,  1143,   568,  1740,    99,    76,
      76,   783,    76,  1854,  1761,   274,   506,  -714,   594,   480,
     668,   103,  1310,   784,   197,  1226,   785,   795,   970,    76,
      76,   786,  1314,  1179,   140,  1877,   192,    76,   787,   839,
     685,  1808,   891,    76,    76,  -338,  1317,  1458,    76,  1564,
    1566,  1568,   490,  1895,  1002,  1170,   482,   935,  1587,   933,
     549,   194,  1200,  -339,   685,   152,  1724,  1895,  1774,  1808,
    1246,   685,   549,   749,  1247,  1137,   462,   131,   544,  1381,
    1289,   781,  1725,   953,   674,  1728,  1218,   782,  -691,   197,
     108,  1726,   136,  1923,   960,   136,   959,   783,   940,   151,
     941,   153,   736,   365,   154,   155,   795,   156,  1072,   784,
     806,  1475,   785,   200,   965,   107,   811,   786,  1798,   447,
     515,   365,   992,   993,   787,  1768,  1024,  1024,    54,  1231,
    1769,   131,  1316,    76,  1653,    54,  1218,   994,    76,   942,
     201,    76,   712,  1024,   386,  1064,   136,  1023,  1838,   545,
    1449,  1450,    76,   414,   415,  1238,   200,   796,   451,  1653,
     416,  1133,   269,   369,  1348,    54,    54,   224,   200,   157,
     249,   818,   871,  1300,   259,    76,    76,    54,  1456,    54,
     417,  1839,  1868,   964,   175,   404,   507,    76,    76,   136,
     897,  1301,  -514,   563,  1311,   964,   765,   803,   592,   418,
    1299,  1299,  1299,  1271,   685,  1396,    76,   556,  1252,  1024,
     871,   482,   447,   522,   727,    76,    54,   131,   482,   814,
    1926,   451,   564,   565,   534,   817,   171,   108,    85,   821,
     844,   145,   136,  1519,    19,    76,   796,   243,  1424,   414,
     415,    76,  1938,   507,   979,  1000,   416,    54,   482,   672,
     273,   408,   420,   419,   482,    54,  1007,  1101,  1051,  1196,
     672,   852,   482,   854,   855,    54,   856,   452,  1499,   858,
      54,   668,   860,   861,   862,  1412,   447,  1419,    54,    76,
    1211,    76,    85,    54,  1559,  1309,    54,  1500,   197,   103,
     173,    69,    76,  1628,    76,  1055,   174,  1499,   954,   482,
     451,   175,   274,  1563,  1546,  1441,   243,    76,    59,    60,
     187,   671,  1629,   685,   670,   672,  1631,  1033,  1637,  1505,
     731,   241,    74,   673,   482,   252,  1274,  1236,  1237,   518,
     482,  1203,   198,  1202,  1278,   674,   871,  1638,   482,    76,
      76,  1848,   704,    76,  1399,   706,   544,    76,   482,  1410,
      76,    -3,   447,   672,  1013,  1665,    72,  1435,   802,   274,
    1750,   482,  1439,   805,  1759,  1849,   672,   211,    85,   482,
      77,   528,   269,   142,   447,   447,  1507,    69,   749,   670,
     813,   255,   273,   107,   420,  1300,  1300,  1300,   637,   638,
     820,   447,  1043,  1560,  1729,    76,   657,   649,    76,   231,
     658,   482,  1637,  1301,  1301,  1301,  -396,  1024,    74,    75,
     243,  1059,  -395,  1730,  1153,    13,    14,    15,    16,    17,
    1164,  1733,  1192,   864,    77,  1168,  1615,  1616,  1617,  1188,
      76,  1584,   639,   640,   865,   866,  1176,    54,  1882,    77,
     271,   592,    76,  1883,   365,   365,   592,    77,  1934,   592,
     386,   630,   490,  1935,   791,  1816,   482,   447,   631,   632,
      77,   937,   273,    77,   175,   274,  1504,    77,   592,   369,
      54,  -619,    69,  -338,   833,  1174,   274,  1367,   186,    76,
     514,    76,  1175,  1693,  1383,  1694,   399,   232,   233,   452,
     234,  1188,   671,    76,   235,   108,   672,   556,   305,   420,
      76,   482,    76,    74,    75,  1522,   288,  1034,  1035,    69,
      77,  1036,  1862,  1225,   402,    77,    76,    76,    76,  1000,
    1870,   420,   420,   672,   482,   844,  1074,  1259,   395,  1618,
     718,  1666,   403,   482,   719,   720,    76,   937,   832,   658,
      74,    75,   833,   870,   261,    77,    77,   871,   262,   185,
      60,   265,   452,   267,   515,  1561,  1652,   450,  1664,   322,
      77,    13,    14,    15,    16,    17,   969,  1902,   527,    60,
     568,  1689,    76,    76,   765,  1430,  1431,   421,   103,  1265,
     422,  1652,   153,    77,    77,   154,   155,   685,   156,  1445,
    1446,   971,   454,    84,  1221,   568,  1146,   972,   983,  1038,
      77,   973,   482,  1039,   365,   953,   273,   490,    91,    77,
     482,   482,    92,  1065,  1067,   146,    54,   871,   871,   423,
    1448,    76,    77,  1468,  1469,  1751,  1752,  1753,    76,   424,
     955,  1284,  1245,   749,   765,  1135,   965,   871,   425,   531,
     426,   556,   536,   455,  1704,   482,  1187,  1754,   460,   575,
    1188,   365,  1751,  1864,  1753,   447,  1755,  1366,  1428,    77,
      77,   658,  1188,   456,   871,    76,    92,    13,    14,    15,
      16,    17,   107,  1063,  1865,   238,     6,     7,     8,     9,
      10,    11,    12,  -174,  1071,   463,  1556,  1073,   261,   190,
    1557,  1076,  1646,   464,   243,  1647,  1188,    76,   465,   871,
    1667,    76,    99,  1668,   871,   515,    76,  1039,  -397,  1669,
    1734,  1810,   712,   871,   658,   871,  1786,    13,    14,    15,
      16,    17,    54,   898,   899,   900,  1375,  1376,  1377,    76,
    1481,   978,   466,  1378,  1379,    76,    76,   187,   596,  1886,
    1936,   401,   401,  1188,   871,   386,   282,  1414,   479,  1963,
     480,  1970,    92,  1960,   190,  1971,    13,    14,    15,    16,
      17,  1785,  1360,   498,   369,  1909,   499,   627,   509,  1913,
     765,   844,    54,    76,  -110,  -110,  -110,  -110,  -110,  -110,
     204,    69,    77,   519,   108,  1470,   633,   634,    13,    14,
      15,    16,    17,   668,   365,   635,   636,  1844,   428,  1306,
     559,   649,  1664,   243,   538,   482,    77,  1609,  1610,   573,
    1060,    54,    74,    75,  1461,   641,   642,   261,   262,  -386,
     689,   600,   267,  1292,  1293,  1294,   766,   660,   596,   765,
     408,   103,    76,    76,    76,   965,    77,  1524,  1525,  1526,
    1527,  1528,  -386,    54,   574,    77,    84,  1451,   502,   578,
    1859,   505,  1374,  1497,   873,   874,   765,  1015,  1016,   601,
     103,    91,    76,  1017,  1018,    77,  1859,   602,   447,   447,
      76,    77,  -399,    76,    76,    84,   629,    76,   643,   248,
     258,  1177,  1039,   644,   251,  1190,  1191,    76,  1135,    92,
      91,   871,  1195,    13,    14,    15,    16,    17,    76,  1017,
    1358,   645,  1898,   646,  -400,   989,  1382,   656,   505,   142,
     660,    77,   705,  1695,   765,  1389,  1390,  1135,   692,  1697,
      76,  -847,    77,  -564,    77,   107,   136,   703,  1837,  -109,
    -109,  -109,  -109,  -109,  -109,  1708,  1709,   716,   136,   905,
     906,   907,   908,   596,  1290,  1394,  1390,   721,    54,  1403,
    1390,  -145,  -145,   190,   107,    99,   722,  1315,    76,   723,
     261,   989,  1472,  1569,   871,   724,  1715,  1604,  1382,   879,
     880,   725,   255,   883,  1334,  1691,  1039,   890,  1692,  1390,
     894,   726,    76,  1498,    99,  1712,  1713,   699,  1723,   871,
     427,   386,    -3,  1194,  1772,  1773,  1779,  1390,   753,    76,
    -398,   243,  1780,  1390,   775,  1655,   798,    69,  1855,  1856,
     369,  1306,  1306,  1306,   788,  1483,  1306,   281,   469,  1738,
    1739,  1960,  1961,   789,   470,   927,   790,   671,   932,   834,
    1655,   672,  1387,  1388,   471,  1497,   792,   108,    74,   673,
     735,    76,   793,   477,   144,    76,   472,   794,    76,   473,
     901,   902,  1632,   800,   474,   903,   904,   816,  1602,  1603,
     957,   475,    85,   909,   910,  -512,   108,  -510,   765,   825,
    1640,  1640,  1368,  1369,  1147,   846,    58,   515,   848,   164,
     165,    61,    62,    63,    64,    65,    66,    67,   765,   872,
      76,   875,   878,   895,   928,   943,   517,   945,   674,   967,
     365,   962,   974,  1678,   975,   977,  1261,   996,   988,    77,
     989,    77,  -694,   -16,   -17,  1030,  1045,  1644,  1031,   765,
    1046,  1047,  1048,  1049,    76,    71,  1138,  1050,  1066,    76,
      76,    76,  1028,  1068,  -595,   386,  1077,  1078,  1079,  1157,
    1139,  1451,  1159,  1160,  1161,  1162,  1042,    77,  1044,  1163,
    1165,   146,   955,  1166,   369,  1167,  1172,    92,  1154,  1173,
    1197,   781,  1198,  1199,  1825,  1498,    77,   782,  1212,  1633,
    1213,  1214,  1224,   478,  1229,  1267,  1146,   783,  1228,   136,
    1233,  1239,  1506,  1508,  1240,  1241,  1250,   795,  1242,   784,
    -583,  -582,   785,  1451,  1285,  1273,  -695,   786,  1307,  1308,
    1318,  1321,  1083,   685,   787,  1322,  1331,  1338,   136,  1332,
     249,   259,  1333,  1340,   447,   447,  1342,  -618,   515,   871,
    1544,  1348,  1825,    76,  1352,  1355,   136,  1354,  1356,    76,
    1380,    76,  1425,  1919,  1426,  1382,  1362,  1447,    76,  1364,
    1429,  1434,  1452,   492,  1453,  1455,  1464,  1374,  1454,  1473,
     765,  1183,   765,  1474,  1476,   103,  1486,   103,  1183,  1488,
      58,  1311,  1489,  1510,   766,    61,    62,    63,    64,    65,
      66,    67,  1397,  1511,  1513,    13,    14,    15,    16,    17,
     103,  1539,  1515,  1516,   765,  1518,  1537,  1497,  1520,   603,
    1532,   604,   605,   606,   114,  1183,  1542,   114,  1540,  1777,
    1146,  1541,  1547,  1549,    76,  1550,    76,  1552,  1243,    71,
    1555,  1553,  1657,  1554,  1657,    85,  1558,   796,  1562,  1571,
     607,  1570,  1586,   608,   609,    76,  1573,  1220,   610,   611,
      54,  1574,  1575,  1836,  1577,  1582,  1588,  1657,  1592,  1801,
    1593,   420,  1614,  1627,    85,  1261,  1605,    58,   114,   107,
    1390,   107,    61,    62,    63,    64,    65,    66,    67,    77,
    1607,   241,   252,  1491,  1648,    77,    77,  1673,  1675,  1680,
    1690,   114,    76,  1702,   107,    76,   205,   655,  1696,    99,
    1700,    99,  1701,  1451,  1703,   765,   136,  1706,  1707,    69,
    1710,   114,  1711,  1721,  1717,  1722,    71,  1604,  1670,  1027,
     766,  1746,   469,  1028,    99,   447,   251,   765,   470,  1618,
    1764,  1760,  1766,   482,  1101,  1770,  1771,  1498,   471,  1778,
      74,    75,  1783,    76,    76,  1787,   477,   518,   114,  1784,
     472,  -584,   365,   473,   114,  1792,   114,  1793,   474,  1876,
     729,   482,    76,   732,  1794,   475,    92,  1795,  1796,  1797,
     114,  1805,  1809,  1812,  1842,  1893,  1801,  1820,  1148,   795,
    1828,   108,    78,   108,  1843,   143,  1821,  1845,   114,    13,
      14,    15,    16,    17,   868,    76,  1829,  1713,  1846,  1847,
     114,   765,  1911,  1860,  1827,   765,   108,  -495,  1867,  1879,
     492,  1880,  1881,  1884,   255,  1885,   765,  1888,  1891,  1899,
     536,   103,  1900,    77,    77,  1906,  1912,  1910,  1917,   765,
      13,    14,    15,    16,    17,  1169,    78,    77,   136,  1918,
    1924,  1925,   145,   243,  1930,  1931,  1933,  1942,  1370,  1948,
     114,   176,  1953,   114,   408,   408,  1949,  1954,   114,    78,
    1957,  1969,  1827,  1958,   766,  1968,    18,   599,  1676,   103,
    1183,  1972,   215,   553,  1962,   240,   478,  1687,  1657,    78,
     179,   180,   911,  1393,    76,   103,    76,   181,   913,   867,
     912,   114,   915,   831,  1337,   914,  1344,   765,   514,   765,
     765,    44,    45,    46,    47,    48,    49,    50,    51,   796,
     114,  1943,  1677,  1894,  1762,   107,   143,  1904,  1866,  1840,
    1819,   103,    78,   136,   143,   136,  1657,   291,   297,  1939,
    1928,  1937,    77,    76,    76,   515,  1671,  1872,   342,   368,
     192,   114,  1657,  1672,   765,    99,  1914,  1955,   136,  1183,
    1871,  1353,   163,  1800,   765,   114,   342,   176,   176,   508,
    1630,  1485,   136,   107,  1853,  1641,  1350,    76,   143,   433,
    1266,  1589,   240,   451,  1147,  1037,  1156,  1683,  1657,   107,
     765,     3,   765,   850,     0,  1230,   919,   655,   655,     0,
       0,    77,     0,    99,     0,   215,   215,   765,   920,     0,
     921,     0,   765,     0,     0,     0,     0,  1767,     0,    99,
       0,  1302,   291,     0,     0,   107,   765,     0,   766,    92,
      76,    78,     0,     0,     0,     0,     0,   108,     0,     0,
      76,     0,  1320,     0,   240,     0,     0,     0,     0,     0,
      77,     0,   114,     0,     0,    99,     0,     0,    92,     0,
       0,     0,     0,     0,     0,    18,     0,     0,     0,    85,
     981,    85,     0,   984,   297,     0,     0,     0,   136,   766,
     297,   291,   291,     0,  1183,   108,     0,     0,   143,  1183,
    1183,  1183,     0,     0,    85,     0,     0,     0,     0,     0,
       0,   108,     0,     0,    48,    49,    50,    51,  1147,     0,
       0,  1831,     0,     0,     0,     0,     0,     0,     0,   342,
       0,   114,   114,     0,     0,     0,     0,     0,     0,     0,
    1950,     0,   492,   342,     0,     0,  1053,   108,     0,     0,
    1057,     0,     0,     0,     0,   408,    58,     0,     0,  1959,
     650,    61,    62,    63,    64,    65,    66,    67,   888,   368,
     675,   684,     0,   114,     0,     0,     0,   114,     0,   114,
       0,     0,   831,     0,     0,     0,   368,     0,     0,   136,
     368,     0,     0,    77,     0,     0,     0,     0,     0,    77,
       0,    77,     0,     0,   114,    71,   114,   114,   889,   114,
       0,     0,   114,     0,   176,   114,   114,   114,     0,     0,
       0,     0,     0,   241,   252,     0,     0,     0,     0,     0,
     143,  1831,     0,   441,   433,  1831,  1831,   136,   742,     0,
     684,     0,     0,  1302,  1302,  1302,   146,  1480,  1484,     0,
       0,     0,     0,   136,   238,     6,     7,     8,     9,    10,
      11,    12,  1932,     0,     0,     0,     0,     0,   215,     0,
       0,   408,   408,     0,  1183,     0,  1183,   215,     0,     0,
     655,     0,     0,  1951,     0,     0,     0,  1951,     0,   136,
       0,   114,   178,     0,     0,     0,   831,   291,   408,   342,
     342,     0,     0,   291,     0,   368,     0,     0,     0,     0,
     260,  1543,  1965,   218,     0,   514,     0,   831,     0,     0,
       0,     0,    58,  1929,     0,    85,     0,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,   114,     0,
       0,    58,    77,   291,     0,    77,    61,    62,    63,    64,
      65,    66,    67,  1180,   291,     0,   291,  1181,   368,  1182,
       0,     0,     0,     0,   831,   831,     0,     0,   293,     0,
     408,    71,   342,    85,   143,   143,     0,   342,  1148,     0,
     342,     0,     0,   143,   143,   143,     0,     0,     0,    85,
      71,     0,     0,  1385,    77,     0,     0,     0,   433,     0,
       0,     0,     0,  1276,     0,     0,  1280,     0,     0,     0,
       0,     0,     0,   665,     0,     0,   688,     0,     0,   231,
       0,     0,     0,     0,     0,    85,     0,     0,     0,     0,
     665,     0,     0,     0,   665,     0,   468,   218,     0,     0,
       0,     0,     0,     0,     0,    77,     0,     0,     0,     0,
     650,   650,     0,   293,     0,     0,     0,     0,     0,   342,
       0,   368,   433,    92,   684,    92,     0,     0,   114,     0,
       0,     0,   675,  1649,     0,  1658,   675,     0,   665,     0,
     114,   114,     0,     0,     0,   368,     0,     0,    92,     0,
       0,     0,     0,  1222,     0,   684,     0,     0,   368,     0,
       0,     0,  1148,     0,     0,     0,   143,  1681,   591,     0,
       0,     0,   554,   293,    58,     0,     0,   212,   213,    61,
      62,    63,    64,    65,    66,    67,   238,     6,     7,     8,
       9,    10,    11,    12,    77,     0,  1878,     0,   433,     0,
       0,   742,    69,   742,     0,     0,     0,     0,     0,   441,
       0,     0,     0,     0,     0,  1401,     0,     0,     0,     0,
     368,   368,  1490,    71,     0,     0,     0,     0,     0,  1491,
       0,     0,     0,    74,    75,     0,     0,   368,     0,   291,
       0,     0,    58,    77,    77,   164,   165,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,  1437,   291,   244,
       0,     0,   441,     0,     0,     0,     0,     0,  1737,     0,
     264,     0,   831,   831,     0,     0,     0,    77,     0,     0,
       0,     0,     0,     0,     0,     0,   831,   831,     0,     0,
    1763,    71,     0,     0,     0,    54,   342,     0,     0,     0,
       0,     0,   441,   368,     0,     0,     0,     0,   143,   342,
       0,     0,   244,     0,     0,     0,     0,     0,     0,   743,
     831,   831,     0,     0,   433,     0,    58,     0,     0,     0,
    1966,    61,    62,    63,    64,    65,    66,    67,   807,   809,
    1973,     0,     0,     0,     0,     0,     0,     0,     0,   780,
       0,     0,     0,     0,    69,     0,   244,     0,   218,     0,
       0,     0,     0,     0,  1807,   665,   441,     0,  1811,    92,
       0,     0,     0,   114,    70,    71,     0,     0,   293,  1818,
       0,   114,     0,   650,   293,    74,    75,     0,     0,   665,
       0,     0,  1835,     0,     0,   368,     0,  1206,     0,     0,
       0,     0,   665,     0,     0,     0,     0,     0,   675,     0,
     114,   591,     0,     0,     0,     0,   591,    92,   244,   591,
       0,     0,     0,     0,   293,     0,     0,     0,   114,   300,
     301,   302,   303,    92,     0,   843,     0,   293,   591,     0,
       0,     0,   441,     0,     0,     0,     0,   114,   244,     0,
       0,   742,     0,     0,   244,   114,     0,     0,   742,     0,
    1887,   707,  1889,  1890,     0,     0,     0,    58,     0,    92,
     164,   165,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   917,     0,
       0,     0,     0,  1345,     0,     0,     0,     0,  1622,     0,
       0,   368,     0,     0,   831,   831,     0,  1915,     0,     0,
       0,     0,     0,     0,     0,     0,    71,  1920,    58,   304,
       0,   164,   165,    61,    62,    63,    64,    65,    66,    67,
       0,     0,   708,     0,     0,   244,     0,   305,     0,     0,
    1645,   143,     0,  1941,     0,  1920,     0,   709,     0,   342,
     710,   711,    61,    62,    63,    64,    65,    66,    67,     0,
    1956,     0,     0,     0,    58,  1941,     0,    71,   441,    61,
      62,    63,    64,    65,    66,    67,  1180,     0,   342,  1964,
    1181,     0,  1182,     0,     0,   114,   114,   114,   114,   114,
     114,  1346,     0,     0,     0,   240,    78,     0,     0,     0,
       0,     0,     0,     0,   244,     0,     0,     0,     0,   291,
     831,     0,     0,    71,     0,   143,   831,     0,     0,   433,
       0,     0,   991,   143,   743,     0,     0,     0,     0,     0,
       0,     0,   831,   831,     0,   110,  1622,  1622,   110,   665,
       0,     0,   688,     0,     0,     0,     0,     0,    58,   167,
     170,   164,   165,    61,    62,    63,    64,    65,    66,    67,
     293,     0,     0,     0,    58,     0,     0,   433,   244,    61,
      62,    63,    64,    65,    66,    67,  1180,     0,     0,   293,
    1181,     0,  1182,   207,     0,     0,     0,     0,   244,   110,
       0,     0,    13,    14,    15,    16,    17,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   244,     0,
       0,     0,   110,    71,   368,   368,  1565,     0,     0,     0,
     114,  1323,     0,     0,     0,     0,     0,     0,   246,     0,
       0,     0,   110,   285,     0,     0,   286,     0,     0,     0,
       0,     0,     0,     0,     0,  1622,     0,    54,     0,   306,
       0,     0,     0,   143,   143,   143,   143,   143,   143,     0,
       0,     0,     0,  1492,   297,     0,     0,     0,     0,   110,
       0,     0,     0,     0,     0,   110,     0,   110,    58,   114,
       0,   246,     0,    61,    62,    63,    64,    65,    66,    67,
       0,   360,   110,   396,     0,     0,     0,     0,     0,     0,
       0,     0,   461,     0,     0,   114,    69,   114,     0,   360,
       0,     0,   240,  1851,     0,     0,     0,  1622,     0,     0,
       0,   110,   360,     0,     0,   246,    70,    71,   244,     0,
     114,     0,     0,     0,     0,     0,     0,    74,    75,     0,
       0,     0,     0,  1622,   114,     0,     0,   512,     0,     0,
     114,     0,   244,     0,     0,     0,     0,   167,     0,   244,
       0,     0,     0,   441,     0,   433,     0,     0,   167,     0,
       0,   110,   991,     0,   110,     0,     0,     0,  1244,   743,
       0,     0,     0,     0,     0,     0,     0,   246,   143,     0,
       0,     0,  1622,  1622,     0,   558,     0,     0,    54,     0,
       0,   560,   562,     0,   532,     0,   569,     0,     0,     0,
       0,   665,   110,     0,     0,     0,     0,   246,     0,     0,
       0,     0,     0,   246,     0,     0,  1622,     0,     0,    58,
     831,   110,   212,   213,    61,    62,    63,    64,    65,    66,
      67,  1619,     0,     0,     0,  1492,     0,   342,     0,     0,
     114,  1492,     0,  1492,     0,     0,     0,    69,     0,     0,
       0,    58,   360,     0,   164,   165,    61,    62,    63,    64,
      65,    66,    67,   342,     0,   342,   360,  1874,    71,     0,
       0,   482,     0,     0,     0,     0,     0,     0,    74,    75,
       0,   701,   244,     0,     0,     0,   306,     0,   342,   306,
       0,     0,   110,   244,   246,   110,     0,     0,     0,   368,
      71,     0,   143,     0,     0,     0,     0,     0,   143,   110,
       0,     0,     0,   110,     0,     0,     0,     0,     0,     0,
     293,  1395,     0,   207,  1325,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   757,   758,    54,     0,     0,
       0,   114,     0,     0,     0,     0,     0,     0,     0,     0,
     368,   368,     0,   110,     0,     0,     0,   360,     0,     0,
       0,     0,     0,   246,     0,     0,     0,     0,    58,  1619,
    1619,   212,   213,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,     0,  1492,     0,     0,  1492,     0,   114,
       0,   244,     0,     0,     0,     0,     0,     0,     0,   441,
       0,     0,     0,     0,     0,   114,     0,   297,   143,     0,
       0,     0,     0,     0,     0,  1442,  1243,    71,     0,     0,
       0,     0,   360,   360,    58,     0,     0,   246,   110,    61,
      62,    63,    64,    65,    66,    67,   291,     0,     0,     0,
       0,   114,     0,     0,     0,     0,     0,     0,  1509,     0,
       0,     0,    69,     0,     0,   306,     0,  1517,     0,   110,
       0,  1521,     0,  1523,   110,     0,     0,   246,   110,     0,
     110,     0,   990,    71,  1496,     0,   672,     0,  1619,     0,
       0,   110,     0,    74,    75,     0,     0,  1492,    54,     0,
       0,     0,     0,     0,     0,   360,     0,   110,   110,   297,
     360,     0,     0,   360,     0,     0,   110,   110,   110,   342,
       0,     0,     0,     0,     0,     0,   143,     0,     0,    58,
       0,   360,   212,   213,    61,    62,    63,    64,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   368,     0,     0,     0,     0,     0,    69,     0,     0,
    1619,     0,     0,     0,     0,    58,     0,   143,   164,   165,
      61,    62,    63,    64,    65,    66,    67,   214,    71,     0,
       0,     0,     0,   143,   143,     0,  1875,   297,    74,    75,
       0,     0,   360,   396,   110,   360,     0,   246,    58,   244,
     701,   527,    60,    61,    62,    63,    64,    65,    66,    67,
       0,     0,  1613,     0,    71,     0,     0,     0,   110,   143,
       0,   246,     0,     0,  1327,   532,     0,     0,   246,     0,
       0,   110,     0,   961,     0,  1875,  1875,     0,     0,   110,
       0,     0,     0,     0,     0,    54,    94,    71,     0,   147,
     926,  1014,  1650,     0,  1661,     0,     0,     0,  1026,     0,
       0,     0,     0,     0,     0,     0,  1496,     0,     0,  1875,
       0,   360,  1634,     0,  1496,   244,    58,  1650,     0,   212,
     213,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,     0,   110,   110,     0,     0,     0,     0,     0,
      94,     0,     0,     0,    69,     0,     0,     0,    58,     0,
     110,   392,   393,    61,    62,    63,    64,    65,    66,    67,
       0,     0,     0,   191,   290,    71,     0,     0,     0,     0,
       0,  1085,     0,     0,     0,    74,    75,     0,   110,     0,
       0,     0,     0,   253,    58,     0,     0,   164,   165,    61,
      62,    63,    64,    65,    66,    67,     0,    71,     0,    72,
       0,   246,     0,     0,   394,     0,     0,     0,   244,   360,
     665,     0,   246,     0,     0,     0,   110,     0,     0,     0,
     283,   110,   360,     0,     0,     0,    94,     0,  1745,     0,
       0,   242,  1747,    71,     0,     0,     0,   360,     0,  1749,
     561,     0,   263,   370,   266,     0,   268,     0,     0,     0,
       0,     0,     0,     0,     0,  1735,   244,     0,  1496,     0,
       0,   413,     0,     0,     0,     0,     0,     0,   665,     0,
       0,    58,   283,   439,   212,   213,    61,    62,    63,    64,
      65,    66,    67,     0,   242,     0,   266,   268,     0,     0,
       0,     0,   110,     0,     0,     0,     0,     0,     0,  1223,
     476,     0,     0,     0,   701,     0,     0,   293,   110,     0,
    1208,   360,     0,   110,     0,     0,   496,     0,     0,     0,
      71,   501,   503,     0,     0,   191,     0,  1219,   242,     0,
       0,     0,     0,     0,     0,  1815,  1817,     0,  1661,     0,
       0,     0,     0,    54,     0,     0,     0,   523,     0,     0,
     525,     0,   526,     0,     0,     0,     0,     0,  1496,     0,
     110,   110,     0,   543,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,   555,   212,   213,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
     242,     0,   266,   268,  1863,     0,     0,     0,     0,     0,
      58,     0,    69,     0,     0,    61,    62,    63,    64,    65,
      66,    67,  1180,     0,   110,     0,  1181,     0,  1182,     0,
     242,     0,  1490,    71,     0,     0,   242,     0,     0,     0,
    1324,  1326,  1328,    74,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1901,   293,  1903,    71,
       0,     0,  1567,   663,   110,     0,   687,     0,     0,     0,
    1347,     0,   360,     0,     0,     0,     0,   104,  1916,     0,
     694,     0,     0,    58,   694,  1085,   185,    60,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,   360,     0,     0,     0,     0,   554,   293,     0,     0,
       0,     0,     0,     0,  1944,  1946,  1947,   242,   246,   110,
       0,     0,     0,   690,   283,   268,     0,     0,   663,     0,
       0,   104,    71,     0,     0,  1027,     0,     0,   110,     0,
     293,     0,   360,     0,    58,     0,   110,   212,   213,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
     242,     0,     0,     0,     0,     0,   701,     0,     0,     0,
       0,   244,    69,    58,   254,     0,     0,     0,    61,    62,
      63,    64,    65,    66,    67,   881,   242,     0,   690,   268,
     360,     0,   740,    71,  1208,     0,   672,     0,     0,   439,
       0,     0,     0,    74,   741,     0,  1422,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   674,   104,   110,   110,
       0,     0,    71,   242,     0,   882,     0,     0,     0,     0,
     827,     0,   110,   110,   374,   503,     0,   110,   110,   838,
       0,   543,     0,   242,     0,     0,     0,     0,   242,     0,
     242,     0,   370,  1501,     0,     0,  1503,     0,     0,     0,
       0,     0,     0,   244,   440,     0,   110,   110,     0,     0,
     242,     0,   242,   242,     0,     0,   110,   110,   110,   110,
     110,   110,     0,     0,     0,     0,     0,   246,     0,     0,
     242,     0,   439,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   242,     0,     0,     0,     0,     0,     0,    58,
       0,     0,   212,   213,    61,    62,    63,    64,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,   246,     0,    69,   524,     0,
       0,   244,     0,     0,     0,   922,     0,     0,     0,     0,
       0,     0,     0,     0,   104,   694,   936,  1490,    71,     0,
       0,     0,     0,     0,  1491,     0,     0,     0,    74,    75,
     947,     0,     0,     0,     0,     0,     0,     0,     0,   663,
       0,     0,     0,    54,   956,     0,     0,     0,   360,     0,
       0,     0,   694,     0,     0,   317,     0,   318,     0,   319,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
     242,   110,   690,   268,    58,     0,     0,   212,   213,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,   936,     0,   242,   690,     0,   997,     0,     0,
       0,   242,    69,     0,   664,   598,  1642,   254,    72,   405,
     110,   110,     0,     0,   439,   439,     0,     0,     0,     0,
       0,   664,   740,    71,     0,   664,   672,     0,     0,     0,
     360,   439,     0,    74,   741,   612,   613,   614,   615,   616,
     617,   618,   619,   620,   621,   622,   110,     0,     0,   200,
       0,     0,     0,     0,     0,     0,   360,    58,   360,   827,
     529,   530,    61,    62,    63,    64,    65,    66,    67,   664,
      13,    14,    15,    16,    17,     0,   623,     0,     0,     0,
       0,   360,     0,     0,     0,     0,     0,     0,     0,     0,
    1134,     0,   110,   701,     0,   110,     0,   439,     0,     0,
       0,   110,   147,     0,     0,     0,    71,     0,    72,     0,
       0,     0,     0,     0,     0,     0,   110,     0,   370,     0,
       0,     0,   110,     0,     0,    54,     0,     0,     0,     0,
       0,     0,     0,     0,   242,     0,     0,     0,   110,   110,
     440,     0,     0,   110,   110,   242,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,   212,
     213,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,   374,     0,   827,     0,     0,     0,     0,     0,     0,
     254,     0,   104,     0,    69,     0,     0,     0,     0,   694,
       0,  1776,  1210,   440,   827,     0,     0,     0,     0,  1216,
     246,   110,     0,     0,  1874,    71,     0,     0,   482,     0,
     701,     0,     0,     0,     0,    74,    75,    58,     0,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   242,
       0,     0,     0,   440,     0,     0,     0,     0,     0,     0,
       0,   827,   827,   242,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   242,     0,     0,   109,     0,     0,
       0,     0,     0,   242,     0,     0,    71,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,   189,    58,     0,
       0,   212,   213,    61,    62,    63,    64,    65,    66,    67,
       0,     0,   246,     0,     0,   439,   664,   440,     0,     0,
       0,     0,   360,     0,   242,     0,    69,     0,     0,   110,
       0,   109,     0,     0,    13,    14,    15,    16,    17,     0,
     664,     0,     0,     0,     0,     0,  1874,    71,     0,     0,
     482,     0,     0,   664,   110,  1303,     0,    74,    75,     0,
       0,     0,   189,  1134,     0,     0,     0,     0,     0,     0,
     110,     0,     0,     0,   256,     0,   189,   189,     0,     0,
       0,     0,     0,     0,     0,     0,   110,   110,     0,    54,
     246,     0,  1134,   440,   189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   436,     0,     0,
    1351,     0,     0,     0,     0,   374,   374,   109,     0,     0,
      58,     0,   110,   212,   213,    61,    62,    63,    64,    65,
      66,    67,   374,   370,   378,    58,     0,     0,   212,   213,
      61,    62,    63,    64,    65,    66,    67,     0,    69,     0,
       0,     0,     0,     0,     0,     0,   110,     0,     0,   189,
     374,   242,     0,    69,   442,     0,     0,     0,   214,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
      75,   663,     0,   214,    71,     0,     0,     0,     0,     0,
     501,   104,     0,     0,    74,    75,    58,     0,   374,   164,
     165,    61,    62,    63,    64,    65,    66,    67,     0,   827,
     827,    13,    14,    15,    16,    17,   189,     0,     0,   440,
       0,     0,     0,   827,   827,     0,     0,     0,   439,   439,
       0,     0,     0,     0,     0,     0,     0,   242,     0,     0,
       0,     0,     0,   242,   450,    71,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,   827,   827,     0,
       0,   189,     0,     0,     0,     0,    54,  1303,  1303,  1303,
     147,     0,     0,     0,   374,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,   189,     0,     0,
     664,     0,     0,   254,     0,   374,     0,    58,     0,     0,
     212,   213,    61,    62,    63,    64,    65,    66,    67,     0,
      58,     0,     0,   164,   165,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,    69,     0,     0,     0,     0,
     242,     0,     0,     0,     0,    54,     0,     0,     0,     0,
       0,     0,   374,   374,   666,   290,    71,   256,     0,     0,
       0,     0,   189,     0,     0,     0,    74,    75,   454,    71,
       0,   666,     0,     0,     0,   666,    58,     0,     0,   212,
     213,    61,    62,    63,    64,    65,    66,    67,   242,   370,
      58,     0,     0,   212,   213,    61,    62,    63,    64,    65,
      66,    67,     0,     0,    69,     0,   374,     0,     0,     0,
       0,     0,   147,     0,     0,     0,     0,     0,    69,   666,
       0,     0,     0,     0,  1490,    71,     0,   189,   189,     0,
       0,     0,     0,   436,     0,    74,    75,     0,   290,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
      75,   827,   827,    58,   104,     0,   164,   165,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1636,     0,     0,
       0,     0,     0,   104,     0,     0,   189,   827,     0,     0,
     442,     0,     0,     0,     0,     0,     0,  1656,     0,  1656,
       0,   254,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   378,  1656,     0,   440,     0,   436,     0,     0,     0,
     256,     0,   109,   370,     0,     0,   147,     0,     0,     0,
       0,     0,     0,   442,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   827,     0,     0,
       0,     0,     0,   827,     0,     0,     0,     0,     0,     0,
       0,     0,   664,     0,     0,     0,     0,     0,     0,   827,
     827,     0,     0,   442,   439,   439,     0,   189,     0,     0,
     436,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   374,     0,     0,  1727,     0,     0,     0,     0,     0,
       0,     0,     0,   189,   374,   374,     0,     0,     0,   374,
     374,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   189,     0,     0,     0,     0,     0,
       0,     0,  1748,     0,     0,     0,   666,   442,   374,   374,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   436,     0,     0,     0,
     666,   115,     0,   242,   115,     0,     0,     0,     0,     0,
       0,     0,     0,   666,     0,     0,     0,     0,   436,   436,
       0,     0,     0,   242,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   436,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   442,     0,   115,     0,     0,     0,     0,
       0,     0,   119,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,  1656,     0,   378,   378,     0,   115,     0,
    1826,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   378,     0,   242,     0,     0,     0,   115,     0,
     440,   436,     0,     0,     0,   439,     0,   189,     0,     0,
       0,     0,     0,     0,     0,   242,   119,     0,     0,     0,
     378,  1656,   189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,  1656,  1826,   119,
       0,   115,     0,   115,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,     0,     0,     0,   115,   378,   119,
       0,     0,   374,   374,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1656,     0,   115,     0,     0,     0,   442,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
    1908,     0,     0,   242,     0,     0,   119,     0,   374,     0,
       0,     0,   119,     0,   119,     0,     0,   827,   104,     0,
     104,     0,     0,     0,     0,     0,     0,     0,   119,     0,
    1215,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,   104,   378,     0,   119,   115,     0,     0,
     115,     0,     0,     0,   374,   115,     0,     0,   119,     0,
     666,     0,     0,   256,     0,   378,     0,     0,     0,     0,
     312,     0,     0,     0,   313,     0,   314,     0,   374,     0,
       0,     0,     0,     0,   374,   242,     0,     0,   115,     0,
       0,     0,    54,   315,     0,     0,     0,     0,     0,     0,
     374,   374,     0,     0,     0,   374,   374,   115,   119,   436,
       0,   119,   378,   378,     0,     0,   119,     0,     0,     0,
     316,   317,     0,   318,     0,   319,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   320,   321,   322,   115,   323,
     324,   325,     0,   326,   327,     0,     0,     0,     0,   119,
       0,    69,   115,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   254,     0,     0,   378,     0,   119,     0,
       0,   328,     0,     0,    72,   405,     0,     0,     0,     0,
       0,   330,   432,    75,   331,   332,   333,   334,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   109,     0,     0,     0,     0,     0,
       0,     0,     0,   189,     0,     0,     0,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   256,     0,     0,   104,     0,     0,     0,     0,     0,
       0,   664,     0,     0,     0,   189,     0,     0,     0,     0,
       0,     0,     0,     0,   442,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,   115,   115,
       0,     0,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   436,   436,     0,     0,     0,     0,   104,   664,
       0,     0,   666,     0,     0,     0,     0,     0,   406,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,   115,     0,   115,     0,     0,     0,
     378,   378,     0,     0,   104,     0,     0,     0,     0,   119,
     119,     0,     0,     0,   378,   378,     0,     0,     0,   378,
     378,   115,     0,   115,   115,     0,   115,     0,     0,   115,
       0,     0,   115,   115,   115,     0,     0,     0,   374,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   378,   378,
       0,   119,     0,     1,     0,   119,   141,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   119,   119,     0,   119,     0,     0,
     119,     0,     0,   119,   119,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,   189,     0,     0,   572,     0,     0,   406,
     577,     0,     0,     0,     0,     0,   188,     0,     0,     0,
     580,   581,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   406,   406,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     442,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   278,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   406,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   378,   378,     0,     0,     0,   406,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   378,     0,
       0,     0,     0,     0,     0,     0,     0,   278,   109,     0,
     109,     0,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,   504,     0,     0,     0,     0,   115,   115,     0,
       0,     0,   278,   109,     0,     0,     0,     0,   436,   436,
       0,     0,   278,     0,   378,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   535,   539,     0,     0,
       0,     0,     0,   546,   547,     0,     0,     0,   378,     0,
       0,     0,     0,     0,   378,     0,   119,     0,     0,   557,
       0,     0,     0,     0,     0,     0,   199,     0,   119,   119,
     378,   378,   209,   210,     0,   378,   378,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   597,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   272,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     661,     0,     0,   256,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   406,   406,   406,   406,   406,   406,   406,
     406,   406,   406,   406,   406,   406,   406,   406,   406,   406,
     406,   406,     0,     0,     0,     0,     0,     0,   717,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     734,     0,     0,     0,   737,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   436,
       0,     0,     0,   759,     0,     0,     0,   760,   761,     0,
       0,     0,     0,     0,     0,     0,   776,   777,   778,   779,
       0,     0,     0,     0,   109,     0,     0,     0,     0,     0,
     115,   666,     0,     0,     0,   801,     0,     0,   115,     0,
       0,     0,     0,   804,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   378,   406,     0,     0,
       0,   551,     0,     0,     0,     0,     0,   115,     0,     0,
       0,   278,   109,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,   109,   666,
       0,   119,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   842,     0,   115,     0,     0,     0,     0,   535,
       0,     0,   115,     0,   847,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   109,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   863,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   378,     0,
       0,     0,     0,   406,     0,   119,     0,     0,   406,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,   406,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   918,     0,     0,     0,   738,     0,   739,     0,
       0,     0,     0,     0,     0,     0,     0,   755,   756,     0,
     406,     0,     0,     0,   939,   944,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   115,   115,   115,   115,   115,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     986,     0,     0,     0,   987,     0,     0,     0,     0,     0,
       0,     0,     0,   939,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   119,   119,   119,   119,   119,     0,
       0,   237,     0,     0,     0,   837,     0,  1029,     0,     0,
      13,    14,    15,    16,    17,     0,     0,    19,  1040,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,  -402,  -402,     0,  -402,    42,    43,     0,  -402,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     1,     0,     0,    54,     0,   115,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   406,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,  1158,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,   115,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,   296,     0,
       0,     0,   115,   161,   115,    74,    75,     0,     0,   406,
     406,   406,     0,     0,     0,     0,   406,   406,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   115,     0,   161,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   406,
       0,   115,     0,     0,     0,     0,     0,   115,     0,     0,
     406,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,   119,     0,  1012,     0,     0,
       0,     0,  1234,     0,   161,     0,  1235,     0,     0,     0,
       0,     0,     0,   939,     0,     0,     0,   161,   119,   161,
       0,     0,     0,  1248,     0,     0,     0,     0,     0,     0,
    1249,     0,   119,     0,     0,     0,     0,     0,   119,  1253,
       0,  1254,     0,     0,     0,     0,     0,     0,     0,   398,
    1260,     0,     0,     0,     0,     0,     0,     0,  1268,     0,
    1415,     0,     0,   159,     0,     0,   398,    13,    14,    15,
      16,    17,  1081,  1082,  1282,     0,     0,   115,  1283,     0,
       0,     0,     0,  1140,  1141,  1142,     0,     0,  1144,     0,
       0,     0,   141,     0,     0,     1,     0,     0,     0,     0,
     312,     0,     0,   161,   313,     0,   314,   161,     0,     0,
     161,   161,     0,     0,   161,     0,     0,   161,   161,     0,
       0,  1178,    54,   315,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   270,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   276,     0,   277,
     316,   317,     0,   318,     0,   319,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   320,   321,   322,     0,   323,
     324,   325,     0,   326,   327,     0,     0,     0,   115,   161,
       0,    69,   161,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   328,     0,     0,    72,   405,     0,     0,     0,     0,
       0,   330,  1416,    75,   331,   332,   333,   334,     0,     0,
    1232,     0,     0,     0,     0,     0,   115,  1404,     0,     0,
       0,   406,   406,     0,     0,     0,     0,     0,     0,   119,
     486,   487,   115,     0,   491,     0,     0,   494,   495,     0,
       0,  1427,     0,     0,     0,   161,     0,  1251,     0,     0,
       0,     0,     0,     0,     0,     0,  1255,  1256,  1257,  1258,
     161,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,     0,     0,  1272,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1466,     0,     0,
       0,  1467,     0,   119,     0,     0,  1286,     0,  1287,     0,
       0,   161,     0,     0,     0,     0,   406,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1502,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,  1512,  1514,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,     0,
       0,  1343,     0,     0,     0,     0,     0,     0,   161,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1548,     0,     0,  1551,   659,     0,  1357,     0,     0,
       0,     0,     0,  1361,     0,  1363,  1365,     0,     0,     0,
     691,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1572,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1578,     0,     0,     0,     0,     0,     0,
    1398,   728,   398,     0,     0,     0,     0,     0,     0,  1405,
       0,  1406,     0,  1407,     0,  1409,     0,     0,  1594,     0,
    1417,     0,  1595,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1599,  1600,     0,     0,
    1432,  1433,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   797,     0,
       0,     0,     0,     0,     0,     0,     0,  1459,  1460,     0,
       0,     0,   406,     0,     0,  1463,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   161,   161,     0,   209,   406,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   161,   161,
       0,     0,     0,     0,     0,  1684,  1685,     0,     0,     0,
       0,   161,     0,     0,     0,  1538,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   161,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   406,   406,
       0,     0,   161,     0,     0,   161,   161,     0,   161,     0,
     161,   161,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1576,     0,     0,   406,     0,     0,     0,     0,
    1581,     0,  1583,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   924,   925,  1590,  1591,     0,   161,     0,     0,
     406,   161,     0,     0,     0,     0,     0,     0,     0,  1596,
    1597,     0,  1598,     0,     0,     0,  1765,     0,   950,   951,
    1601,     0,     0,     0,     0,     0,  1606,     0,     0,     0,
       0,   958,  1611,  1612,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   406,  1551,     0,
       0,     0,     0,     0,     0,     0,  1790,     0,     0,     0,
       0,   980,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1001,  1806,     0,  1004,  1005,     0,  1008,     0,
    1010,  1011,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1822,     0,     0,  1823,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1052,   385,     0,
       0,  1056,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1698,  1699,     0,     0,     0,     0,     0,
       0,     0,     0,  1705,   161,     0,     0,   429,   385,     0,
       0,     0,     0,     0,     0,     0,  1718,     0,     0,     0,
       0,     0,     0,  1719,  1720,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   485,     0,
       0,     0,  1892,     0,     0,   485,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   161,     0,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,  -402,  -402,
       0,  -402,    42,    43,     0,  -402,     0,     0,     0,     0,
       0,     0,     0,     0,   161,     0,     0,   161,     0,  1781,
       0,  1782,    54,   485,     0,     0,     0,     0,     0,     0,
       0,     0,  1788,  1789,     0,     0,     0,     0,     0,  1791,
       0,     0,     0,     0,  1217,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,    59,    60,    61,    62,
      63,    64,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,   485,
       0,     0,     0,     0,     0,  1217,     0,     0,   385,   676,
       0,    70,    71,     0,    72,   296,     0,     0,     0,     0,
       0,     0,    74,    75,     0,     0,     0,     0,     0,   697,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1861,     0,     0,     0,     0,
       0,     0,  1869,     0,  1275,     0,     0,  1279,  1873,     0,
       0,     0,   485,   730,     0,   485,   733,     0,     0,     0,
       0,     0,     0,   385,     0,     0,   161,   676,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   161,     0,     0,     0,     0,     0,     0,     0,   161,
     161,     0,     0,     0,     0,     0,  1905,     0,   485,     0,
       0,     0,   485,     0,     0,     0,     0,     0,   161,     0,
       0,   161,     0,   161,   161,     0,     0,     0,     0,     0,
    1922,     0,     0,     0,     0,     0,     0,     0,  1927,     0,
       0,     0,     0,     0,   385,     0,     0,     0,     0,     0,
       0,   357,     0,  1940,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   161,     0,     0,     0,     0,   357,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   434,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   485,     0,   458,   385,     0,     0,
       0,     0,     0,     0,     0,     0,  1400,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1411,     0,     0,     0,     0,     0,     0,   196,  1420,
    1421,   161,     0,     0,     0,     0,     0,   385,     0,     0,
       0,     0,     0,     0,   250,     0,     0,     0,  1436,     0,
       0,  1440,     0,  1443,  1444,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   552,     0,     0,     0,     0,     0,     0,   485,
     485,     0,     0,   196,  1471,     0,     0,   298,     0,     0,
     934,   385,     0,     0,     0,     0,     0,   196,   390,     0,
       0,   676,     0,   161,     0,   676,     0,     0,     0,     0,
       0,     0,   952,     0,   385,   196,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   449,     0,
       0,   453,     0,   161,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1545,   485,   982,     0,   485,   985,     0,     0,   161,
       0,     0,     0,     0,     0,   161,     0,   385,     0,     0,
     676,     0,   676,   676,     0,     0,     0,     0,     0,   676,
     196,     0,     0,     0,     0,     0,     0,     0,     0,   385,
     385,     0,     0,   250,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,     0,
     485,     0,     0,     0,   485,     0,   161,     0,   485,  1054,
       0,     0,   485,  1058,     0,     0,     0,     0,     0,   453,
    1061,     0,     0,  1440,     0,     0,     0,   196,     0,     0,
       0,   458,     0,     0,   773,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1608,     0,     0,     0,     0,   595,     0,
       0,     0,   385,   485,     0,     0,     0,     0,     0,     0,
       0,     0,   196,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,   161,   161,     0,     0,     0,     0,
       0,     0,   398,     0,     0,     0,   161,     0,   669,     0,
     686,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   841,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1682,     0,     0,     0,
       0,     0,   485,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   857,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   669,     0,     0,     0,   676,     0,   754,
     773,   877,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   887,     0,   892,   887,     0,     0,     0,   161,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   896,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     676,   676,     0,     0,  1731,  1732,     0,   676,     0,     0,
       0,     0,     0,     0,     0,     0,  1736,     0,   196,   196,
       0,     0,   930,     0,   449,   434,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   946,     0,
       0,     0,     0,     0,   161,     0,     0,     0,     0,     0,
     385,     0,     0,     0,     0,   485,  1277,     0,   485,  1281,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   390,     0,     0,
       0,   976,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   595,     0,   595,   595,     0,   595,     0,   434,   595,
       0,   877,   595,   595,   595,     0,     0,     0,  1799,     0,
       0,     0,     0,     0,     0,     0,     0,   449,     0,     0,
       0,     0,     0,   161,     0,    13,    14,    15,    16,    17,
       0,     0,    19,   458,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,  -402,  -402,     0,  -402,
      42,    43,     0,  -402,     0,  1062,     0,     0,   385,     0,
       0,     0,     0,     0,  1850,     0,     0,     0,   196,     0,
      54,   449,     0,   938,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   669,     0,     0,   485,  1402,     0,
       0,     0,   357,     0,    59,    60,   385,     0,     0,     0,
       0,     0,   676,  1413,     0,   196,     0,   877,     0,     0,
       0,     0,     0,     0,     0,  1171,     0,     0,     0,    69,
       0,     0,   887,     0,     0,     0,     0,     0,     0,   485,
    1438,     0,   676,     0,     0,     0,     0,   449,     0,   458,
       0,     0,    72,   385,   385,     0,     0,     0,     0,     0,
      74,    75,     0,     0,     0,     0,     0,     0,     0,   449,
     449,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   449,     0,     0,     0,
    1207,  1209,     0,     0,     0,     0,     0,     0,   434,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1295,     0,     0,     0,     0,     0,  1296,     0,   458,
      13,    14,    15,    16,    17,    18,     0,    19,   887,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,   449,     0,     0,    42,    43,     0,   196,     0,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,     0,   390,     0,    54,  1297,     0,     0,     0,
       0,     0,     0,  1263,     0,   458,     0,     0,     0,     0,
       0,     0,     0,  1270,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   385,    57,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   754,     0,     0,     0,
       0,     0,     0,  1298,     0,     0,     0,    72,   853,  1335,
    1335,     0,     0,     0,     0,    74,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     485,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   485,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   434,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   887,     0,     0,     0,     0,     0,   385,     0,
     449,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1408,     0,     0,     0,     0,     0,
    1418,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     595,     0,     0,     0,     0,     0,     0,     0,     0,   385,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   485,   485,
     458,     0,  1462,     0,     0,     0,     0,  1465,     0,     0,
       0,     0,     0,     0,   485,     0,     0,   312,     0,     0,
       0,   313,     0,   314,   250,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,     0,     0,   196,   887,     0,     0,   390,     0,
       0,     0,   595,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   458,     0,     0,   773,   316,   317,     0,
     762,     0,   319,    59,    60,    61,    62,    63,    64,    65,
      66,    67,   320,   321,   322,     0,   323,   324,   325,     0,
     326,   327,     0,     0,     0,     0,   669,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,   485,     0,     0,
       0,     0,     0,     0,   458,   485,   773,     0,   328,    71,
       0,   763,   764,     0,     0,     0,   457,     0,   330,    74,
      75,   331,   332,   333,   334,     0,     0,     0,   976,     0,
       0,     0,     0,   449,   449,     0,     0,     0,  1579,  1580,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,     0,     0,   485,  1852,     0,     0,   485,
       0,     0,   595,   595,   595,     0,   595,   595,     0,     0,
       0,     0,     0,   453,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   485,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     357,     0,     0,     0,     0,  1635,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   250,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   485,   485,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1674,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   485,  1686,
       0,     0,  1688,     0,   390,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1086,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,   312,     0,
      42,    43,   313,     0,   314,    44,    45,    46,    47,    48,
      49,    50,    51,    52,     0,     0,     0,    53,     0,  1087,
      54,  1088,    -2,     0,  1089,     0,   196,  1090,  1091,  1092,
    1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,  1101,  -281,
    1102,  1103,  1104,  1105,  1106,     0,  1107,     0,   316,   317,
      57,   762,     0,   319,  1108,  1109,    61,    62,    63,    64,
      65,    66,    67,   320,   321,   322,  1110,   323,   324,   325,
       0,   326,   327,     0,     0,     0,     0,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,   390,     0,
       0,     0,     0,     0,     0,     0,     0,   595,    -3,   328,
      71,     0,    72,   329,     0,     0,     0,   274,     0,   330,
      74,    75,   331,   332,   333,   334,     0,     0,     0,     0,
       0,     0,     0,     0,  -173,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   449,
     449,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   887,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,   250,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,     0,     0,    42,    43,     0,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,    52,     0,     0,     0,
      53,     0,     0,    54,    55,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    56,
       0,     0,     0,    57,    58,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,   250,    68,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,   595,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,    71,     0,    72,    73,     0,     0,     0,
     449,     0,     0,    74,    75,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   595,     0,     0,   453,     4,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,  1086,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,   312,
       0,    42,    43,   313,     0,   314,    44,    45,    46,    47,
      48,    49,    50,    51,    52,     0,     0,     0,    53,     0,
    1087,    54,  1088,    -2,     0,  1089,     0,     0,  1090,  1091,
    1092,  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1100,  1101,
    -281,  1102,  1103,  1104,  1105,  1106,     0,  1107,     0,   316,
     317,    57,   762,     0,   319,  1108,  1109,    61,    62,    63,
      64,    65,    66,    67,   320,   321,   322,  1110,   323,   324,
     325,     0,   326,   327,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     328,    71,     0,    72,   329,     0,     0,     0,   274,     0,
     330,    74,    75,   331,   332,   333,   334,     0,     0,     0,
       0,     0,     0,     0,     0,  -173,     4,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,   312,     0,
      42,    43,   313,     0,   314,    44,    45,    46,    47,    48,
      49,    50,    51,    52,     0,     0,     0,    53,     0,     0,
      54,   315,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   316,   317,
      57,   318,     0,   319,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   320,   321,   322,     0,   323,   324,   325,
       0,   326,   327,     0,     0,     0,     0,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   328,
       0,     0,    72,   329,     0,     0,     0,     0,     0,   330,
      74,    75,   331,   332,   333,   334,     0,     0,     0,     0,
       0,     0,     0,  1659,  1660,     4,   238,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,     0,     0,   312,     0,    42,
      43,   313,     0,   314,    44,    45,    46,    47,    48,    49,
      50,    51,    52,     0,     0,     0,    53,     0,     0,    54,
     315,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   316,   317,    57,
     318,     0,   319,    59,    60,    61,    62,    63,    64,    65,
      66,    67,   320,   321,   322,     0,   323,   324,   325,     0,
     326,   327,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   328,     0,
       0,    72,   329,     0,     0,     0,     0,     0,   330,    74,
      75,   331,   332,   333,   334,   238,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,   312,     0,    42,    43,
     313,     0,   314,   366,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,   315,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   316,   317,     0,   318,
       0,   319,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   320,   321,   322,     0,   323,   324,   325,     0,   326,
     327,     0,     0,     0,     0,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   328,     0,     0,
      72,   431,     0,     0,     0,     0,     0,   330,   432,    75,
     331,   332,   333,   334,   238,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,     0,     0,   312,     0,    42,    43,   313,
       0,   314,   366,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    54,   315,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   316,   317,     0,   318,     0,
     319,    59,    60,    61,    62,    63,    64,    65,    66,    67,
     320,   321,   322,     0,   323,   324,   325,     0,   326,   327,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,     0,     0,    72,
    1204,     0,     0,     0,     0,     0,   330,  1205,    75,   331,
     332,   333,   334,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,   312,     0,    42,    43,   313,     0,
     314,   366,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,   315,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   316,   317,     0,   318,     0,   319,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   320,
     321,   322,     0,   323,   324,   325,     0,   326,   327,     0,
       0,     0,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   328,     0,     0,    72,   405,
       0,     0,     0,     0,     0,   330,    74,    75,   331,   332,
     333,   334,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,   312,     0,    42,    43,   313,     0,   314,
     366,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    54,   315,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   316,   317,     0,   318,     0,   319,    59,
      60,    61,    62,    63,    64,    65,    66,    67,   320,   321,
     322,     0,   323,   324,   325,     0,   326,   327,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   328,     0,     0,    72,   431,     0,
       0,     0,     0,     0,   330,    74,    75,   331,   332,   333,
     334,   237,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,  -402,  -402,     0,  -402,    42,    43,     0,  -402,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,    54,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,  -401,  -401,
       0,  -401,    42,    43,     0,  -401,    58,     0,     0,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    70,    71,     0,    72,   239,     0,
       0,     0,  -705,     0,     0,    74,    75,     4,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,     0,
       0,    42,    43,     0,     0,     0,    44,    45,    46,    47,
      48,    49,    50,    51,    52,     0,     0,     0,    53,     0,
       0,    54,     0,     0,     0,     0,  -334,  -334,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -334,
       0,     0,     0,    72,    73,     0,     0,     0,     0,     0,
       0,    74,    75,     4,   238,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,     0,     0,     0,     0,    42,    43,     0,
       0,     0,    44,    45,    46,    47,    48,    49,    50,    51,
      52,     0,     0,     0,    53,     0,     0,    54,     0,     0,
       0,     0,  -335,  -335,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -335,     0,     0,     0,    72,
      73,     0,     0,     0,     0,     0,     0,    74,    75,   237,
     238,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,  -402,
    -402,     0,  -402,    42,    43,     0,  -402,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    54,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,  -402,  -402,     0,  -402,
      42,    43,     0,  -402,    58,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    70,    71,     0,    72,   239,     0,     0,  1295,
       0,     0,     0,    74,    75,  1296,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,     0,     0,    42,    43,     0,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    54,  1297,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1477,     0,     0,     0,    72,   853,     0,     0,  1295,
       0,     0,     0,    74,    75,  1296,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,     0,     0,    42,    43,     0,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    54,  1297,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1478,     0,     0,     0,    72,   853,     0,     0,  1295,
       0,     0,     0,    74,    75,  1296,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,     0,     0,    42,    43,     0,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    54,  1297,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1479,     0,     0,     0,    72,   853,     0,     0,     0,
       0,     0,     0,    74,    75,   237,   238,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,  -402,  -402,     0,  -402,    42,
      43,     0,  -402,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,    19,    54,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,     0,     0,    59,    60,     0,     0,  1319,     0,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,   312,     0,     0,     0,   313,
       0,   314,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   239,     0,     0,     0,  1087,     0,   315,    74,
      75,  1089,  1738,  1739,  1090,  1091,  1092,  1093,  1094,  1095,
    1096,  1097,  1098,  1099,  1100,  1101,  -281,  1102,  1103,  1104,
    1105,  1106,     0,  1107,     0,   316,   317,     0,   762,     0,
     319,  1108,  1109,    61,    62,    63,    64,    65,    66,    67,
     320,   321,   322,  1110,   323,   324,   325,     0,   326,   327,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,  1319,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,    71,     0,    72,
     405,     0,     0,     0,   274,     0,   330,    74,    75,   331,
     332,   333,   334,   312,     0,     0,     0,   313,     0,   314,
       0,  -173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1087,     0,   315,    -2,     0,  1089,
       0,     0,  1090,  1091,  1092,  1093,  1094,  1095,  1096,  1097,
    1098,  1099,  1100,  1101,  -281,  1102,  1103,  1104,  1105,  1106,
       0,  1107,     0,   316,   317,     0,   762,     0,   319,  1108,
    1109,    61,    62,    63,    64,    65,    66,    67,   320,   321,
     322,  1110,   323,   324,   325,     0,   326,   327,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,  1319,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   328,    71,     0,    72,   405,     0,
       0,     0,   274,     0,   330,    74,    75,   331,   332,   333,
     334,   312,     0,     0,     0,   313,     0,   314,     0,  -173,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1087,     0,   315,     0,     0,  1089,     0,     0,
    1090,  1091,  1092,  1093,  1094,  1095,  1096,  1097,  1098,  1099,
    1100,  1101,  -281,  1102,  1103,  1104,  1105,  1106,     0,  1107,
       0,   316,   317,     0,   762,     0,   319,  1108,  1109,    61,
      62,    63,    64,    65,    66,    67,   320,   321,   322,  1110,
     323,   324,   325,     0,   326,   327,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   328,    71,     0,    72,   405,     0,     0,     0,
     274,     0,   330,    74,    75,   331,   332,   333,   334,     0,
       0,     0,     0,     0,     0,     0,     0,  -173,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,  -402,  -402,     0,
    -402,    42,    43,     0,  -402,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,    59,    60,    61,    62,    63,
      64,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      70,    71,     0,    72,   239,     0,     0,     0,  -709,     0,
       0,    74,    75,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,  -402,  -402,     0,  -402,    42,    43,     0,  -402,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,    71,     0,    72,   239,
       0,     0,     0,     0,     0,     0,    74,    75,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,     0,
       0,    42,    43,     0,     0,     0,   366,    45,    46,    47,
      48,    49,    50,    51,     0,     0,    13,    14,    15,    16,
      17,    54,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,  -402,  -402,     0,
    -402,    42,    43,     0,  -402,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,  1021,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -580,    72,   367,    59,    60,     0,     0,     0,
       0,    74,    75,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,    72,     0,     0,    42,    43,     0,     0,
       0,   366,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    60,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,     0,  1714,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,   367,
       0,     0,     0,     0,     0,     0,    74,    75,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,     0,
       0,    42,    43,     0,     0,     0,   366,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,  1716,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,   367,     0,     0,     0,     0,     0,
       0,    74,    75,   238,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,   366,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    60,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,   367,
       0,     0,     0,     0,     0,     0,    74,    75,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,     0,
       0,    42,    43,     0,     0,     0,   366,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,     0,     0,     0,     0,     0,
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
      40,    41,  -402,  -402,     0,  -402,    42,    43,     0,  -402,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    60,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,   239,
       0,     0,     0,     0,     0,     0,    74,    75,    13,    14,
      15,    16,    17,    18,   582,    19,   583,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,   312,     0,    42,    43,   313,     0,   314,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    54,   315,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   584,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   316,   317,     0,   318,     0,   319,    59,    60,    61,
      62,    63,    64,    65,    66,    67,   320,   321,   322,     0,
     323,   324,   325,     0,   326,   327,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   328,     0,     0,    72,   585,     0,     0,     0,
     274,     0,   330,    74,    75,   586,   587,   333,   334,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,   312,     0,    42,    43,   313,     0,   314,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,   315,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   316,   317,     0,   318,     0,   319,    59,    60,
      61,    62,    63,    64,    65,    66,    67,   320,   321,   322,
       0,   323,   324,   325,     0,   326,   327,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   328,     0,   411,    72,   329,     0,     0,
       0,     0,     0,   330,    74,    75,   331,   332,   333,   334,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,   312,     0,    42,    43,   313,     0,   314,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    54,   315,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   316,   317,     0,   318,     0,   319,    59,
      60,    61,    62,    63,    64,    65,    66,    67,   320,   321,
     322,     0,   323,   324,   325,     0,   326,   327,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   328,     0,     0,    72,   585,     0,
       0,     0,   274,     0,   330,    74,    75,   331,   332,   333,
     334,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,   312,     0,    42,    43,   313,     0,
     314,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,   315,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   316,   317,     0,   318,     0,   319,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   320,
     321,   322,     0,   323,   324,   325,     0,   326,   327,     0,
       0,     0,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   328,     0,     0,    72,   329,
       0,     0,     0,     0,     0,   330,    74,    75,   331,   332,
     333,   334,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,     0,     0,   312,     0,    42,    43,   313,
       0,   314,   366,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    54,   315,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   316,   317,     0,   318,     0,
     319,    59,    60,    61,    62,    63,    64,    65,    66,    67,
     320,   321,   322,     0,   323,   324,   325,     0,   326,   327,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,     0,     0,    72,
     431,     0,     0,     0,     0,     0,   330,    74,    75,   331,
     332,   333,   334,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,   312,     0,    42,    43,
     313,     0,   314,   366,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,   315,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   316,   317,     0,   318,
       0,   319,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   320,   321,   322,     0,   323,   324,   325,     0,   326,
     327,     0,     0,     0,     0,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   328,     0,     0,
      72,   405,     0,     0,     0,     0,     0,   330,    74,    75,
     331,   332,   333,   334,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,     0,     0,     0,     0,    42,
      43,     0,     0,     0,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    70,    71,
       0,    72,    73,     0,     0,     0,  -707,     0,     0,    74,
      75,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,    44,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
      59,    60,    61,    62,    63,    64,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    70,    71,     0,    72,    73,
       0,     0,     0,     0,     0,     0,    74,    75,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     0,
       0,     0,     0,    42,    43,     0,     0,     0,    44,    45,
      46,    47,    48,    49,    50,    51,     0,     0,     0,     0,
       0,     0,     0,    54,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,    59,    60,    61,
      62,    63,    64,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    69,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    71,     0,    72,    73,     0,     0,     0,
       0,     0,     0,    74,    75,   540,   238,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,     0,     0,     0,     0,    42,
      43,     0,     0,     0,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    54,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,    59,    60,    42,    43,     0,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    54,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   859,    72,   853,     0,
       0,     0,     0,     0,     0,    74,    75,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,     0,     0,
       0,     0,    42,    43,     0,     0,     0,    44,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,     0,  1371,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    59,    60,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,   853,     0,     0,     0,     0,
       0,     0,    74,    75,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,     0,     0,     0,     0,    42,
      43,     0,     0,     0,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    59,    60,     0,     0,    42,    43,     0,
       0,     0,    44,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    54,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   281,     0,     0,     0,     0,     0,     0,    74,
      75,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
      73,     0,     0,     0,     0,     0,     0,    74,    75,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,     0,     0,    42,    43,     0,     0,     0,    44,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    59,    60,
       0,     0,    42,    43,     0,     0,     0,   366,    45,    46,
      47,    48,    49,    50,    51,     0,     0,     0,     0,     0,
       0,     0,    54,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,   427,     0,     0,
       0,     0,     0,     0,    74,    75,    59,    60,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,   367,     0,     0,     0,     0,
       0,     0,    74,    75,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,     0,     0,     0,     0,    42,
      43,     0,     0,     0,   366,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    59,    60,     0,     0,    42,    43,     0,
       0,     0,   366,    45,    46,    47,    48,    49,    50,    51,
       0,     0,     0,     0,     0,     0,     0,    54,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   281,     0,     0,     0,     0,     0,     0,    74,
      75,    59,    60,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
     427,     0,     0,     0,     0,     0,     0,    74,    75,   237,
     238,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,  -402,
    -402,     0,  -402,    42,    43,     0,  -402,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    54,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,     0,     0,    59,    60,    42,
      43,     0,     0,     0,    44,    45,    46,    47,    48,    49,
      50,    51,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,    59,    60,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,   853,     0,     0,     0,     0,     0,     0,    74,
      75,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,     0,     0,     0,     0,    42,    43,     0,     0,
       0,   366,    45,    46,    47,    48,    49,    50,    51,     0,
       0,     0,     0,     0,     0,     0,    54,     0,     0,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      59,    60,     0,     0,    42,    43,     0,     0,     0,   366,
      45,    46,    47,    48,    49,    50,    51,     0,     0,     0,
       0,     0,     0,     0,    54,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,   296,
       0,     0,     0,     0,     0,     0,    74,    75,    59,    60,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,   853,     0,     0,
       0,     0,     0,     0,    74,    75,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,     0,
       0,    42,    43,     0,     0,     0,   366,    45,    46,    47,
      48,    49,    50,    51,     0,    13,    14,    15,    16,    17,
       0,    54,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,  -402,  -402,     0,  -402,
      42,    43,     0,  -402,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,    59,    60,     0,     0,     0,     0,
       0,    74,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,   296,     0,     0,     0,     0,     0,     0,
      74,    75,   238,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,     0,     0,     0,     0,    42,    43,     0,     0,     0,
     366,    45,    46,    47,    48,    49,    50,    51,     0,     0,
       0,     0,     0,     0,     0,    54,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   826,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -593,    72,   238,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,     0,     0,     0,
       0,    42,    43,     0,     0,     0,   366,    45,    46,    47,
      48,    49,    50,    51,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1643,     0,     0,     0,     0,   238,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,    72,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,     0,     0,
      42,    43,     0,     0,     0,   366,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   238,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    59,    60,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
    -402,  -402,     0,  -402,    42,    43,     0,  -402,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,    54,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,     0,     0,    59,    60,
      42,    43,     0,     0,     0,    44,    45,    46,    47,    48,
      49,    50,    51,     0,     0,     0,     0,     0,     0,     0,
      54,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,    58,     0,     0,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,     0,     0,     0,     0,    42,    43,
      71,     0,    72,    44,    45,    46,    47,    48,    49,    50,
      51,     0,     0,     0,     0,     0,     0,     0,    54,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
       0,     0,    59,    60,    42,    43,     0,     0,     0,   366,
      45,    46,    47,    48,    49,    50,    51,   312,     0,     0,
       0,   313,     0,   314,    54,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     315,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,    59,    60,
       0,     0,     0,     0,     0,     0,     0,   316,   317,     0,
     318,     0,   319,  1813,    60,    61,    62,    63,    64,    65,
      66,    67,   320,   321,   322,     0,   323,   324,   325,     0,
     326,   327,     0,     0,     0,     0,   312,     0,    69,     0,
     313,     0,   314,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   328,   315,
       0,    72,   405,     0,     0,     0,     0,     0,   330,    74,
      75,   331,   332,   333,   334,     0,     0,     0,     0,     0,
       0,     0,  1814,  -173,     0,     0,   316,   317,     0,   318,
       0,   319,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   320,   321,   322,     0,   323,   324,   325,   312,   326,
     327,     0,   313,     0,   314,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   315,     0,     0,     0,     0,     0,   328,  1174,     0,
      72,   405,     0,     0,     0,  1175,     0,   330,    74,    75,
     331,   332,   333,   334,     0,     0,     0,     0,   316,   317,
       0,   318,     0,   319,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   320,   321,   322,     0,   323,   324,   325,
     312,   326,   327,     0,   313,     0,   314,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   315,     0,     0,     0,     0,     0,   328,
       0,     0,    72,   405,     0,     0,     0,   457,     0,   330,
      74,    75,   331,   332,   333,   334,     0,     0,     0,     0,
     316,   317,     0,   318,     0,   319,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   320,   321,   322,     0,   323,
     324,   325,   312,   326,   327,     0,   313,     0,   314,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   315,     0,     0,     0,     0,
       0,   328,   772,     0,    72,   405,     0,     0,     0,     0,
       0,   330,    74,    75,   331,   332,   333,   334,     0,     0,
       0,     0,   316,   317,     0,   318,     0,   319,    59,    60,
      61,    62,    63,    64,    65,    66,    67,   320,   321,   322,
       0,   323,   324,   325,   312,   326,   327,     0,   313,     0,
     314,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   315,     0,     0,
       0,     0,     0,   328,     0,     0,    72,   405,     0,     0,
       0,   274,     0,   330,    74,    75,   331,   332,   333,   334,
       0,     0,     0,     0,   316,   317,     0,   318,     0,   319,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   320,
     321,   322,     0,   323,   324,   325,   312,   326,   327,     0,
     313,     0,   314,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   315,
       0,     0,     0,     0,     0,   328,   884,     0,    72,   405,
       0,     0,     0,     0,     0,   330,    74,    75,   331,   332,
     333,   334,     0,     0,     0,     0,   316,   317,     0,   318,
       0,   319,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   320,   321,   322,     0,   323,   324,   325,   312,   326,
     327,     0,   313,     0,   314,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   315,     0,     0,     0,     0,     0,   328,     0,     0,
      72,   405,     0,     0,   916,     0,     0,   330,    74,    75,
     331,   332,   333,   334,     0,     0,     0,     0,   316,   317,
       0,   318,     0,   319,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   320,   321,   322,     0,   323,   324,   325,
     312,   326,   327,     0,   313,     0,   314,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   315,     0,     0,     0,     0,     0,   328,
    1269,     0,    72,   405,     0,     0,     0,     0,     0,   330,
      74,    75,   331,   332,   333,   334,     0,     0,     0,     0,
     316,   317,     0,   318,     0,   319,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   320,   321,   322,     0,   323,
     324,   325,   312,   326,   327,     0,   313,     0,   314,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   315,     0,     0,     0,     0,
       0,   328,     0,     0,    72,   405,     0,     0,     0,  1329,
       0,   330,    74,    75,   331,   332,   333,   334,     0,     0,
       0,     0,   316,   317,     0,   318,     0,   319,    59,    60,
      61,    62,    63,    64,    65,    66,    67,   320,   321,   322,
       0,   323,   324,   325,   312,   326,   327,     0,   313,     0,
     314,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   315,     0,     0,
       0,     0,     0,   328,     0,  1744,    72,   405,     0,     0,
       0,     0,     0,   330,    74,    75,   331,   332,   333,   334,
       0,     0,     0,     0,   316,   317,     0,   318,     0,   319,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   320,
     321,   322,     0,   323,   324,   325,   312,   326,   327,     0,
     313,     0,   314,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   315,
       0,     0,     0,     0,     0,   328,  1945,     0,    72,   405,
       0,     0,     0,     0,     0,   330,    74,    75,   331,   332,
     333,   334,     0,     0,     0,     0,   316,   317,     0,   318,
       0,   319,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   320,   321,   322,     0,   323,   324,   325,   312,   326,
     327,     0,   313,     0,   314,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   315,     0,     0,     0,     0,     0,   328,     0,     0,
      72,   405,     0,     0,     0,     0,     0,   330,    74,    75,
     331,   332,   333,   334,     0,     0,     0,     0,   316,   317,
       0,   318,     0,   319,    59,    60,    61,    62,    63,    64,
      65,    66,    67,   320,   321,   322,     0,   323,   324,   325,
     312,   326,   327,     0,   313,     0,   314,     0,     0,    69,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   315,     0,     0,     0,     0,     0,   571,
       0,     0,    72,   405,     0,     0,     0,     0,     0,   330,
      74,    75,   331,   332,   333,   334,     0,     0,     0,     0,
     316,   317,     0,   318,     0,   319,    59,    60,    61,    62,
      63,    64,    65,    66,    67,   320,   321,   322,     0,   323,
     324,   325,   312,   326,   327,     0,   313,     0,   314,     0,
       0,    69,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   315,     0,     0,     0,     0,
       0,   576,     0,     0,    72,   405,     0,     0,     0,     0,
       0,   330,    74,    75,   331,   332,   333,   334,     0,     0,
       0,     0,   316,   317,     0,   318,     0,   319,    59,    60,
      61,    62,    63,    64,    65,    66,    67,   320,   321,   322,
       0,   323,   324,   325,   312,   326,   327,     0,   313,     0,
     314,     0,     0,    69,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   315,     0,     0,
       0,     0,     0,   579,     0,     0,    72,   405,     0,     0,
       0,     0,     0,   330,    74,    75,   331,   332,   333,   334,
       0,     0,     0,     0,   316,   317,     0,   318,     0,   319,
      59,    60,    61,    62,    63,    64,    65,    66,    67,   320,
     321,   322,     0,   323,   324,   325,   312,   326,   327,     0,
     313,     0,   314,     0,     0,    69,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   315,
       0,     0,     0,     0,     0,   328,     0,     0,    72,   405,
       0,     0,     0,     0,     0,   330,   840,    75,   331,   332,
     333,   334,     0,     0,     0,     0,   316,   317,     0,   318,
       0,   319,    59,    60,    61,    62,    63,    64,    65,    66,
      67,   320,   321,   322,     0,   323,   324,   325,     0,   326,
     327,     0,     0,     0,     0,     0,     0,    69,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   328,     0,     0,
      72,   405,     0,     0,     0,     0,     0,   330,   432,    75,
     331,   332,   333,   334
};

static const yytype_int16 yycheck[] =
{
       1,   175,   214,     4,     1,   158,     1,   370,   214,    70,
     202,   158,   251,     4,   341,   169,    70,    70,   214,     1,
     146,   703,   328,    70,   158,   610,   215,   368,   236,   596,
     214,   278,    91,   214,     1,   174,   684,   589,   214,   671,
     768,   671,   385,    70,   385,   214,   389,    72,   389,   217,
     650,    52,    53,   835,    55,   159,  1096,    78,    55,   895,
      55,     1,    70,   188,  1253,  1254,   743,   889,   135,    70,
    1618,   519,   749,    55,   675,   742,   439,    78,   290,   742,
     671,   498,   499,    78,   290,    86,  1618,   819,    55,  1619,
      91,  1680,   433,    94,   290,  1618,    91,    98,     1,    94,
    1181,  1738,   291,    98,   926,   231,   290,    98,   740,   290,
     740,   559,   310,   845,   290,    55,    70,   285,   286,  1223,
     187,   290,   143,   177,   177,    67,   759,   760,     1,   148,
     177,   677,   740,   740,   153,   136,   315,   224,   139,   835,
     141,   742,   112,   776,   141,   146,   141,   740,  1742,   740,
     177,   152,    55,   214,    84,    67,   215,  1075,   159,   141,
     214,   214,   249,   342,   343,   740,   128,   214,    67,   177,
      55,    92,   259,   117,   141,   176,   177,    56,    57,    79,
     150,   176,    55,   760,   557,    72,    73,   214,     0,   127,
     191,   223,    72,    73,   226,    98,   191,  1727,   744,   776,
     162,   141,   748,   401,   205,   149,   214,    84,   150,   842,
       1,   757,   758,   214,   215,   368,   248,   146,  1855,   240,
     215,    94,   152,   177,   154,   154,   258,  1145,   407,   290,
     231,   169,   291,    67,   146,    84,   290,   290,   141,   240,
     968,   658,   481,   290,   269,   240,   146,   146,  1080,   250,
     171,   150,   253,    84,   433,   467,   141,   127,   253,   260,
     214,   467,   148,   152,    55,   842,   152,   154,   141,   270,
     271,   467,   273,   128,   154,   154,   297,   154,   328,    93,
     433,   278,   171,   467,    78,   967,   467,   476,   705,   290,
     291,   467,   171,   878,     0,  1825,   291,   298,   467,   546,
     663,  1895,   608,   304,   305,   154,  1088,   162,   309,  1390,
    1391,  1392,   146,  1857,   991,   867,   150,   660,  1422,   660,
     146,   342,   922,   154,   687,   146,  1874,  1871,  1917,  1923,
     997,   694,   146,   437,   997,  1117,   206,     1,   278,  1175,
    1072,   553,  1874,   684,   170,  1875,   947,   553,     0,   143,
     141,  1874,     1,  1897,   697,     4,   697,   553,   990,   112,
     990,    53,   429,   157,    56,    57,   555,    59,   816,   553,
     496,  1289,   553,   143,   701,   278,   502,   553,  1908,   504,
     253,   175,   990,   990,   553,   148,   759,   760,    67,   974,
     153,    55,  1088,   394,  1512,    67,   997,   990,   399,   990,
     170,   402,   403,   776,   557,   804,    55,  1040,    70,   279,
    1599,  1600,   413,   467,   467,   990,   143,   476,   413,  1537,
     467,  1117,   149,   557,    86,    67,    67,    91,   143,   146,
      94,   518,   152,  1080,    98,   436,   437,    67,  1260,    67,
     467,   103,   162,   170,   146,   173,   240,   448,   449,    98,
     629,  1080,   154,   128,   146,   170,   457,   489,   328,   467,
    1292,  1293,  1294,  1040,   827,  1193,   467,   146,  1014,   842,
     152,   150,   597,   260,   146,   476,    67,   141,   150,   511,
     162,   476,   157,   158,   271,   517,   146,   278,     1,   521,
     863,     4,   141,  1329,    19,   496,   555,   370,  1226,   553,
     553,   502,    70,   297,   146,   146,   553,    67,   150,   150,
     146,   174,   148,   467,   150,    67,   146,    85,   146,   918,
     150,   571,   150,   573,   574,    67,   576,   191,   152,   579,
      67,   684,   582,   583,   584,  1212,   661,  1219,    67,   540,
     939,   542,    55,    67,  1380,   152,    67,   171,   342,   546,
     146,   126,   553,   152,   555,   146,   146,   152,  1206,   150,
     555,   146,   154,  1385,   171,  1242,   439,   568,   101,   102,
     151,   146,   171,   936,   368,   150,   171,   769,   152,   171,
     146,    94,   157,   158,   150,    98,   146,   986,   987,   253,
     150,   934,   171,   934,   146,   170,   152,   171,   150,   600,
     601,  1790,   399,   604,   146,   402,   546,   608,   150,   146,
     611,   152,   737,   150,   753,   171,   149,   146,   488,   154,
    1660,   150,   146,   493,  1664,   146,   150,   171,   141,   150,
       1,   656,   149,     4,   759,   760,   171,   126,   742,   433,
     510,   544,   146,   546,   148,  1292,  1293,  1294,   122,   123,
     520,   776,   778,  1381,   152,   656,   148,   146,   659,     3,
     152,   150,   152,  1292,  1293,  1294,     3,  1040,   157,   158,
     543,   797,     3,   171,   848,    12,    13,    14,    15,    16,
     859,   171,   145,   148,    55,   864,  1477,  1478,  1479,   152,
     691,  1419,   166,   167,   159,   160,   875,    67,   148,    70,
     152,   571,   703,   153,   498,   499,   576,    78,   148,   579,
     863,   157,   146,   153,   148,  1755,   150,   842,   164,   165,
      91,   661,   146,    94,   146,   154,  1311,    98,   598,   863,
      67,   153,   126,   154,   152,   147,   154,  1154,   763,   740,
     253,   742,   154,  1565,   145,  1567,   146,    43,    44,   413,
      46,   152,   146,   754,    50,   546,   150,   146,   168,   148,
     761,   150,   763,   157,   158,  1332,   170,   149,   150,   126,
     141,   153,  1812,   965,   146,   146,   777,   778,   779,   146,
    1820,   148,   148,   150,   150,  1158,   818,  1026,   159,   146,
     148,  1519,   146,   150,   152,   148,   797,   737,   148,   152,
     157,   158,   152,   148,    99,   176,   177,   152,   103,   101,
     102,   106,   476,   108,   687,  1382,  1512,   146,  1514,   112,
     191,    12,    13,    14,    15,    16,   148,  1867,   101,   102,
     152,  1559,   833,   834,   835,  1234,  1235,   148,   835,  1031,
     148,  1537,    53,   214,   215,    56,    57,  1210,    59,  1248,
    1249,   148,   146,   835,   958,   152,   847,   148,   146,   148,
     231,   152,   150,   152,   658,  1206,   146,   146,   835,   240,
     150,   150,     1,   148,   148,     4,    67,   152,   152,   148,
    1253,   882,   253,  1282,  1283,   140,   141,   142,   889,   148,
     684,  1059,   996,   997,   895,   835,  1223,   152,   148,   270,
     148,   146,   273,   152,  1586,   150,   148,   162,   146,  1088,
     152,   705,   140,   141,   142,  1040,   171,   148,   148,   290,
     291,   152,   152,    21,   152,   926,    55,    12,    13,    14,
      15,    16,   835,   803,   162,     4,     5,     6,     7,     8,
       9,    10,    11,   171,   814,   146,   148,   817,   243,    78,
     152,   821,   148,   152,   827,   148,   152,   958,   152,   152,
     148,   962,   835,   148,   152,   838,   967,   152,     3,   148,
     148,   148,   973,   152,   152,   152,  1704,    12,    13,    14,
      15,    16,    67,   630,   631,   632,  1165,  1166,  1167,   990,
    1296,   719,   152,  1172,  1173,   996,   997,   151,   152,   148,
     148,  1155,  1156,   152,   152,  1158,   135,  1215,   152,   148,
      93,   148,   141,   152,   143,   152,    12,    13,    14,    15,
      16,  1703,  1148,   146,  1158,  1879,   146,  1206,   154,  1883,
    1031,  1404,    67,  1034,    12,    13,    14,    15,    16,    17,
      84,   126,   413,   154,   835,  1284,   159,   160,    12,    13,
      14,    15,    16,  1206,   848,   120,   121,  1785,   187,  1080,
     154,   146,  1758,   936,   145,   150,   437,  1466,  1467,   146,
     798,    67,   157,   158,  1266,   124,   125,   372,   373,   148,
     375,   155,   377,  1077,  1078,  1079,   457,   151,   152,  1090,
     753,  1088,  1093,  1094,  1095,  1422,   467,   105,   106,   107,
     108,   109,   171,    67,   146,   476,  1088,  1254,   237,   146,
    1806,   240,  1162,  1302,   157,   158,  1117,   151,   152,   155,
    1117,  1088,  1123,   151,   152,   496,  1822,   112,  1253,  1254,
    1131,   502,   128,  1134,  1135,  1117,   163,  1138,   158,  1134,
    1135,   151,   152,   156,  1135,   151,   152,  1148,  1088,   278,
    1117,   152,   153,    12,    13,    14,    15,    16,  1159,   151,
     152,   168,  1858,   126,   128,   151,   152,   149,   297,   540,
     151,   542,   170,  1572,  1175,   151,   152,  1117,   148,  1578,
    1181,   154,   553,   154,   555,  1088,   835,   154,  1773,    12,
      13,    14,    15,    16,    17,  1594,  1595,   148,   847,   637,
     638,   639,   640,   152,  1074,   151,   152,   148,    67,   151,
     152,   151,   152,   342,  1117,  1088,   148,  1087,  1219,   148,
     515,   151,   152,   151,   152,   148,  1599,   151,   152,   600,
     601,   148,  1135,   604,  1104,   151,   152,   608,   151,   152,
     611,   148,  1243,  1302,  1117,   151,   152,     9,   151,   152,
     150,  1404,   151,   916,   152,   153,   151,   152,   128,  1260,
     128,  1134,   151,   152,   146,  1512,   146,   126,   152,   153,
    1404,  1292,  1293,  1294,   148,  1296,  1297,   150,  1490,    72,
      73,   152,   153,   148,  1490,   656,   148,   146,   659,    65,
    1537,   150,  1182,  1183,  1490,  1484,   148,  1088,   157,   158,
     429,  1302,   148,  1492,  1295,  1306,  1490,   148,  1309,  1490,
     633,   634,  1486,   151,  1490,   635,   636,   154,  1457,  1458,
     691,  1490,   835,   641,   642,   154,  1117,   154,  1329,   154,
    1497,  1498,  1155,  1156,   847,   151,    98,  1210,   146,   101,
     102,   103,   104,   105,   106,   107,   108,   109,  1349,   148,
    1351,   148,   154,   154,    73,   151,  1351,    17,   170,   154,
    1154,   152,   146,  1542,   171,   171,  1029,    17,   151,   740,
     151,   742,   145,   153,   153,   153,   148,  1502,   152,  1380,
     148,   148,   148,   148,  1385,   147,    65,   148,   148,  1390,
    1391,  1392,   763,   148,   148,  1548,   154,   154,   154,   148,
     171,  1548,   152,   148,   148,   152,   777,   778,   779,   148,
     148,   540,  1206,   148,  1548,   148,   148,   546,   170,   148,
     148,  1633,   148,   148,  1765,  1484,   797,  1633,   148,  1490,
     148,   148,   145,  1492,   148,   145,  1427,  1633,   154,  1088,
     148,   148,  1312,  1313,   148,   148,   151,  1636,   148,  1633,
     148,   148,  1633,  1600,   148,   170,   145,  1633,   148,   152,
     146,   146,   833,  1826,  1633,   146,   146,    13,  1117,   146,
    1134,  1135,   146,    69,  1599,  1600,   171,   153,  1351,   152,
    1350,    86,  1823,  1484,   153,   151,  1135,   171,   151,  1490,
     154,  1492,   145,  1892,   145,   152,   171,   151,  1499,   171,
     171,   148,   148,   225,   152,   148,   148,  1557,   152,   148,
    1511,   882,  1513,   145,   145,  1512,   146,  1514,   889,   171,
      98,   146,   171,    75,   895,   103,   104,   105,   106,   107,
     108,   109,  1195,   148,   148,    12,    13,    14,    15,    16,
    1537,   145,   171,   171,  1545,   171,   146,  1736,   171,   115,
     171,   117,   118,   119,     1,   926,   146,     4,   171,  1685,
    1551,   171,   148,   145,  1565,   145,  1567,   152,   146,   147,
     153,   152,  1512,   148,  1514,  1088,   153,  1636,   115,   148,
     146,   145,   154,   149,   150,  1586,   151,   958,   154,   155,
      67,   151,   151,  1772,   148,   151,   145,  1537,   148,  1738,
     148,   148,   145,   171,  1117,  1268,   151,    98,    55,  1512,
     152,  1514,   103,   104,   105,   106,   107,   108,   109,   990,
     148,  1134,  1135,   153,   146,   996,   997,   146,   146,   104,
     145,    78,  1633,   145,  1537,  1636,   152,   359,   151,  1512,
     151,  1514,   151,  1790,   154,  1646,  1295,   148,   148,   126,
     148,    98,   148,   148,   151,   148,   147,   151,  1528,   150,
    1031,   148,  1874,  1034,  1537,  1790,  1657,  1668,  1874,   146,
     146,   145,   171,   150,    85,   148,   148,  1736,  1874,   145,
     157,   158,   151,  1684,  1685,   145,  1875,  1351,   135,   151,
    1874,   148,  1486,  1874,   141,   151,   143,   148,  1874,  1825,
     422,   150,  1703,   425,   148,  1874,   835,   148,   148,   148,
     157,   153,   171,   171,   148,  1854,  1855,   171,   847,  1908,
     153,  1512,     1,  1514,   148,     4,   171,   145,   175,    12,
      13,    14,    15,    16,    17,  1736,   171,   152,   148,   148,
     187,  1742,  1881,   171,  1765,  1746,  1537,   149,   171,   153,
     472,    98,   146,   152,  1657,    70,  1757,   146,   145,   147,
    1131,  1758,   147,  1134,  1135,   171,   171,   151,   104,  1770,
      12,    13,    14,    15,    16,    17,    55,  1148,  1427,   104,
     162,   162,  1295,  1656,   148,   153,   148,   145,  1159,   145,
     237,    70,   146,   240,  1457,  1458,   148,   171,   245,    78,
      70,   171,  1823,   148,  1175,   148,    17,   330,  1536,  1806,
    1181,   171,    91,  1874,  1953,    94,  1875,  1557,  1758,    98,
    1874,  1874,   643,  1188,  1825,  1822,  1827,  1874,   645,   588,
     644,   278,   647,   535,  1106,   646,  1117,  1838,  1351,  1840,
    1841,    52,    53,    54,    55,    56,    57,    58,    59,  1908,
     297,  1923,  1537,  1855,  1667,  1758,   135,  1871,  1815,  1774,
    1758,  1858,   141,  1512,   143,  1514,  1806,   146,   147,  1918,
    1905,  1917,  1243,  1874,  1875,  1748,  1529,  1823,   157,   158,
    1875,   328,  1822,  1529,  1885,  1758,  1884,  1935,  1537,  1260,
    1822,  1138,    45,  1736,  1895,   342,   175,   176,   177,   245,
    1484,  1297,  1551,  1806,  1799,  1498,  1131,  1908,   187,   188,
    1031,  1427,   191,  1908,  1427,   770,   851,  1551,  1858,  1822,
    1921,     0,  1923,   568,    -1,   973,   649,   649,   650,    -1,
      -1,  1302,    -1,  1806,    -1,   214,   215,  1938,   649,    -1,
     649,    -1,  1943,    -1,    -1,    -1,    -1,  1675,    -1,  1822,
      -1,  1080,   231,    -1,    -1,  1858,  1957,    -1,  1329,  1088,
    1961,   240,    -1,    -1,    -1,    -1,    -1,  1758,    -1,    -1,
    1971,    -1,  1090,    -1,   253,    -1,    -1,    -1,    -1,    -1,
    1351,    -1,   429,    -1,    -1,  1858,    -1,    -1,  1117,    -1,
      -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,  1512,
     722,  1514,    -1,   725,   283,    -1,    -1,    -1,  1657,  1380,
     289,   290,   291,    -1,  1385,  1806,    -1,    -1,   297,  1390,
    1391,  1392,    -1,    -1,  1537,    -1,    -1,    -1,    -1,    -1,
      -1,  1822,    -1,    -1,    56,    57,    58,    59,  1551,    -1,
      -1,  1769,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,
      -1,   498,   499,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1930,    -1,   784,   342,    -1,    -1,   788,  1858,    -1,    -1,
     792,    -1,    -1,    -1,    -1,  1738,    98,    -1,    -1,  1949,
     359,   103,   104,   105,   106,   107,   108,   109,   110,   368,
     369,   370,    -1,   540,    -1,    -1,    -1,   544,    -1,   546,
      -1,    -1,   804,    -1,    -1,    -1,   385,    -1,    -1,  1758,
     389,    -1,    -1,  1484,    -1,    -1,    -1,    -1,    -1,  1490,
      -1,  1492,    -1,    -1,   571,   147,   573,   574,   150,   576,
      -1,    -1,   579,    -1,   413,   582,   583,   584,    -1,    -1,
      -1,    -1,    -1,  1656,  1657,    -1,    -1,    -1,    -1,    -1,
     429,  1879,    -1,   188,   433,  1883,  1884,  1806,   437,    -1,
     439,    -1,    -1,  1292,  1293,  1294,  1295,  1296,  1297,    -1,
      -1,    -1,    -1,  1822,     4,     5,     6,     7,     8,     9,
      10,    11,  1910,    -1,    -1,    -1,    -1,    -1,   467,    -1,
      -1,  1854,  1855,    -1,  1565,    -1,  1567,   476,    -1,    -1,
     922,    -1,    -1,  1931,    -1,    -1,    -1,  1935,    -1,  1858,
      -1,   658,    70,    -1,    -1,    -1,   918,   496,  1881,   498,
     499,    -1,    -1,   502,    -1,   504,    -1,    -1,    -1,    -1,
      60,  1349,  1960,    91,    -1,  1748,    -1,   939,    -1,    -1,
      -1,    -1,    98,  1906,    -1,  1758,    -1,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,   705,    -1,
      -1,    98,  1633,   542,    -1,  1636,   103,   104,   105,   106,
     107,   108,   109,   110,   553,    -1,   555,   114,   557,   116,
      -1,    -1,    -1,    -1,   986,   987,    -1,    -1,   146,    -1,
    1953,   147,   571,  1806,   573,   574,    -1,   576,  1427,    -1,
     579,    -1,    -1,   582,   583,   584,    -1,    -1,    -1,  1822,
     147,    -1,    -1,   150,  1685,    -1,    -1,    -1,   597,    -1,
      -1,    -1,    -1,  1045,    -1,    -1,  1048,    -1,    -1,    -1,
      -1,    -1,    -1,   368,    -1,    -1,   371,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,  1858,    -1,    -1,    -1,    -1,
     385,    -1,    -1,    -1,   389,    -1,   214,   215,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1736,    -1,    -1,    -1,    -1,
     649,   650,    -1,   231,    -1,    -1,    -1,    -1,    -1,   658,
      -1,   660,   661,  1512,   663,  1514,    -1,    -1,   835,    -1,
      -1,    -1,   671,  1511,    -1,  1513,   675,    -1,   433,    -1,
     847,   848,    -1,    -1,    -1,   684,    -1,    -1,  1537,    -1,
      -1,    -1,    -1,     9,    -1,   694,    -1,    -1,   697,    -1,
      -1,    -1,  1551,    -1,    -1,    -1,   705,  1545,   328,    -1,
      -1,    -1,   290,   291,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,     4,     5,     6,     7,
       8,     9,    10,    11,  1825,    -1,  1827,    -1,   737,    -1,
      -1,   740,   126,   742,    -1,    -1,    -1,    -1,    -1,   504,
      -1,    -1,    -1,    -1,    -1,  1197,    -1,    -1,    -1,    -1,
     759,   760,   146,   147,    -1,    -1,    -1,    -1,    -1,   153,
      -1,    -1,    -1,   157,   158,    -1,    -1,   776,    -1,   778,
      -1,    -1,    98,  1874,  1875,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,  1239,   797,    94,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,  1646,    -1,
     105,    -1,  1234,  1235,    -1,    -1,    -1,  1908,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1248,  1249,    -1,    -1,
    1668,   147,    -1,    -1,    -1,    67,   835,    -1,    -1,    -1,
      -1,    -1,   597,   842,    -1,    -1,    -1,    -1,   847,   848,
      -1,    -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,   437,
    1282,  1283,    -1,    -1,   863,    -1,    98,    -1,    -1,    -1,
    1961,   103,   104,   105,   106,   107,   108,   109,   498,   499,
    1971,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   467,
      -1,    -1,    -1,    -1,   126,    -1,   191,    -1,   476,    -1,
      -1,    -1,    -1,    -1,  1742,   660,   661,    -1,  1746,  1758,
      -1,    -1,    -1,  1080,   146,   147,    -1,    -1,   496,  1757,
      -1,  1088,    -1,   922,   502,   157,   158,    -1,    -1,   684,
      -1,    -1,  1770,    -1,    -1,   934,    -1,   936,    -1,    -1,
      -1,    -1,   697,    -1,    -1,    -1,    -1,    -1,   947,    -1,
    1117,   571,    -1,    -1,    -1,    -1,   576,  1806,   253,   579,
      -1,    -1,    -1,    -1,   542,    -1,    -1,    -1,  1135,    60,
      61,    62,    63,  1822,    -1,   553,    -1,   555,   598,    -1,
      -1,    -1,   737,    -1,    -1,    -1,    -1,  1154,   283,    -1,
      -1,   990,    -1,    -1,   289,  1162,    -1,    -1,   997,    -1,
    1838,    12,  1840,  1841,    -1,    -1,    -1,    98,    -1,  1858,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   648,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,  1480,    -1,
      -1,  1040,    -1,    -1,  1466,  1467,    -1,  1885,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   147,  1895,    98,   150,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    83,    -1,    -1,   370,    -1,   168,    -1,    -1,
    1502,  1080,    -1,  1921,    -1,  1923,    -1,    98,    -1,  1088,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
    1938,    -1,    -1,    -1,    98,  1943,    -1,   147,   863,   103,
     104,   105,   106,   107,   108,   109,   110,    -1,  1117,  1957,
     114,    -1,   116,    -1,    -1,  1292,  1293,  1294,  1295,  1296,
    1297,   171,    -1,    -1,    -1,  1134,  1135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   439,    -1,    -1,    -1,    -1,  1148,
    1572,    -1,    -1,   147,    -1,  1154,  1578,    -1,    -1,  1158,
      -1,    -1,   740,  1162,   742,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1594,  1595,    -1,     1,  1618,  1619,     4,   934,
      -1,    -1,   937,    -1,    -1,    -1,    -1,    -1,    98,    52,
      53,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     778,    -1,    -1,    -1,    98,    -1,    -1,  1206,   503,   103,
     104,   105,   106,   107,   108,   109,   110,    -1,    -1,   797,
     114,    -1,   116,    86,    -1,    -1,    -1,    -1,   523,    55,
      -1,    -1,    12,    13,    14,    15,    16,   147,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   543,    -1,
      -1,    -1,    78,   147,  1253,  1254,   150,    -1,    -1,    -1,
    1427,   171,    -1,    -1,    -1,    -1,    -1,    -1,    94,    -1,
      -1,    -1,    98,   136,    -1,    -1,   139,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1727,    -1,    67,    -1,   152,
      -1,    -1,    -1,  1292,  1293,  1294,  1295,  1296,  1297,    -1,
      -1,    -1,    -1,  1302,  1303,    -1,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    -1,   141,    -1,   143,    98,  1486,
      -1,   147,    -1,   103,   104,   105,   106,   107,   108,   109,
      -1,   157,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   205,    -1,    -1,  1512,   126,  1514,    -1,   175,
      -1,    -1,  1351,  1795,    -1,    -1,    -1,  1799,    -1,    -1,
      -1,   187,   188,    -1,    -1,   191,   146,   147,   663,    -1,
    1537,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,    -1,    -1,  1825,  1551,    -1,    -1,   250,    -1,    -1,
    1557,    -1,   687,    -1,    -1,    -1,    -1,   260,    -1,   694,
      -1,    -1,    -1,  1158,    -1,  1404,    -1,    -1,   271,    -1,
      -1,   237,   990,    -1,   240,    -1,    -1,    -1,   996,   997,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   253,  1427,    -1,
      -1,    -1,  1874,  1875,    -1,   298,    -1,    -1,    67,    -1,
      -1,   304,   305,    -1,   270,    -1,   309,    -1,    -1,    -1,
      -1,  1206,   278,    -1,    -1,    -1,    -1,   283,    -1,    -1,
      -1,    -1,    -1,   289,    -1,    -1,  1908,    -1,    -1,    98,
    1892,   297,   101,   102,   103,   104,   105,   106,   107,   108,
     109,  1480,    -1,    -1,    -1,  1484,    -1,  1486,    -1,    -1,
    1657,  1490,    -1,  1492,    -1,    -1,    -1,   126,    -1,    -1,
      -1,    98,   328,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,  1512,    -1,  1514,   342,   146,   147,    -1,
      -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,
      -1,   394,   827,    -1,    -1,    -1,   399,    -1,  1537,   402,
      -1,    -1,   368,   838,   370,   371,    -1,    -1,    -1,  1548,
     147,    -1,  1551,    -1,    -1,    -1,    -1,    -1,  1557,   385,
      -1,    -1,    -1,   389,    -1,    -1,    -1,    -1,    -1,    -1,
    1148,  1191,    -1,   436,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   448,   449,    67,    -1,    -1,
      -1,  1758,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1599,  1600,    -1,   429,    -1,    -1,    -1,   433,    -1,    -1,
      -1,    -1,    -1,   439,    -1,    -1,    -1,    -1,    98,  1618,
    1619,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    -1,  1633,    -1,    -1,  1636,    -1,  1806,
      -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1404,
      -1,    -1,    -1,    -1,    -1,  1822,    -1,  1656,  1657,    -1,
      -1,    -1,    -1,    -1,    -1,  1243,   146,   147,    -1,    -1,
      -1,    -1,   498,   499,    98,    -1,    -1,   503,   504,   103,
     104,   105,   106,   107,   108,   109,  1685,    -1,    -1,    -1,
      -1,  1858,    -1,    -1,    -1,    -1,    -1,    -1,  1318,    -1,
      -1,    -1,   126,    -1,    -1,   568,    -1,  1327,    -1,   535,
      -1,  1331,    -1,  1333,   540,    -1,    -1,   543,   544,    -1,
     546,    -1,   146,   147,  1302,    -1,   150,    -1,  1727,    -1,
      -1,   557,    -1,   157,   158,    -1,    -1,  1736,    67,    -1,
      -1,    -1,    -1,    -1,    -1,   571,    -1,   573,   574,  1748,
     576,    -1,    -1,   579,    -1,    -1,   582,   583,   584,  1758,
      -1,    -1,    -1,    -1,    -1,    -1,  1765,    -1,    -1,    98,
      -1,   597,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1790,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,
    1799,    -1,    -1,    -1,    -1,    98,    -1,  1806,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   146,   147,    -1,
      -1,    -1,    -1,  1822,  1823,    -1,  1825,  1826,   157,   158,
      -1,    -1,   658,   659,   660,   661,    -1,   663,    98,  1134,
     703,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,  1472,    -1,   147,    -1,    -1,    -1,   684,  1858,
      -1,   687,    -1,    -1,   157,   691,    -1,    -1,   694,    -1,
      -1,   697,    -1,   699,    -1,  1874,  1875,    -1,    -1,   705,
      -1,    -1,    -1,    -1,    -1,    67,     1,   147,    -1,     4,
     150,   754,  1512,    -1,  1514,    -1,    -1,    -1,   761,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1484,    -1,    -1,  1908,
      -1,   737,  1490,    -1,  1492,  1210,    98,  1537,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    -1,   759,   760,    -1,    -1,    -1,    -1,    -1,
      55,    -1,    -1,    -1,   126,    -1,    -1,    -1,    98,    -1,
     776,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,    -1,    78,   146,   147,    -1,    -1,    -1,    -1,
      -1,   834,    -1,    -1,    -1,   157,   158,    -1,   804,    -1,
      -1,    -1,    -1,    98,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,   147,    -1,   149,
      -1,   827,    -1,    -1,   154,    -1,    -1,    -1,  1303,   835,
    1765,    -1,   838,    -1,    -1,    -1,   842,    -1,    -1,    -1,
     135,   847,   848,    -1,    -1,    -1,   141,    -1,  1648,    -1,
      -1,    94,  1652,   147,    -1,    -1,    -1,   863,    -1,  1659,
     154,    -1,   105,   158,   107,    -1,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1633,  1351,    -1,  1636,    -1,
      -1,   176,    -1,    -1,    -1,    -1,    -1,    -1,  1823,    -1,
      -1,    98,   187,   188,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    -1,   147,    -1,   149,   150,    -1,    -1,
      -1,    -1,   918,    -1,    -1,    -1,    -1,    -1,    -1,   962,
     215,    -1,    -1,    -1,   967,    -1,    -1,  1685,   934,    -1,
     936,   937,    -1,   939,    -1,    -1,   231,    -1,    -1,    -1,
     147,   236,   237,    -1,    -1,   240,    -1,   154,   191,    -1,
      -1,    -1,    -1,    -1,    -1,  1755,  1756,    -1,  1758,    -1,
      -1,    -1,    -1,    67,    -1,    -1,    -1,   262,    -1,    -1,
     265,    -1,   267,    -1,    -1,    -1,    -1,    -1,  1736,    -1,
     986,   987,    -1,   278,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,   291,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
     253,    -1,   255,   256,  1814,    -1,    -1,    -1,    -1,    -1,
      98,    -1,   126,    -1,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,    -1,  1040,    -1,   114,    -1,   116,    -1,
     283,    -1,   146,   147,    -1,    -1,   289,    -1,    -1,    -1,
    1093,  1094,  1095,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1866,  1825,  1868,   147,
      -1,    -1,   150,   368,  1080,    -1,   371,    -1,    -1,    -1,
    1123,    -1,  1088,    -1,    -1,    -1,    -1,     1,  1888,    -1,
     385,    -1,    -1,    98,   389,  1138,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,  1117,    -1,    -1,    -1,    -1,  1874,  1875,    -1,    -1,
      -1,    -1,    -1,    -1,  1924,  1925,  1926,   370,  1134,  1135,
      -1,    -1,    -1,   376,   429,   378,    -1,    -1,   433,    -1,
      -1,    55,   147,    -1,    -1,   150,    -1,    -1,  1154,    -1,
    1908,    -1,  1158,    -1,    98,    -1,  1162,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
     413,    -1,    -1,    -1,    -1,    -1,  1219,    -1,    -1,    -1,
      -1,  1656,   126,    98,    98,    -1,    -1,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   439,    -1,   441,   442,
    1206,    -1,   146,   147,  1210,    -1,   150,    -1,    -1,   504,
      -1,    -1,    -1,   157,   158,    -1,  1222,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   170,   141,  1234,  1235,
      -1,    -1,   147,   476,    -1,   150,    -1,    -1,    -1,    -1,
     535,    -1,  1248,  1249,   158,   540,    -1,  1253,  1254,   544,
      -1,   546,    -1,   496,    -1,    -1,    -1,    -1,   501,    -1,
     503,    -1,   557,  1306,    -1,    -1,  1309,    -1,    -1,    -1,
      -1,    -1,    -1,  1748,   188,    -1,  1282,  1283,    -1,    -1,
     523,    -1,   525,   526,    -1,    -1,  1292,  1293,  1294,  1295,
    1296,  1297,    -1,    -1,    -1,    -1,    -1,  1303,    -1,    -1,
     543,    -1,   597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   555,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,   101,   102,   103,   104,   105,   106,   107,   108,
     109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,  1351,    -1,   126,   262,    -1,
      -1,  1826,    -1,    -1,    -1,   650,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   278,   660,   661,   146,   147,    -1,
      -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,   157,   158,
     675,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   684,
      -1,    -1,    -1,    67,   689,    -1,    -1,    -1,  1404,    -1,
      -1,    -1,   697,    -1,    -1,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
     663,  1427,   665,   666,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,   737,    -1,   687,   688,    -1,   742,    -1,    -1,
      -1,   694,   126,    -1,   368,   146,  1499,   371,   149,   150,
    1466,  1467,    -1,    -1,   759,   760,    -1,    -1,    -1,    -1,
      -1,   385,   146,   147,    -1,   389,   150,    -1,    -1,    -1,
    1486,   776,    -1,   157,   158,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,  1502,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,  1512,    98,  1514,   804,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   433,
      12,    13,    14,    15,    16,    -1,   170,    -1,    -1,    -1,
      -1,  1537,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     835,    -1,  1548,  1586,    -1,  1551,    -1,   842,    -1,    -1,
      -1,  1557,   847,    -1,    -1,    -1,   147,    -1,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1572,    -1,   863,    -1,
      -1,    -1,  1578,    -1,    -1,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   827,    -1,    -1,    -1,  1594,  1595,
     504,    -1,    -1,  1599,  1600,   838,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,   535,    -1,   918,    -1,    -1,    -1,    -1,    -1,    -1,
     544,    -1,   546,    -1,   126,    -1,    -1,    -1,    -1,   934,
      -1,  1684,   937,   557,   939,    -1,    -1,    -1,    -1,   944,
    1656,  1657,    -1,    -1,   146,   147,    -1,    -1,   150,    -1,
    1703,    -1,    -1,    -1,    -1,   157,   158,    98,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   922,
      -1,    -1,    -1,   597,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   986,   987,   936,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   947,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,   956,    -1,    -1,   147,    -1,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    78,    98,    -1,
      -1,   101,   102,   103,   104,   105,   106,   107,   108,   109,
      -1,    -1,  1748,    -1,    -1,  1040,   660,   661,    -1,    -1,
      -1,    -1,  1758,    -1,   997,    -1,   126,    -1,    -1,  1765,
      -1,    55,    -1,    -1,    12,    13,    14,    15,    16,    -1,
     684,    -1,    -1,    -1,    -1,    -1,   146,   147,    -1,    -1,
     150,    -1,    -1,   697,  1790,  1080,    -1,   157,   158,    -1,
      -1,    -1,   143,  1088,    -1,    -1,    -1,    -1,    -1,    -1,
    1806,    -1,    -1,    -1,    98,    -1,   157,   158,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1822,  1823,    -1,    67,
    1826,    -1,  1117,   737,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,
    1135,    -1,    -1,    -1,    -1,   759,   760,   141,    -1,    -1,
      98,    -1,  1858,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   776,  1158,   158,    98,    -1,    -1,   101,   102,
     103,   104,   105,   106,   107,   108,   109,    -1,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1892,    -1,    -1,   240,
     804,  1134,    -1,   126,   188,    -1,    -1,    -1,   146,   147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,  1206,    -1,   146,   147,    -1,    -1,    -1,    -1,    -1,
    1215,   835,    -1,    -1,   157,   158,    98,    -1,   842,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,  1234,
    1235,    12,    13,    14,    15,    16,   297,    -1,    -1,   863,
      -1,    -1,    -1,  1248,  1249,    -1,    -1,    -1,  1253,  1254,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1210,    -1,    -1,
      -1,    -1,    -1,  1216,   146,   147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   278,    -1,    -1,  1282,  1283,    -1,
      -1,   342,    -1,    -1,    -1,    -1,    67,  1292,  1293,  1294,
    1295,    -1,    -1,    -1,   918,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,   368,    -1,    -1,
     934,    -1,    -1,   937,    -1,   939,    -1,    98,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
    1303,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,
      -1,    -1,   986,   987,   368,   146,   147,   371,    -1,    -1,
      -1,    -1,   433,    -1,    -1,    -1,   157,   158,   146,   147,
      -1,   385,    -1,    -1,    -1,   389,    98,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,  1351,  1404,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,   126,    -1,  1040,    -1,    -1,    -1,
      -1,    -1,  1427,    -1,    -1,    -1,    -1,    -1,   126,   433,
      -1,    -1,    -1,    -1,   146,   147,    -1,   498,   499,    -1,
      -1,    -1,    -1,   504,    -1,   157,   158,    -1,   146,   147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,  1466,  1467,    98,  1088,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1492,    -1,    -1,
      -1,    -1,    -1,  1117,    -1,    -1,   557,  1502,    -1,    -1,
     504,    -1,    -1,    -1,    -1,    -1,    -1,  1512,    -1,  1514,
      -1,  1135,   147,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   535,  1537,    -1,  1158,    -1,   597,    -1,    -1,    -1,
     544,    -1,   546,  1548,    -1,    -1,  1551,    -1,    -1,    -1,
      -1,    -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1572,    -1,    -1,
      -1,    -1,    -1,  1578,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1206,    -1,    -1,    -1,    -1,    -1,    -1,  1594,
    1595,    -1,    -1,   597,  1599,  1600,    -1,   658,    -1,    -1,
     661,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1234,  1235,    -1,    -1,  1619,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   684,  1248,  1249,    -1,    -1,    -1,  1253,
    1254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   705,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1657,    -1,    -1,    -1,   660,   661,  1282,  1283,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   737,    -1,    -1,    -1,
     684,     1,    -1,  1636,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   697,    -1,    -1,    -1,    -1,   759,   760,
      -1,    -1,    -1,  1656,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   737,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1758,    -1,   759,   760,    -1,    78,    -1,
    1765,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   776,    -1,  1727,    -1,    -1,    -1,    98,    -1,
    1404,   842,    -1,    -1,    -1,  1790,    -1,   848,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1748,    55,    -1,    -1,    -1,
     804,  1806,   863,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,  1822,  1823,    78,
      -1,   141,    -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   835,    -1,    -1,    -1,    -1,    -1,   157,   842,    98,
      -1,    -1,  1466,  1467,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1858,    -1,   175,    -1,    -1,    -1,   863,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,    -1,    -1,
    1875,    -1,    -1,  1826,    -1,    -1,   135,    -1,  1502,    -1,
      -1,    -1,   141,    -1,   143,    -1,    -1,  1892,  1512,    -1,
    1514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   157,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,  1537,   918,    -1,   175,   237,    -1,    -1,
     240,    -1,    -1,    -1,  1548,   245,    -1,    -1,   187,    -1,
     934,    -1,    -1,   937,    -1,   939,    -1,    -1,    -1,    -1,
      45,    -1,    -1,    -1,    49,    -1,    51,    -1,  1572,    -1,
      -1,    -1,    -1,    -1,  1578,  1908,    -1,    -1,   278,    -1,
      -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
    1594,  1595,    -1,    -1,    -1,  1599,  1600,   297,   237,  1040,
      -1,   240,   986,   987,    -1,    -1,   245,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   328,   114,
     115,   116,    -1,   118,   119,    -1,    -1,    -1,    -1,   278,
      -1,   126,   342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1657,    -1,    -1,  1040,    -1,   297,    -1,
      -1,   146,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,   156,   157,   158,   159,   160,   161,   162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   342,  1088,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1154,    -1,    -1,    -1,  1158,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   429,
      -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1135,    -1,    -1,  1758,    -1,    -1,    -1,    -1,    -1,
      -1,  1765,    -1,    -1,    -1,  1206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1158,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1790,    -1,    -1,    -1,
     429,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   498,   499,
      -1,    -1,  1806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,  1822,  1823,
      -1,    -1,  1206,    -1,    -1,    -1,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     540,    -1,    -1,    -1,   544,    -1,   546,    -1,    -1,    -1,
    1234,  1235,    -1,    -1,  1858,    -1,    -1,    -1,    -1,   498,
     499,    -1,    -1,    -1,  1248,  1249,    -1,    -1,    -1,  1253,
    1254,   571,    -1,   573,   574,    -1,   576,    -1,    -1,   579,
      -1,    -1,   582,   583,   584,    -1,    -1,    -1,  1892,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1282,  1283,
      -1,   540,    -1,     0,    -1,   544,     3,   546,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   571,    -1,   573,   574,    -1,   576,    -1,    -1,
     579,    -1,    -1,   582,   583,   584,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   658,    -1,
      -1,    -1,    -1,  1404,    -1,    -1,   312,    -1,    -1,   315,
     316,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
     326,   327,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   342,   343,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   705,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   658,
    1404,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1486,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   407,    -1,    -1,    -1,    -1,   705,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1466,  1467,    -1,    -1,    -1,   433,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1502,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   224,  1512,    -1,
    1514,    -1,    -1,    -1,    -1,   835,    -1,    -1,    -1,    -1,
      -1,    -1,   239,    -1,    -1,    -1,    -1,   847,   848,    -1,
      -1,    -1,   249,  1537,    -1,    -1,    -1,    -1,  1599,  1600,
      -1,    -1,   259,    -1,  1548,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   273,   274,    -1,    -1,
      -1,    -1,    -1,   280,   281,    -1,    -1,    -1,  1572,    -1,
      -1,    -1,    -1,    -1,  1578,    -1,   835,    -1,    -1,   296,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,   847,   848,
    1594,  1595,    88,    89,    -1,  1599,  1600,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     367,    -1,    -1,  1657,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   629,   630,   631,   632,   633,   634,   635,
     636,   637,   638,   639,   640,   641,   642,   643,   644,   645,
     646,   647,    -1,    -1,    -1,    -1,    -1,    -1,   405,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     427,    -1,    -1,    -1,   431,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1790,
      -1,    -1,    -1,   450,    -1,    -1,    -1,   454,   455,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   463,   464,   465,   466,
      -1,    -1,    -1,    -1,  1758,    -1,    -1,    -1,    -1,    -1,
    1080,  1765,    -1,    -1,    -1,   482,    -1,    -1,  1088,    -1,
      -1,    -1,    -1,   490,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1790,   753,    -1,    -1,
      -1,   287,    -1,    -1,    -1,    -1,    -1,  1117,    -1,    -1,
      -1,   518,  1806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1135,    -1,    -1,  1822,  1823,
      -1,  1080,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1088,
      -1,    -1,   549,    -1,  1154,    -1,    -1,    -1,    -1,   556,
      -1,    -1,  1162,    -1,   561,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1858,    -1,    -1,    -1,  1117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   585,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1135,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1892,    -1,
      -1,    -1,    -1,   859,    -1,  1154,    -1,    -1,   864,    -1,
      -1,    -1,    -1,  1162,    -1,    -1,    -1,    -1,    -1,   875,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   649,    -1,    -1,    -1,   432,    -1,   434,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   443,   444,    -1,
     916,    -1,    -1,    -1,   671,   672,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1292,  1293,  1294,  1295,  1296,  1297,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     727,    -1,    -1,    -1,   731,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   740,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1292,  1293,  1294,  1295,  1296,  1297,    -1,
      -1,     3,    -1,    -1,    -1,   541,    -1,   764,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    19,   775,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    47,    48,    -1,    50,    -1,
      -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,
      -1,    -1,   819,    -1,    -1,    67,    -1,  1427,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1088,    -1,    -1,    -1,    -1,    -1,   845,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   853,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,  1486,    -1,  1427,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,    -1,  1512,    44,  1514,   157,   158,    -1,    -1,  1165,
    1166,  1167,    -1,    -1,    -1,    -1,  1172,  1173,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1537,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1486,    -1,  1195,
      -1,  1551,    -1,    -1,    -1,    -1,    -1,  1557,    -1,    -1,
    1206,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1512,    -1,  1514,    -1,   753,    -1,    -1,
      -1,    -1,   979,    -1,   115,    -1,   983,    -1,    -1,    -1,
      -1,    -1,    -1,   990,    -1,    -1,    -1,   128,  1537,   130,
      -1,    -1,    -1,  1000,    -1,    -1,    -1,    -1,    -1,    -1,
    1007,    -1,  1551,    -1,    -1,    -1,    -1,    -1,  1557,  1016,
      -1,  1018,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
    1027,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1035,    -1,
       5,    -1,    -1,    44,    -1,    -1,   177,    12,    13,    14,
      15,    16,   828,   829,  1051,    -1,    -1,  1657,  1055,    -1,
      -1,    -1,    -1,   839,   840,   841,    -1,    -1,   844,    -1,
      -1,    -1,  1069,    -1,    -1,  1072,    -1,    -1,    -1,    -1,
      45,    -1,    -1,   214,    49,    -1,    51,   218,    -1,    -1,
     221,   222,    -1,    -1,   225,    -1,    -1,   228,   229,    -1,
      -1,   877,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,  1657,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,   130,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   114,
     115,   116,    -1,   118,   119,    -1,    -1,    -1,  1758,   290,
      -1,   126,   293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,   156,   157,   158,   159,   160,   161,   162,    -1,    -1,
     976,    -1,    -1,    -1,    -1,    -1,  1806,  1204,    -1,    -1,
      -1,  1457,  1458,    -1,    -1,    -1,    -1,    -1,    -1,  1758,
     221,   222,  1822,    -1,   225,    -1,    -1,   228,   229,    -1,
      -1,  1228,    -1,    -1,    -1,   366,    -1,  1013,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1022,  1023,  1024,  1025,
     381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1858,    -1,
      -1,    -1,    -1,    -1,    -1,  1041,    -1,  1806,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1274,    -1,    -1,
      -1,  1278,    -1,  1822,    -1,    -1,  1062,    -1,  1064,    -1,
      -1,   422,    -1,    -1,    -1,    -1,  1542,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1308,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1858,
      -1,    -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   467,    -1,    -1,    -1,
      -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,   479,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1358,    -1,    -1,  1361,   366,    -1,  1143,    -1,    -1,
      -1,    -1,    -1,  1149,    -1,  1151,  1152,    -1,    -1,    -1,
     381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1399,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1410,    -1,    -1,    -1,    -1,    -1,    -1,
    1196,   422,   553,    -1,    -1,    -1,    -1,    -1,    -1,  1205,
      -1,  1207,    -1,  1209,    -1,  1211,    -1,    -1,  1435,    -1,
    1216,    -1,  1439,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1453,  1454,    -1,    -1,
    1236,  1237,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   479,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1263,  1264,    -1,
      -1,    -1,  1738,    -1,    -1,  1271,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   653,   654,    -1,  1301,  1772,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   679,   680,
      -1,    -1,    -1,    -1,    -1,  1552,  1553,    -1,    -1,    -1,
      -1,   692,    -1,    -1,    -1,  1341,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   722,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1854,  1855,
      -1,    -1,   743,    -1,    -1,   746,   747,    -1,   749,    -1,
     751,   752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1408,    -1,    -1,  1881,    -1,    -1,    -1,    -1,
    1416,    -1,  1418,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   653,   654,  1430,  1431,    -1,   788,    -1,    -1,
    1906,   792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1445,
    1446,    -1,  1448,    -1,    -1,    -1,  1673,    -1,   679,   680,
    1456,    -1,    -1,    -1,    -1,    -1,  1462,    -1,    -1,    -1,
      -1,   692,  1468,  1469,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1953,  1705,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1713,    -1,    -1,    -1,
      -1,   722,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   743,  1740,    -1,   746,   747,    -1,   749,    -1,
     751,   752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1761,    -1,    -1,  1764,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   788,   158,    -1,
      -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1579,  1580,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1589,   945,    -1,    -1,   187,   188,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1602,    -1,    -1,    -1,
      -1,    -1,    -1,  1609,  1610,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   218,    -1,
      -1,    -1,  1849,    -1,    -1,   225,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   996,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1045,    -1,    -1,  1048,    -1,  1695,
      -1,  1697,    67,   293,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1708,  1709,    -1,    -1,    -1,    -1,    -1,  1715,
      -1,    -1,    -1,    -1,   945,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,   101,   102,   103,   104,
     105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   359,
      -1,    -1,    -1,    -1,    -1,   996,    -1,    -1,   368,   369,
      -1,   146,   147,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,   389,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1811,    -1,    -1,    -1,    -1,
      -1,    -1,  1818,    -1,  1045,    -1,    -1,  1048,  1824,    -1,
      -1,    -1,   422,   423,    -1,   425,   426,    -1,    -1,    -1,
      -1,    -1,    -1,   433,    -1,    -1,  1197,   437,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,
    1221,    -1,    -1,    -1,    -1,    -1,  1872,    -1,   468,    -1,
      -1,    -1,   472,    -1,    -1,    -1,    -1,    -1,  1239,    -1,
      -1,  1242,    -1,  1244,  1245,    -1,    -1,    -1,    -1,    -1,
    1896,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1904,    -1,
      -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,    -1,
      -1,   157,    -1,  1919,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1285,    -1,    -1,    -1,    -1,   175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   554,    -1,   202,   557,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1197,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    78,  1220,
    1221,  1352,    -1,    -1,    -1,    -1,    -1,   597,    -1,    -1,
      -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,  1239,    -1,
      -1,  1242,    -1,  1244,  1245,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,   649,
     650,    -1,    -1,   143,  1285,    -1,    -1,   147,    -1,    -1,
     660,   661,    -1,    -1,    -1,    -1,    -1,   157,   158,    -1,
      -1,   671,    -1,  1434,    -1,   675,    -1,    -1,    -1,    -1,
      -1,    -1,   682,    -1,   684,   175,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   188,    -1,
      -1,   191,    -1,  1464,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1352,   722,   723,    -1,   725,   726,    -1,    -1,  1490,
      -1,    -1,    -1,    -1,    -1,  1496,    -1,   737,    -1,    -1,
     740,    -1,   742,   743,    -1,    -1,    -1,    -1,    -1,   749,
     240,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   759,
     760,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,    -1,
     780,    -1,    -1,    -1,   784,    -1,  1547,    -1,   788,   789,
      -1,    -1,   792,   793,    -1,    -1,    -1,    -1,    -1,   289,
     800,    -1,    -1,  1434,    -1,    -1,    -1,   297,    -1,    -1,
      -1,   457,    -1,    -1,   460,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1464,    -1,    -1,    -1,    -1,   328,    -1,
      -1,    -1,   842,   843,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   863,  1625,  1626,    -1,    -1,    -1,    -1,
      -1,    -1,  1633,    -1,    -1,    -1,  1637,    -1,   368,    -1,
     370,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1547,    -1,    -1,    -1,
      -1,    -1,   922,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   578,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   433,    -1,    -1,    -1,   947,    -1,   439,
     596,   597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   607,    -1,   609,   610,    -1,    -1,    -1,  1729,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     990,   991,    -1,    -1,  1625,  1626,    -1,   997,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1637,    -1,   498,   499,
      -1,    -1,   658,    -1,   504,   661,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   674,    -1,
      -1,    -1,    -1,    -1,  1795,    -1,    -1,    -1,    -1,    -1,
    1040,    -1,    -1,    -1,    -1,  1045,  1046,    -1,  1048,  1049,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,
      -1,   717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   571,    -1,   573,   574,    -1,   576,    -1,   734,   579,
      -1,   737,   582,   583,   584,    -1,    -1,    -1,  1729,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   597,    -1,    -1,
      -1,    -1,    -1,  1874,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    19,   769,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      47,    48,    -1,    50,    -1,   801,    -1,    -1,  1158,    -1,
      -1,    -1,    -1,    -1,  1795,    -1,    -1,    -1,   658,    -1,
      67,   661,    -1,   663,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   684,    -1,    -1,  1197,  1198,    -1,
      -1,    -1,   848,    -1,   101,   102,  1206,    -1,    -1,    -1,
      -1,    -1,  1212,  1213,    -1,   705,    -1,   863,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   871,    -1,    -1,    -1,   126,
      -1,    -1,   878,    -1,    -1,    -1,    -1,    -1,    -1,  1239,
    1240,    -1,  1242,    -1,    -1,    -1,    -1,   737,    -1,   895,
      -1,    -1,   149,  1253,  1254,    -1,    -1,    -1,    -1,    -1,
     157,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   759,
     760,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,    -1,
     936,   937,    -1,    -1,    -1,    -1,    -1,    -1,   944,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,     9,    -1,   965,
      12,    13,    14,    15,    16,    17,    -1,    19,   974,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,   842,    -1,    -1,    47,    48,    -1,   848,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,   863,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,  1029,    -1,  1031,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1039,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1404,    97,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,    -1,   145,    -1,    -1,    -1,   149,   150,  1105,
    1106,    -1,    -1,    -1,    -1,   157,   158,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1496,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1175,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1188,    -1,    -1,    -1,    -1,    -1,  1548,    -1,
    1040,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1210,    -1,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1080,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1599,
    1600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1618,  1619,
    1266,    -1,  1268,    -1,    -1,    -1,    -1,  1273,    -1,    -1,
      -1,    -1,    -1,    -1,  1634,    -1,    -1,    45,    -1,    -1,
      -1,    49,    -1,    51,  1134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,  1154,  1311,    -1,    -1,  1158,    -1,
      -1,    -1,  1162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1329,    -1,    -1,  1332,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   114,   115,   116,    -1,
     118,   119,    -1,    -1,    -1,    -1,  1206,    -1,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1727,    -1,    -1,
      -1,    -1,    -1,    -1,  1380,  1735,  1382,    -1,   146,   147,
      -1,   149,   150,    -1,    -1,    -1,   154,    -1,   156,   157,
     158,   159,   160,   161,   162,    -1,    -1,    -1,  1404,    -1,
      -1,    -1,    -1,  1253,  1254,    -1,    -1,    -1,  1414,  1415,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1790,    -1,    -1,    -1,    -1,  1795,  1796,    -1,    -1,  1799,
      -1,    -1,  1292,  1293,  1294,    -1,  1296,  1297,    -1,    -1,
      -1,    -1,    -1,  1303,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1825,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1486,    -1,    -1,    -1,    -1,  1491,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1874,  1875,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1533,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1908,  1555,
      -1,    -1,  1558,    -1,  1404,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    45,    -1,
      47,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    -1,    64,    -1,    66,
      67,    68,    69,    -1,    71,    -1,  1486,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    -1,    93,    -1,    95,    96,
      97,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1557,   145,   146,
     147,    -1,   149,   150,    -1,    -1,    -1,   154,    -1,   156,
     157,   158,   159,   160,   161,   162,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1599,
    1600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,  1656,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    -1,    -1,    -1,
      64,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    93,
      -1,    -1,    -1,    97,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,  1748,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,  1765,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,   147,    -1,   149,   150,    -1,    -1,    -1,
    1790,    -1,    -1,   157,   158,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1823,    -1,    -1,  1826,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    45,
      -1,    47,    48,    49,    -1,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,    -1,    -1,    64,    -1,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    -1,    93,    -1,    95,
      96,    97,    98,    -1,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,   147,    -1,   149,   150,    -1,    -1,    -1,   154,    -1,
     156,   157,   158,   159,   160,   161,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   171,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    45,    -1,
      47,    48,    49,    -1,    51,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    -1,    -1,    -1,    64,    -1,    -1,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    96,
      97,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   114,   115,   116,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
      -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,
     157,   158,   159,   160,   161,   162,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   170,   171,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    45,    -1,    47,
      48,    49,    -1,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    -1,    -1,    -1,    64,    -1,    -1,    67,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    96,    97,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   114,   115,   116,    -1,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,   157,
     158,   159,   160,   161,   162,     4,     5,     6,     7,     8,
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
     109,   110,   111,   112,    -1,   114,   115,   116,    -1,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,
     159,   160,   161,   162,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    -1,    -1,    45,    -1,    47,    48,    49,
      -1,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   114,   115,   116,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,
     160,   161,   162,     4,     5,     6,     7,     8,     9,    10,
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
     111,   112,    -1,   114,   115,   116,    -1,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,   160,
     161,   162,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    45,    -1,    47,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   114,   115,   116,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,    -1,    -1,   149,   150,    -1,
      -1,    -1,    -1,    -1,   156,   157,   158,   159,   160,   161,
     162,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    -1,    46,    47,    48,    -1,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,    67,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      -1,    46,    47,    48,    -1,    50,    98,    -1,    -1,   101,
     102,   103,   104,   105,   106,   107,   108,   109,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,   147,    -1,   149,   150,    -1,
      -1,    -1,   154,    -1,    -1,   157,   158,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    -1,    -1,    -1,    64,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    97,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    -1,    -1,    -1,    64,    -1,    -1,    67,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,    -1,
      -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   145,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    19,    67,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      47,    48,    -1,    50,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,   147,    -1,   149,   150,    -1,    -1,     3,
      -1,    -1,    -1,   157,   158,     9,    -1,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   145,    -1,    -1,    -1,   149,   150,    -1,    -1,     3,
      -1,    -1,    -1,   157,   158,     9,    -1,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   145,    -1,    -1,    -1,   149,   150,    -1,    -1,     3,
      -1,    -1,    -1,   157,   158,     9,    -1,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   145,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    -1,    46,    47,
      48,    -1,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    19,    67,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,    49,
      -1,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,    66,    -1,    68,   157,
     158,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    -1,    93,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   146,   147,    -1,   149,
     150,    -1,    -1,    -1,   154,    -1,   156,   157,   158,   159,
     160,   161,   162,    45,    -1,    -1,    -1,    49,    -1,    51,
      -1,   171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    66,    -1,    68,    69,    -1,    71,
      -1,    -1,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      -1,    93,    -1,    95,    96,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,   147,    -1,   149,   150,    -1,
      -1,    -1,   154,    -1,   156,   157,   158,   159,   160,   161,
     162,    45,    -1,    -1,    -1,    49,    -1,    51,    -1,   171,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    66,    -1,    68,    -1,    -1,    71,    -1,    -1,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    -1,    93,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,   147,    -1,   149,   150,    -1,    -1,    -1,
     154,    -1,   156,   157,   158,   159,   160,   161,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    47,    48,    -1,    50,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    -1,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,   147,    -1,   149,   150,    -1,    -1,    -1,   154,    -1,
      -1,   157,   158,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    47,    48,    -1,    50,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,   147,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    -1,    12,    13,    14,    15,
      16,    67,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    -1,
      46,    47,    48,    -1,    50,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,   149,   150,   101,   102,    -1,    -1,    -1,
      -1,   157,   158,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,   149,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,   128,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,   128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,   157,   158,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    -1,    46,    47,    48,    -1,    50,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    45,    -1,    47,    48,    49,    -1,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     114,   115,   116,    -1,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,    -1,    -1,   149,   150,    -1,    -1,    -1,
     154,    -1,   156,   157,   158,   159,   160,   161,   162,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    45,    -1,    47,    48,    49,    -1,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   114,   115,   116,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   146,    -1,   148,   149,   150,    -1,    -1,
      -1,    -1,    -1,   156,   157,   158,   159,   160,   161,   162,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    45,    -1,    47,    48,    49,    -1,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   114,   115,   116,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,    -1,    -1,   149,   150,    -1,
      -1,    -1,   154,    -1,   156,   157,   158,   159,   160,   161,
     162,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    45,    -1,    47,    48,    49,    -1,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   114,   115,   116,    -1,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,   160,
     161,   162,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    -1,    -1,    45,    -1,    47,    48,    49,
      -1,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   114,   115,   116,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,
     160,   161,   162,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    45,    -1,    47,    48,
      49,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    68,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   114,   115,   116,    -1,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,
     159,   160,   161,   162,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,
      48,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,   101,   102,   103,   104,   105,   106,   107,
     108,   109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,   147,
      -1,   149,   150,    -1,    -1,    -1,   154,    -1,    -1,   157,
     158,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,   147,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,
      54,    55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   147,    -1,   149,   150,    -1,    -1,    -1,
      -1,    -1,    -1,   157,   158,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,
      48,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,   101,   102,    47,    48,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,   149,   150,    -1,
      -1,    -1,    -1,    -1,    -1,   157,   158,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,    -1,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,
      48,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      -1,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,   101,   102,    -1,    -1,    47,    48,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,    -1,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,   101,   102,
      -1,    -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    67,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,    -1,   157,   158,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,
      48,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      -1,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,   101,   102,    -1,    -1,    47,    48,    -1,
      -1,    -1,    52,    53,    54,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,    -1,   157,   158,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    -1,    46,    47,    48,    -1,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    67,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    -1,    -1,   101,   102,    47,
      48,    -1,    -1,    -1,    52,    53,    54,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,   157,
     158,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,
      -1,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
     101,   102,    -1,    -1,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    67,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   157,   158,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
      -1,    -1,    -1,    -1,   157,   158,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    12,    13,    14,    15,    16,
      -1,    67,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    -1,    46,
      47,    48,    -1,    50,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   101,   102,    -1,    -1,    -1,    -1,
      -1,   157,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,
     157,   158,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    -1,    -1,    -1,    47,    48,    -1,    -1,    -1,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,   149,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    -1,
      -1,    47,    48,    -1,    -1,    -1,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,   149,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,    -1,    -1,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,   101,   102,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    -1,    46,    47,    48,    -1,    50,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    67,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    -1,    -1,   101,   102,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      67,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,
      -1,    98,    -1,    -1,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,    -1,    -1,    -1,    47,    48,
     147,    -1,   149,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    67,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      -1,    -1,   101,   102,    47,    48,    -1,    -1,    -1,    52,
      53,    54,    55,    56,    57,    58,    59,    45,    -1,    -1,
      -1,    49,    -1,    51,    67,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      68,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   114,   115,   116,    -1,
     118,   119,    -1,    -1,    -1,    -1,    45,    -1,   126,    -1,
      49,    -1,    51,    -1,    -1,    -1,   149,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    68,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,   157,
     158,   159,   160,   161,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   170,   171,    -1,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   114,   115,   116,    45,   118,
     119,    -1,    49,    -1,    51,    -1,    -1,   126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    -1,    -1,    -1,    -1,    -1,   146,   147,    -1,
     149,   150,    -1,    -1,    -1,   154,    -1,   156,   157,   158,
     159,   160,   161,   162,    -1,    -1,    -1,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   114,   115,   116,
      45,   118,   119,    -1,    49,    -1,    51,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,   146,
      -1,    -1,   149,   150,    -1,    -1,    -1,   154,    -1,   156,
     157,   158,   159,   160,   161,   162,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   114,
     115,   116,    45,   118,   119,    -1,    49,    -1,    51,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
      -1,   146,   147,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,   156,   157,   158,   159,   160,   161,   162,    -1,    -1,
      -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   114,   115,   116,    45,   118,   119,    -1,    49,    -1,
      51,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,
      -1,    -1,    -1,   146,    -1,    -1,   149,   150,    -1,    -1,
      -1,   154,    -1,   156,   157,   158,   159,   160,   161,   162,
      -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   114,   115,   116,    45,   118,   119,    -1,
      49,    -1,    51,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,    -1,    -1,    -1,    -1,   146,   147,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,   160,
     161,   162,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   114,   115,   116,    45,   118,
     119,    -1,    49,    -1,    51,    -1,    -1,   126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
     149,   150,    -1,    -1,   153,    -1,    -1,   156,   157,   158,
     159,   160,   161,   162,    -1,    -1,    -1,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   114,   115,   116,
      45,   118,   119,    -1,    49,    -1,    51,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,   146,
     147,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,
     157,   158,   159,   160,   161,   162,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   114,
     115,   116,    45,   118,   119,    -1,    49,    -1,    51,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,   149,   150,    -1,    -1,    -1,   154,
      -1,   156,   157,   158,   159,   160,   161,   162,    -1,    -1,
      -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   114,   115,   116,    45,   118,   119,    -1,    49,    -1,
      51,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,
      -1,    -1,    -1,   146,    -1,   148,   149,   150,    -1,    -1,
      -1,    -1,    -1,   156,   157,   158,   159,   160,   161,   162,
      -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   114,   115,   116,    45,   118,   119,    -1,
      49,    -1,    51,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,    -1,    -1,    -1,    -1,   146,   147,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,   160,
     161,   162,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   114,   115,   116,    45,   118,
     119,    -1,    49,    -1,    51,    -1,    -1,   126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,
     159,   160,   161,   162,    -1,    -1,    -1,    -1,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   114,   115,   116,
      45,   118,   119,    -1,    49,    -1,    51,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,    -1,   146,
      -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,   156,
     157,   158,   159,   160,   161,   162,    -1,    -1,    -1,    -1,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   114,
     115,   116,    45,   118,   119,    -1,    49,    -1,    51,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,   156,   157,   158,   159,   160,   161,   162,    -1,    -1,
      -1,    -1,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   114,   115,   116,    45,   118,   119,    -1,    49,    -1,
      51,    -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    -1,    -1,
      -1,    -1,    -1,   146,    -1,    -1,   149,   150,    -1,    -1,
      -1,    -1,    -1,   156,   157,   158,   159,   160,   161,   162,
      -1,    -1,    -1,    -1,    95,    96,    -1,    98,    -1,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   114,   115,   116,    45,   118,   119,    -1,
      49,    -1,    51,    -1,    -1,   126,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,   156,   157,   158,   159,   160,
     161,   162,    -1,    -1,    -1,    -1,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   114,   115,   116,    -1,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   156,   157,   158,
     159,   160,   161,   162
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   173,   380,   381,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    47,    48,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    64,    67,    68,    93,    97,    98,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   113,   126,
     146,   147,   149,   150,   157,   158,   176,   177,   191,   272,
     273,   274,   275,   276,   277,   278,   279,   280,   281,   282,
     283,   285,   287,   289,   290,   291,   292,   293,   294,   295,
     296,   297,   299,   301,   302,   303,   305,   306,   310,   311,
     312,   313,   314,   316,   322,   323,   324,   325,   336,   339,
     372,   375,   385,   390,   392,   398,   402,   407,   408,   409,
     410,   411,   412,   413,   414,   434,   451,   452,   453,   454,
       0,   173,   177,   191,   276,   278,   287,   290,   302,   306,
     311,   112,   146,    53,    56,    57,    59,   146,   146,   396,
     397,   398,   298,   299,   101,   102,   177,   352,   373,   374,
     352,   146,   385,   146,   146,   146,   191,   397,   402,   408,
     409,   410,   412,   413,   414,   101,   313,   151,   173,   279,
     287,   290,   407,   411,   450,   451,   454,   455,   171,   174,
     143,   170,   212,   355,    84,   152,   391,   352,   174,   174,
     174,   171,   101,   102,   146,   191,   284,   393,   402,   403,
     404,   405,   406,   407,   411,   415,   416,   417,   418,   419,
     425,     3,    43,    44,    46,    50,   304,     3,     4,   150,
     191,   278,   291,   295,   297,   307,   312,   387,   407,   411,
     454,   276,   278,   290,   302,   306,   311,   388,   407,   411,
      60,   296,   296,   291,   297,   296,   291,   296,   291,   149,
     396,   152,   174,   146,   154,   220,   396,   396,   173,   267,
     268,   150,   287,   290,   452,   352,   352,   385,   170,   290,
     146,   191,   393,   402,   407,   416,   150,   191,   454,   386,
      60,    61,    62,    63,   150,   168,   352,   361,   363,   367,
     369,   370,    45,    49,    51,    68,    95,    96,    98,   100,
     110,   111,   112,   114,   115,   116,   118,   119,   146,   150,
     156,   159,   160,   161,   162,   175,   176,   178,   179,   180,
     183,   190,   191,   192,   193,   196,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   209,   214,   287,
     312,   353,   354,   371,   450,   455,    52,   150,   191,   286,
     290,   294,   295,   301,   302,   308,   309,   310,   311,   315,
     322,   323,   339,   348,   350,   434,   446,   447,   448,   449,
     454,   455,   101,   102,   154,   177,   312,   425,   398,   146,
     368,   369,   146,   146,   178,   150,   190,   191,   207,   208,
     312,   148,   371,   290,   408,   409,   410,   412,   413,   414,
     148,   148,   148,   148,   148,   148,   148,   150,   287,   434,
     452,   150,   157,   191,   209,   278,   279,   286,   288,   290,
     302,   309,   311,   343,   344,   347,   348,   349,   446,   454,
     146,   407,   411,   454,   146,   152,    21,   154,   209,   356,
     146,   352,   220,   146,   152,   152,   152,   397,   402,   404,
     405,   406,   415,   417,   418,   419,   290,   403,   416,   152,
      93,   395,   150,   396,   433,   434,   396,   396,   391,   267,
     146,   396,   433,   391,   396,   396,   290,   393,   146,   146,
     289,   290,   287,   290,   173,   287,   450,   455,   314,   154,
     391,   267,   352,   355,   278,   295,   389,   407,   411,   154,
     391,   267,   373,   290,   302,   290,   290,   101,   313,   101,
     102,   177,   312,   317,   373,   173,   177,   351,   145,   173,
       3,   283,   285,   290,   294,   220,   173,   173,   395,   146,
     395,   174,   209,   397,   402,   290,   146,   173,   352,   154,
     352,   154,   352,   128,   157,   158,   366,   148,   152,   352,
     370,   146,   190,   146,   146,   193,   146,   190,   146,   146,
     190,   190,    18,    20,    80,   150,   159,   160,   194,   195,
     209,   216,   220,   325,   353,   454,   152,   173,   146,   180,
     155,   155,   112,   115,   117,   118,   119,   146,   149,   150,
     154,   155,   129,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   170,   211,   212,   213,   193,   193,   163,
     157,   164,   165,   159,   160,   120,   121,   122,   123,   166,
     167,   124,   125,   158,   156,   168,   126,   127,   169,   146,
     191,   429,   430,   431,   432,   433,   149,   148,   152,   396,
     151,   173,   288,   290,   302,   309,   311,   445,   446,   454,
     455,   146,   150,   158,   170,   191,   434,   435,   436,   437,
     438,   439,   440,   457,   191,   315,   454,   290,   309,   296,
     291,   396,   148,   288,   290,   447,   288,   434,   447,     9,
     340,   352,   337,   154,   361,   170,   361,    12,    83,    98,
     101,   102,   176,   399,   400,   401,   148,   173,   148,   152,
     148,   148,   148,   148,   148,   148,   148,   146,   396,   433,
     434,   146,   433,   434,   173,   287,   452,   173,   174,   174,
     146,   158,   191,   402,   420,   421,   422,   423,   424,   425,
     426,   427,   428,   128,   454,   174,   174,   352,   352,   173,
     173,   173,    98,   149,   150,   176,   177,   356,   357,   358,
     359,   360,   147,   209,   215,   146,   173,   173,   173,   173,
     402,   404,   405,   406,   415,   417,   418,   419,   148,   148,
     148,   148,   148,   148,   148,   403,   416,   396,   146,   355,
     151,   173,   220,   391,   173,   220,   393,   216,   354,   216,
     354,   393,   383,   220,   391,   395,   154,   391,   267,   383,
     220,   391,   319,   320,   318,   154,   128,   290,   345,   346,
     349,   350,   148,   152,    65,   269,   270,   174,   290,   283,
     157,   209,   173,   402,   344,   383,   151,   173,   146,   365,
     363,   364,   353,   150,   353,   353,   353,   209,   353,   148,
     353,   353,   353,   173,   148,   159,   160,   195,    17,   292,
     148,   152,   148,   157,   158,   148,   215,   209,   154,   177,
     177,   110,   150,   177,   147,   184,   185,   209,   110,   150,
     177,   325,   209,   184,   177,   154,   209,   193,   196,   196,
     196,   197,   197,   198,   198,   199,   199,   199,   199,   200,
     200,   201,   202,   203,   204,   205,   153,   216,   173,   430,
     431,   432,   290,   429,   396,   396,   150,   177,    73,   300,
     209,   354,   177,   288,   434,   447,   290,   294,   454,   173,
     436,   437,   438,   151,   173,    17,   209,   290,   435,   457,
     396,   396,   434,   288,   445,   455,   290,   177,   396,   288,
     447,   312,   152,   456,   170,   212,   341,   154,   340,   148,
     354,   148,   148,   152,   146,   171,   209,   171,   178,   146,
     396,   433,   434,   146,   433,   434,   173,   173,   151,   151,
     146,   402,   421,   422,   423,   426,    17,   290,   420,   424,
     146,   396,   439,   457,   396,   396,   457,   146,   396,   439,
     396,   396,   174,   208,   352,   151,   152,   151,   152,   457,
     457,   128,   342,   343,   344,   342,   352,   150,   177,   173,
     153,   152,   456,   356,   149,   150,   153,   360,   148,   152,
     173,   342,   177,   393,   177,   148,   148,   148,   148,   148,
     148,   146,   396,   433,   434,   146,   396,   433,   434,   393,
     178,   434,   209,   220,   345,   148,   148,   148,   148,   381,
     382,   220,   383,   220,   391,   382,   220,   154,   154,   154,
     326,   174,   174,   177,   271,   352,    17,    66,    68,    71,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    87,    88,    89,    90,    91,    93,   101,   102,
     113,   173,   216,   217,   218,   219,   220,   221,   222,   224,
     225,   235,   239,   240,   241,   242,   243,   244,   249,   250,
     256,   257,   258,   272,   290,   294,   352,   392,    65,   171,
     174,   174,   174,   342,   174,   382,   276,   278,   287,   376,
     377,   378,   379,   371,   170,   362,   362,   148,   173,   152,
     148,   148,   152,   148,   193,   148,   148,   148,   193,    17,
     292,   209,   148,   148,   147,   154,   193,   151,   174,   184,
     110,   114,   116,   177,   186,   187,   188,   148,   152,   186,
     151,   152,   145,   357,   207,   153,   345,   148,   148,   148,
     429,   186,   288,   447,   150,   157,   191,   209,   312,   209,
     290,   345,   148,   148,   148,     5,   290,   396,   435,   154,
     177,   425,     9,   352,   145,   356,   340,   456,   154,   148,
     400,   184,   174,   148,   173,   173,   345,   345,   426,   148,
     148,   148,   148,   146,   402,   425,   420,   424,   173,   173,
     151,   174,   457,   173,   173,   174,   174,   174,   174,   355,
     173,   207,   208,   209,   394,   356,   358,   145,   173,   147,
     209,   342,   174,   170,   146,   396,   433,   434,   146,   396,
     433,   434,   173,   173,   395,   148,   174,   174,   384,   382,
     220,   384,   326,   326,   326,     3,     9,    68,   145,   273,
     280,   281,   287,   290,   327,   332,   450,   148,   152,   152,
     171,   146,    56,    57,   171,   220,   272,   392,   146,    17,
     218,   146,   146,   171,   352,   171,   352,   157,   352,   154,
     217,   146,   146,   146,   220,   209,   210,   210,    13,   259,
      69,   226,   171,   174,   222,    73,   171,   352,    86,   245,
     351,   290,   153,   271,   171,   151,   151,   174,   152,   384,
     393,   174,   171,   174,   171,   174,   148,   354,   368,   368,
     177,    73,   181,   182,   353,   193,   193,   193,   193,   193,
     154,   357,   152,   145,   189,   150,   187,   189,   189,   151,
     152,   117,   149,   185,   151,   216,   456,   207,   174,   146,
     396,   433,   434,   151,   173,   174,   174,   174,   209,   174,
     146,   396,   439,   434,   289,     5,   157,   174,   209,   340,
     396,   396,   312,   341,   456,   145,   145,   173,   148,   171,
     345,   345,   174,   174,   148,   146,   396,   433,   434,   146,
     396,   439,   402,   396,   396,   345,   345,   151,   344,   347,
     347,   348,   148,   152,   152,   148,   186,   128,   162,   174,
     174,   356,   209,   174,   148,   209,   173,   173,   345,   345,
     355,   396,   152,   148,   145,   384,   145,   145,   145,   145,
     287,   325,   333,   450,   287,   332,   146,   321,   171,   171,
     146,   153,   191,   328,   329,   335,   402,   403,   416,   152,
     171,   352,   173,   352,   184,   171,   220,   171,   220,   216,
      75,   148,   173,   148,   173,   171,   171,   216,   171,   357,
     171,   216,   215,   216,   105,   106,   107,   108,   109,   251,
     253,   254,   171,    92,   171,    79,   146,   146,   174,   145,
     171,   171,   146,   218,   220,   396,   171,   148,   173,   145,
     145,   173,   152,   152,   148,   153,   148,   152,   153,   357,
     456,   215,   115,   186,   187,   150,   187,   150,   187,   151,
     145,   148,   173,   151,   151,   151,   174,   148,   173,   209,
     209,   174,   151,   174,   456,   338,   154,   341,   145,   376,
     174,   174,   148,   148,   173,   173,   174,   174,   174,   173,
     173,   174,   208,   208,   151,   151,   174,   148,   396,   345,
     345,   174,   174,   216,   145,   321,   321,   321,   146,   191,
     330,   331,   433,   441,   442,   443,   444,   171,   152,   171,
     328,   171,   371,   397,   402,   209,   290,   152,   171,   334,
     335,   334,   352,   128,   349,   350,   148,   148,   146,   218,
     216,   227,   272,   274,   277,   283,   290,   294,   218,   170,
     171,   216,   236,   237,   272,   171,   456,   148,   148,   148,
     220,   253,   254,   146,   209,   146,   178,   227,   193,   246,
     104,   218,   396,   377,   173,   173,   209,   182,   209,   456,
     145,   151,   151,   186,   186,   345,   151,   345,   174,   174,
     151,   151,   145,   154,   340,   174,   148,   148,   345,   345,
     148,   148,   151,   152,   128,   344,   128,   151,   174,   174,
     174,   148,   148,   151,   442,   443,   444,   290,   441,   152,
     171,   396,   396,   171,   148,   402,   396,   218,    72,    73,
     154,   230,   231,   232,   148,   216,   148,   216,   290,   216,
     217,   140,   141,   142,   162,   171,   238,   148,   153,   217,
     145,   154,   232,   218,   146,   173,   171,   178,   148,   153,
     148,   148,   152,   153,   244,   248,   352,   393,   145,   151,
     151,   174,   174,   151,   151,   340,   456,   145,   174,   174,
     173,   174,   151,   148,   148,   148,   148,   148,   441,   396,
     329,   208,   228,   229,   394,   153,   173,   218,   230,   171,
     148,   218,   171,   101,   170,   216,   217,   216,   218,   237,
     171,   171,   173,   173,   255,   288,   290,   450,   153,   171,
     150,   178,   260,   261,   262,   218,   193,   184,    70,   103,
     245,   247,   148,   148,   456,   145,   148,   148,   347,   146,
     396,   433,   434,   331,   128,   152,   153,   265,   266,   272,
     171,   174,   217,   216,   141,   162,   238,   171,   162,   174,
     217,   265,   255,   174,   146,   191,   393,   441,   177,   153,
      98,   146,   148,   153,   152,    70,   148,   218,   146,   218,
     218,   145,   173,   208,   228,   231,   233,   234,   272,   147,
     147,   216,   217,   216,   233,   174,   171,   252,   290,   260,
     151,   208,   171,   260,   262,   218,   216,   104,   104,   345,
     218,   223,   174,   231,   162,   162,   162,   174,   252,   207,
     148,   153,   178,   148,   148,   153,   148,   248,    70,   243,
     174,   218,   145,   223,   216,   147,   216,   216,   145,   148,
     220,   178,   263,   146,   171,   263,   218,    70,   148,   220,
     152,   153,   208,   148,   218,   178,   177,   264,   148,   171,
     148,   152,   171,   177
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   172,   173,   174,   175,   175,   175,   175,   175,   176,
     176,   176,   176,   176,   176,   176,   177,   177,   177,   178,
     179,   179,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   181,   181,   182,   182,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   184,   184,   184,   185,
     185,   186,   186,   187,   187,   187,   187,   187,   187,   187,
     188,   188,   188,   189,   189,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   191,
     191,   191,   192,   192,   192,   192,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   194,   194,   194,   194,   195,
     195,   196,   196,   197,   197,   197,   197,   198,   198,   198,
     199,   199,   199,   200,   200,   200,   200,   200,   201,   201,
     201,   202,   202,   203,   203,   204,   204,   205,   205,   206,
     206,   207,   207,   207,   208,   209,   209,   209,   210,   210,
     211,   211,   212,   212,   213,   213,   213,   213,   213,   213,
     213,   213,   213,   213,   213,   214,   214,   215,   215,   215,
     215,   216,   216,   217,   217,   218,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   219,   220,
     220,   221,   221,   222,   222,   222,   222,   222,   223,   223,
     224,   225,   225,   225,   225,   225,   226,   226,   227,   227,
     227,   227,   228,   228,   228,   229,   229,   230,   230,   231,
     231,   232,   233,   233,   234,   234,   235,   235,   235,   235,
     235,   235,   236,   236,   237,   237,   237,   237,   237,   237,
     237,   237,   237,   237,   237,   237,   237,   237,   237,   237,
     237,   237,   238,   238,   238,   238,   239,   239,   239,   239,
     239,   239,   239,   239,   239,   239,   239,   239,   239,   239,
     239,   239,   239,   239,   239,   239,   240,   240,   241,   242,
     243,   244,   244,   245,   245,   246,   246,   247,   248,   248,
     248,   248,   248,   248,   249,   249,   250,   250,   250,   251,
     251,   252,   252,   253,   253,   253,   253,   254,   255,   255,
     255,   255,   255,   256,   257,   257,   258,   258,   258,   258,
     258,   259,   259,   260,   260,   261,   261,   262,   262,   263,
     263,   263,   264,   264,   265,   265,   266,   266,   267,   267,
     268,   268,   269,   269,   270,   270,   271,   271,   272,   272,
     272,   273,   273,   274,   274,   274,   274,   274,   275,   275,
     275,   276,   276,   276,   277,   277,   277,   277,   277,   278,
     278,   279,   279,   280,   280,   280,   281,   281,   281,   281,
     281,   282,   282,   283,   283,   283,   283,   284,   284,   285,
     285,   285,   286,   286,   286,   287,   287,   287,   288,   288,
     288,   289,   289,   290,   290,   291,   291,   292,   292,   292,
     292,   292,   293,   294,   294,   294,   295,   295,   296,   296,
     296,   296,   296,   296,   296,   296,   297,   297,   297,   297,
     297,   297,   297,   297,   297,   297,   297,   297,   297,   297,
     297,   297,   297,   297,   297,   297,   297,   297,   297,   297,
     297,   298,   298,   299,   300,   300,   301,   301,   301,   301,
     301,   302,   302,   303,   303,   303,   303,   304,   304,   304,
     304,   304,   304,   305,   305,   305,   305,   306,   307,   306,
     306,   308,   308,   308,   308,   309,   309,   309,   310,   310,
     310,   310,   311,   311,   311,   312,   312,   312,   312,   312,
     312,   313,   313,   313,   314,   314,   315,   315,   317,   316,
     318,   316,   319,   316,   320,   316,   316,   321,   321,   322,
     322,   323,   323,   324,   324,   324,   325,   325,   325,   325,
     325,   325,   325,   325,   326,   326,   327,   327,   327,   327,
     327,   327,   327,   327,   327,   327,   328,   328,   328,   329,
     329,   329,   330,   330,   330,   331,   332,   332,   333,   333,
     334,   334,   335,   336,   337,   336,   336,   336,   338,   336,
     336,   336,   339,   339,   340,   340,   340,   340,   341,   341,
     342,   342,   342,   342,   342,   342,   342,   343,   343,   343,
     343,   344,   344,   345,   345,   345,   345,   346,   346,   346,
     346,   347,   347,   347,   347,   347,   348,   348,   348,   348,
     348,   349,   349,   350,   350,   351,   351,   352,   352,   352,
     353,   353,   353,   354,   354,   355,   355,   355,   356,   356,
     357,   357,   357,   357,   357,   358,   358,   359,   359,   360,
     360,   360,   360,   360,   361,   361,   362,   362,   364,   363,
     365,   363,   363,   363,   366,   366,   366,   366,   367,   367,
     367,   367,   368,   368,   369,   369,   370,   370,   371,   371,
     371,   371,   372,   372,   372,   373,   373,   374,   374,   375,
     375,   376,   376,   377,   377,   378,   378,   378,   379,   379,
     380,   380,   381,   381,   382,   382,   383,   384,   385,   385,
     385,   385,   385,   386,   385,   387,   385,   388,   385,   389,
     385,   390,   390,   390,   391,   391,   392,   392,   392,   392,
     392,   392,   392,   392,   392,   392,   393,   393,   393,   394,
     395,   395,   396,   396,   397,   397,   398,   399,   399,   400,
     400,   400,   401,   401,   401,   401,   401,   401,   402,   402,
     403,   403,   403,   403,   404,   404,   404,   404,   405,   405,
     405,   405,   405,   405,   405,   406,   406,   406,   406,   407,
     407,   407,   408,   408,   408,   408,   408,   409,   409,   409,
     409,   410,   410,   410,   410,   410,   410,   411,   411,   411,
     412,   412,   412,   412,   412,   413,   413,   413,   413,   414,
     414,   414,   414,   414,   414,   415,   415,   416,   416,   416,
     416,   417,   417,   417,   417,   418,   418,   418,   418,   418,
     418,   418,   419,   419,   419,   419,   419,   420,   420,   420,
     420,   420,   421,   421,   421,   422,   422,   422,   422,   423,
     423,   423,   424,   424,   424,   424,   424,   425,   425,   426,
     426,   426,   427,   427,   428,   428,   429,   429,   429,   430,
     430,   430,   430,   430,   431,   431,   431,   431,   432,   432,
     432,   433,   433,   433,   433,   434,   434,   434,   434,   435,
     435,   435,   435,   436,   436,   436,   436,   436,   437,   437,
     437,   437,   438,   438,   438,   439,   439,   439,   440,   440,
     440,   440,   440,   440,   441,   441,   441,   442,   442,   442,
     442,   442,   443,   443,   443,   443,   444,   444,   445,   445,
     445,   446,   446,   447,   447,   447,   447,   447,   447,   448,
     448,   448,   448,   448,   448,   448,   448,   448,   448,   449,
     449,   449,   449,   450,   450,   450,   451,   451,   452,   452,
     452,   452,   452,   452,   453,   453,   453,   453,   453,   453,
     454,   454,   454,   455,   455,   456,   456,   457,   457
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
       1,     0,     1,     5,     0,     1,     1,     2,     2,     3,
       3,     1,     3,     1,     2,     2,     2,     4,     4,     4,
       4,     1,     1,     1,     2,     2,     3,     1,     0,     3,
       2,     1,     2,     2,     3,     1,     2,     2,     1,     2,
       2,     3,     1,     2,     2,     1,     2,     3,     1,     2,
       3,     1,     3,     4,     1,     1,     1,     1,     0,     7,
       0,     8,     0,     8,     0,     8,     1,     0,     3,     3,
       3,     1,     1,     2,     1,     1,     1,     2,     1,     2,
       1,     2,     1,     2,     0,     2,     3,     4,     4,     3,
       2,     2,     3,     3,     2,     1,     0,     1,     4,     1,
       2,     2,     0,     1,     4,     1,     2,     3,     1,     2,
       0,     1,     2,     6,     0,     8,     7,     9,     0,    12,
      11,     1,     3,     3,     2,     2,     4,     5,     0,     2,
       0,     1,     1,     1,     5,     5,     5,     1,     5,     5,
       9,     1,     5,     0,     1,     1,     5,     1,     1,     5,
       5,     1,     3,     3,     4,     1,     1,     1,     1,     2,
       1,     3,     3,     2,     3,     1,     3,     1,     1,     1,
       1,     1,     2,     1,     1,     0,     2,     2,     1,     4,
       0,     1,     2,     3,     4,     2,     2,     1,     2,     2,
       5,     5,     7,     6,     1,     3,     0,     2,     0,     5,
       0,     5,     3,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     5,     6,     1,     1,
       3,     3,     2,     3,     3,     2,     4,     1,     4,     7,
      10,     1,     4,     2,     2,     1,     1,     5,     2,     5,
       0,     1,     3,     4,     0,     1,     0,     0,     1,     1,
       1,     2,     5,     0,     8,     0,     7,     0,     7,     0,
       8,     1,     2,     3,     0,     4,     3,     4,     4,     4,
       4,     5,     5,     5,     5,     6,     1,     1,     1,     3,
       0,     5,     0,     1,     1,     2,     6,     1,     3,     0,
       1,     4,     1,     1,     1,     1,     1,     1,     1,     3,
       2,     1,     2,     2,     2,     3,     4,     5,     2,     4,
       5,     4,     5,     3,     4,     8,     9,     3,     4,     2,
       1,     2,     6,     8,     9,     3,     4,     2,     3,     4,
       5,     4,     5,     4,     5,     3,     4,     1,     1,     1,
       4,     8,     9,     3,     4,     2,     3,     3,     4,     4,
       5,     4,     5,     3,     4,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     6,     8,     9,     3,     4,     2,     4,     1,
       2,     2,     2,     3,     4,     2,     4,     4,     3,     6,
       8,     3,     2,     4,     1,     2,     2,     1,     1,     2,
       3,     4,     2,     4,     6,     8,     1,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     5,     8,
       3,     2,     3,     7,     1,     5,     5,     6,     6,     1,
       3,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     5,     8,     3,     1,     2,     1,     2,     6,
       5,     6,     7,     7,     1,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     8,     3,     1,     1,
       2,     1,     1,     2,     3,     2,     3,     2,     3,     3,
       2,     4,     3,     2,     3,     2,     4,     3,     2,     6,
       6,     6,     7,     1,     2,     1,     1,     1,     2,     3,
       2,     3,     2,     3,     3,     4,     2,     3,     4,     2,
       5,     6,     7,     6,     6,     0,     1,     0,     2
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
#line 6707 "Parser/parser.cc"
    break;

  case 3:
#line 536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6713 "Parser/parser.cc"
    break;

  case 4:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6719 "Parser/parser.cc"
    break;

  case 5:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6725 "Parser/parser.cc"
    break;

  case 6:
#line 545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6731 "Parser/parser.cc"
    break;

  case 7:
#line 546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6737 "Parser/parser.cc"
    break;

  case 8:
#line 547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6743 "Parser/parser.cc"
    break;

  case 18:
#line 564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6749 "Parser/parser.cc"
    break;

  case 19:
#line 568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6755 "Parser/parser.cc"
    break;

  case 20:
#line 572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6761 "Parser/parser.cc"
    break;

  case 21:
#line 574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6771 "Parser/parser.cc"
    break;

  case 22:
#line 585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6777 "Parser/parser.cc"
    break;

  case 23:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6783 "Parser/parser.cc"
    break;

  case 24:
#line 591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6789 "Parser/parser.cc"
    break;

  case 26:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6795 "Parser/parser.cc"
    break;

  case 27:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6801 "Parser/parser.cc"
    break;

  case 28:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6807 "Parser/parser.cc"
    break;

  case 29:
#line 600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6813 "Parser/parser.cc"
    break;

  case 30:
#line 602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6823 "Parser/parser.cc"
    break;

  case 32:
#line 616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6834 "Parser/parser.cc"
    break;

  case 33:
#line 626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6843 "Parser/parser.cc"
    break;

  case 34:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6849 "Parser/parser.cc"
    break;

  case 36:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "New array subscript is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6855 "Parser/parser.cc"
    break;

  case 37:
#line 644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6861 "Parser/parser.cc"
    break;

  case 38:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6871 "Parser/parser.cc"
    break;

  case 39:
#line 652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6877 "Parser/parser.cc"
    break;

  case 40:
#line 654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6883 "Parser/parser.cc"
    break;

  case 41:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6889 "Parser/parser.cc"
    break;

  case 42:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6895 "Parser/parser.cc"
    break;

  case 43:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6901 "Parser/parser.cc"
    break;

  case 44:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6907 "Parser/parser.cc"
    break;

  case 45:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 6913 "Parser/parser.cc"
    break;

  case 46:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6919 "Parser/parser.cc"
    break;

  case 47:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 6925 "Parser/parser.cc"
    break;

  case 48:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6931 "Parser/parser.cc"
    break;

  case 49:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6937 "Parser/parser.cc"
    break;

  case 50:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6943 "Parser/parser.cc"
    break;

  case 51:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 6949 "Parser/parser.cc"
    break;

  case 52:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 6955 "Parser/parser.cc"
    break;

  case 53:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 6961 "Parser/parser.cc"
    break;

  case 54:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 6967 "Parser/parser.cc"
    break;

  case 55:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 6977 "Parser/parser.cc"
    break;

  case 56:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 6983 "Parser/parser.cc"
    break;

  case 58:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 6989 "Parser/parser.cc"
    break;

  case 59:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6995 "Parser/parser.cc"
    break;

  case 62:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7001 "Parser/parser.cc"
    break;

  case 64:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7007 "Parser/parser.cc"
    break;

  case 65:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7013 "Parser/parser.cc"
    break;

  case 66:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7019 "Parser/parser.cc"
    break;

  case 67:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7025 "Parser/parser.cc"
    break;

  case 68:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7031 "Parser/parser.cc"
    break;

  case 69:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7037 "Parser/parser.cc"
    break;

  case 70:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7043 "Parser/parser.cc"
    break;

  case 71:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7049 "Parser/parser.cc"
    break;

  case 72:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7057 "Parser/parser.cc"
    break;

  case 73:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7063 "Parser/parser.cc"
    break;

  case 74:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7072 "Parser/parser.cc"
    break;

  case 77:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7078 "Parser/parser.cc"
    break;

  case 78:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7084 "Parser/parser.cc"
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
#line 7104 "Parser/parser.cc"
    break;

  case 80:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7110 "Parser/parser.cc"
    break;

  case 81:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7116 "Parser/parser.cc"
    break;

  case 82:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7122 "Parser/parser.cc"
    break;

  case 83:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7128 "Parser/parser.cc"
    break;

  case 84:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7134 "Parser/parser.cc"
    break;

  case 85:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7140 "Parser/parser.cc"
    break;

  case 86:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7146 "Parser/parser.cc"
    break;

  case 87:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7152 "Parser/parser.cc"
    break;

  case 88:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7161 "Parser/parser.cc"
    break;

  case 89:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7167 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7173 "Parser/parser.cc"
    break;

  case 91:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7179 "Parser/parser.cc"
    break;

  case 92:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7185 "Parser/parser.cc"
    break;

  case 93:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7191 "Parser/parser.cc"
    break;

  case 94:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7197 "Parser/parser.cc"
    break;

  case 95:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7203 "Parser/parser.cc"
    break;

  case 97:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7209 "Parser/parser.cc"
    break;

  case 98:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7215 "Parser/parser.cc"
    break;

  case 99:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7221 "Parser/parser.cc"
    break;

  case 100:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7227 "Parser/parser.cc"
    break;

  case 101:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7233 "Parser/parser.cc"
    break;

  case 102:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7239 "Parser/parser.cc"
    break;

  case 103:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7245 "Parser/parser.cc"
    break;

  case 104:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7251 "Parser/parser.cc"
    break;

  case 112:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7257 "Parser/parser.cc"
    break;

  case 114:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7263 "Parser/parser.cc"
    break;

  case 115:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7269 "Parser/parser.cc"
    break;

  case 116:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7275 "Parser/parser.cc"
    break;

  case 118:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7281 "Parser/parser.cc"
    break;

  case 119:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7287 "Parser/parser.cc"
    break;

  case 121:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7293 "Parser/parser.cc"
    break;

  case 122:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7299 "Parser/parser.cc"
    break;

  case 124:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7305 "Parser/parser.cc"
    break;

  case 125:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7311 "Parser/parser.cc"
    break;

  case 126:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7317 "Parser/parser.cc"
    break;

  case 127:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7323 "Parser/parser.cc"
    break;

  case 129:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7329 "Parser/parser.cc"
    break;

  case 130:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7335 "Parser/parser.cc"
    break;

  case 132:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7341 "Parser/parser.cc"
    break;

  case 134:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7347 "Parser/parser.cc"
    break;

  case 136:
#line 914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7353 "Parser/parser.cc"
    break;

  case 138:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7359 "Parser/parser.cc"
    break;

  case 140:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7365 "Parser/parser.cc"
    break;

  case 142:
#line 932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7371 "Parser/parser.cc"
    break;

  case 143:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7377 "Parser/parser.cc"
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
#line 7389 "Parser/parser.cc"
    break;

  case 147:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7395 "Parser/parser.cc"
    break;

  case 148:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7401 "Parser/parser.cc"
    break;

  case 152:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7407 "Parser/parser.cc"
    break;

  case 153:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7413 "Parser/parser.cc"
    break;

  case 154:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7419 "Parser/parser.cc"
    break;

  case 155:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7425 "Parser/parser.cc"
    break;

  case 156:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7431 "Parser/parser.cc"
    break;

  case 157:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7437 "Parser/parser.cc"
    break;

  case 158:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7443 "Parser/parser.cc"
    break;

  case 159:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7449 "Parser/parser.cc"
    break;

  case 160:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7455 "Parser/parser.cc"
    break;

  case 161:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7461 "Parser/parser.cc"
    break;

  case 162:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7467 "Parser/parser.cc"
    break;

  case 163:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7473 "Parser/parser.cc"
    break;

  case 164:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7479 "Parser/parser.cc"
    break;

  case 165:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7485 "Parser/parser.cc"
    break;

  case 166:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7491 "Parser/parser.cc"
    break;

  case 168:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7497 "Parser/parser.cc"
    break;

  case 169:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7503 "Parser/parser.cc"
    break;

  case 170:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7509 "Parser/parser.cc"
    break;

  case 172:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7515 "Parser/parser.cc"
    break;

  case 173:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7521 "Parser/parser.cc"
    break;

  case 185:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7527 "Parser/parser.cc"
    break;

  case 187:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7533 "Parser/parser.cc"
    break;

  case 188:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7539 "Parser/parser.cc"
    break;

  case 189:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7545 "Parser/parser.cc"
    break;

  case 190:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7551 "Parser/parser.cc"
    break;

  case 192:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7557 "Parser/parser.cc"
    break;

  case 193:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7563 "Parser/parser.cc"
    break;

  case 194:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7569 "Parser/parser.cc"
    break;

  case 195:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7575 "Parser/parser.cc"
    break;

  case 196:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7581 "Parser/parser.cc"
    break;

  case 199:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7587 "Parser/parser.cc"
    break;

  case 200:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7593 "Parser/parser.cc"
    break;

  case 201:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7599 "Parser/parser.cc"
    break;

  case 202:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7605 "Parser/parser.cc"
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
#line 7619 "Parser/parser.cc"
    break;

  case 204:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7625 "Parser/parser.cc"
    break;

  case 205:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7634 "Parser/parser.cc"
    break;

  case 206:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7640 "Parser/parser.cc"
    break;

  case 207:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7646 "Parser/parser.cc"
    break;

  case 208:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7652 "Parser/parser.cc"
    break;

  case 209:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7658 "Parser/parser.cc"
    break;

  case 210:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7664 "Parser/parser.cc"
    break;

  case 211:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7670 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7676 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7682 "Parser/parser.cc"
    break;

  case 215:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7688 "Parser/parser.cc"
    break;

  case 216:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7694 "Parser/parser.cc"
    break;

  case 217:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7700 "Parser/parser.cc"
    break;

  case 218:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7706 "Parser/parser.cc"
    break;

  case 220:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7712 "Parser/parser.cc"
    break;

  case 221:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 222:
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7724 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7730 "Parser/parser.cc"
    break;

  case 225:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7736 "Parser/parser.cc"
    break;

  case 226:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7742 "Parser/parser.cc"
    break;

  case 227:
#line 1186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7748 "Parser/parser.cc"
    break;

  case 228:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7754 "Parser/parser.cc"
    break;

  case 229:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7760 "Parser/parser.cc"
    break;

  case 230:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7766 "Parser/parser.cc"
    break;

  case 231:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7772 "Parser/parser.cc"
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
#line 7791 "Parser/parser.cc"
    break;

  case 234:
#line 1222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7797 "Parser/parser.cc"
    break;

  case 235:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7803 "Parser/parser.cc"
    break;

  case 236:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7809 "Parser/parser.cc"
    break;

  case 237:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7816 "Parser/parser.cc"
    break;

  case 238:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7823 "Parser/parser.cc"
    break;

  case 239:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7829 "Parser/parser.cc"
    break;

  case 240:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7835 "Parser/parser.cc"
    break;

  case 241:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7841 "Parser/parser.cc"
    break;

  case 242:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7848 "Parser/parser.cc"
    break;

  case 243:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7855 "Parser/parser.cc"
    break;

  case 244:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7861 "Parser/parser.cc"
    break;

  case 245:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7867 "Parser/parser.cc"
    break;

  case 246:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7876 "Parser/parser.cc"
    break;

  case 247:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7882 "Parser/parser.cc"
    break;

  case 248:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7888 "Parser/parser.cc"
    break;

  case 249:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7894 "Parser/parser.cc"
    break;

  case 250:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 7900 "Parser/parser.cc"
    break;

  case 251:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 7906 "Parser/parser.cc"
    break;

  case 252:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 7912 "Parser/parser.cc"
    break;

  case 253:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 7918 "Parser/parser.cc"
    break;

  case 254:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 7924 "Parser/parser.cc"
    break;

  case 255:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 7930 "Parser/parser.cc"
    break;

  case 256:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 7936 "Parser/parser.cc"
    break;

  case 257:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 7942 "Parser/parser.cc"
    break;

  case 258:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 7948 "Parser/parser.cc"
    break;

  case 259:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 7954 "Parser/parser.cc"
    break;

  case 260:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 7960 "Parser/parser.cc"
    break;

  case 261:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 7966 "Parser/parser.cc"
    break;

  case 262:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 7972 "Parser/parser.cc"
    break;

  case 263:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 7978 "Parser/parser.cc"
    break;

  case 264:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 7984 "Parser/parser.cc"
    break;

  case 265:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 7990 "Parser/parser.cc"
    break;

  case 266:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7996 "Parser/parser.cc"
    break;

  case 267:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8002 "Parser/parser.cc"
    break;

  case 268:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8008 "Parser/parser.cc"
    break;

  case 269:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8014 "Parser/parser.cc"
    break;

  case 270:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8020 "Parser/parser.cc"
    break;

  case 271:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8026 "Parser/parser.cc"
    break;

  case 272:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8032 "Parser/parser.cc"
    break;

  case 273:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8038 "Parser/parser.cc"
    break;

  case 274:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8044 "Parser/parser.cc"
    break;

  case 275:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8050 "Parser/parser.cc"
    break;

  case 278:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 8058 "Parser/parser.cc"
    break;

  case 279:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Mutex statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8064 "Parser/parser.cc"
    break;

  case 280:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8070 "Parser/parser.cc"
    break;

  case 281:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8076 "Parser/parser.cc"
    break;

  case 283:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8082 "Parser/parser.cc"
    break;

  case 284:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8088 "Parser/parser.cc"
    break;

  case 286:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8094 "Parser/parser.cc"
    break;

  case 287:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8100 "Parser/parser.cc"
    break;

  case 288:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8106 "Parser/parser.cc"
    break;

  case 289:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8112 "Parser/parser.cc"
    break;

  case 290:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8118 "Parser/parser.cc"
    break;

  case 291:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8124 "Parser/parser.cc"
    break;

  case 292:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8130 "Parser/parser.cc"
    break;

  case 293:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8136 "Parser/parser.cc"
    break;

  case 294:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8142 "Parser/parser.cc"
    break;

  case 295:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8148 "Parser/parser.cc"
    break;

  case 296:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8154 "Parser/parser.cc"
    break;

  case 297:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8160 "Parser/parser.cc"
    break;

  case 298:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8166 "Parser/parser.cc"
    break;

  case 299:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 300:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8178 "Parser/parser.cc"
    break;

  case 301:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8184 "Parser/parser.cc"
    break;

  case 302:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8190 "Parser/parser.cc"
    break;

  case 303:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8196 "Parser/parser.cc"
    break;

  case 304:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8202 "Parser/parser.cc"
    break;

  case 305:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8208 "Parser/parser.cc"
    break;

  case 306:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8214 "Parser/parser.cc"
    break;

  case 307:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8220 "Parser/parser.cc"
    break;

  case 309:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8226 "Parser/parser.cc"
    break;

  case 310:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8232 "Parser/parser.cc"
    break;

  case 311:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8238 "Parser/parser.cc"
    break;

  case 316:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8244 "Parser/parser.cc"
    break;

  case 317:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8250 "Parser/parser.cc"
    break;

  case 318:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8256 "Parser/parser.cc"
    break;

  case 319:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8262 "Parser/parser.cc"
    break;

  case 320:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8268 "Parser/parser.cc"
    break;

  case 321:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8274 "Parser/parser.cc"
    break;

  case 322:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8280 "Parser/parser.cc"
    break;

  case 323:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8286 "Parser/parser.cc"
    break;

  case 326:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8292 "Parser/parser.cc"
    break;

  case 327:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8298 "Parser/parser.cc"
    break;

  case 328:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8304 "Parser/parser.cc"
    break;

  case 329:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8310 "Parser/parser.cc"
    break;

  case 330:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8316 "Parser/parser.cc"
    break;

  case 331:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8322 "Parser/parser.cc"
    break;

  case 332:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8331 "Parser/parser.cc"
    break;

  case 333:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8340 "Parser/parser.cc"
    break;

  case 334:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8346 "Parser/parser.cc"
    break;

  case 337:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8352 "Parser/parser.cc"
    break;

  case 338:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8358 "Parser/parser.cc"
    break;

  case 340:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8364 "Parser/parser.cc"
    break;

  case 341:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8370 "Parser/parser.cc"
    break;

  case 351:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8376 "Parser/parser.cc"
    break;

  case 352:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8382 "Parser/parser.cc"
    break;

  case 356:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8388 "Parser/parser.cc"
    break;

  case 358:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8394 "Parser/parser.cc"
    break;

  case 359:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8400 "Parser/parser.cc"
    break;

  case 360:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8406 "Parser/parser.cc"
    break;

  case 361:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8412 "Parser/parser.cc"
    break;

  case 362:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8418 "Parser/parser.cc"
    break;

  case 363:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8424 "Parser/parser.cc"
    break;

  case 365:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8430 "Parser/parser.cc"
    break;

  case 366:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8436 "Parser/parser.cc"
    break;

  case 367:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8442 "Parser/parser.cc"
    break;

  case 368:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8453 "Parser/parser.cc"
    break;

  case 369:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8459 "Parser/parser.cc"
    break;

  case 370:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8465 "Parser/parser.cc"
    break;

  case 371:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8471 "Parser/parser.cc"
    break;

  case 372:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8477 "Parser/parser.cc"
    break;

  case 373:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8486 "Parser/parser.cc"
    break;

  case 374:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8495 "Parser/parser.cc"
    break;

  case 375:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8504 "Parser/parser.cc"
    break;

  case 376:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8513 "Parser/parser.cc"
    break;

  case 377:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8522 "Parser/parser.cc"
    break;

  case 378:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8531 "Parser/parser.cc"
    break;

  case 379:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8540 "Parser/parser.cc"
    break;

  case 380:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8549 "Parser/parser.cc"
    break;

  case 381:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8557 "Parser/parser.cc"
    break;

  case 382:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8565 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8571 "Parser/parser.cc"
    break;

  case 387:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 388:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 401:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8589 "Parser/parser.cc"
    break;

  case 404:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 407:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8601 "Parser/parser.cc"
    break;

  case 408:
#line 1808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8607 "Parser/parser.cc"
    break;

  case 409:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8613 "Parser/parser.cc"
    break;

  case 410:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8619 "Parser/parser.cc"
    break;

  case 412:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8625 "Parser/parser.cc"
    break;

  case 414:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8631 "Parser/parser.cc"
    break;

  case 415:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8637 "Parser/parser.cc"
    break;

  case 417:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8643 "Parser/parser.cc"
    break;

  case 418:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8649 "Parser/parser.cc"
    break;

  case 419:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8655 "Parser/parser.cc"
    break;

  case 420:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8661 "Parser/parser.cc"
    break;

  case 421:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8667 "Parser/parser.cc"
    break;

  case 422:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8673 "Parser/parser.cc"
    break;

  case 423:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8679 "Parser/parser.cc"
    break;

  case 424:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8685 "Parser/parser.cc"
    break;

  case 425:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8691 "Parser/parser.cc"
    break;

  case 426:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8697 "Parser/parser.cc"
    break;

  case 427:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8703 "Parser/parser.cc"
    break;

  case 428:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8709 "Parser/parser.cc"
    break;

  case 429:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8715 "Parser/parser.cc"
    break;

  case 430:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8721 "Parser/parser.cc"
    break;

  case 431:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8727 "Parser/parser.cc"
    break;

  case 432:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8733 "Parser/parser.cc"
    break;

  case 433:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8739 "Parser/parser.cc"
    break;

  case 434:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8745 "Parser/parser.cc"
    break;

  case 435:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8751 "Parser/parser.cc"
    break;

  case 436:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8757 "Parser/parser.cc"
    break;

  case 437:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8763 "Parser/parser.cc"
    break;

  case 438:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8769 "Parser/parser.cc"
    break;

  case 439:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8775 "Parser/parser.cc"
    break;

  case 440:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8781 "Parser/parser.cc"
    break;

  case 441:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8787 "Parser/parser.cc"
    break;

  case 442:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8793 "Parser/parser.cc"
    break;

  case 443:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8799 "Parser/parser.cc"
    break;

  case 444:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8805 "Parser/parser.cc"
    break;

  case 445:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8811 "Parser/parser.cc"
    break;

  case 446:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8817 "Parser/parser.cc"
    break;

  case 447:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8823 "Parser/parser.cc"
    break;

  case 448:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8829 "Parser/parser.cc"
    break;

  case 449:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8835 "Parser/parser.cc"
    break;

  case 451:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8841 "Parser/parser.cc"
    break;

  case 453:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8847 "Parser/parser.cc"
    break;

  case 454:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8853 "Parser/parser.cc"
    break;

  case 455:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8859 "Parser/parser.cc"
    break;

  case 457:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8865 "Parser/parser.cc"
    break;

  case 458:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8871 "Parser/parser.cc"
    break;

  case 459:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8877 "Parser/parser.cc"
    break;

  case 460:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 8883 "Parser/parser.cc"
    break;

  case 462:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8889 "Parser/parser.cc"
    break;

  case 464:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8895 "Parser/parser.cc"
    break;

  case 465:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8901 "Parser/parser.cc"
    break;

  case 466:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 8907 "Parser/parser.cc"
    break;

  case 467:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 8913 "Parser/parser.cc"
    break;

  case 468:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 8919 "Parser/parser.cc"
    break;

  case 469:
#line 1966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 8925 "Parser/parser.cc"
    break;

  case 470:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 8931 "Parser/parser.cc"
    break;

  case 471:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 8937 "Parser/parser.cc"
    break;

  case 472:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 8943 "Parser/parser.cc"
    break;

  case 474:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8949 "Parser/parser.cc"
    break;

  case 475:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8955 "Parser/parser.cc"
    break;

  case 476:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8961 "Parser/parser.cc"
    break;

  case 478:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 8967 "Parser/parser.cc"
    break;

  case 479:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 8973 "Parser/parser.cc"
    break;

  case 480:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 8982 "Parser/parser.cc"
    break;

  case 482:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8988 "Parser/parser.cc"
    break;

  case 483:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 484:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 486:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 487:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 489:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 490:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9024 "Parser/parser.cc"
    break;

  case 491:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9030 "Parser/parser.cc"
    break;

  case 493:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 494:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 495:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 496:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 497:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9060 "Parser/parser.cc"
    break;

  case 499:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9066 "Parser/parser.cc"
    break;

  case 500:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9072 "Parser/parser.cc"
    break;

  case 501:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9078 "Parser/parser.cc"
    break;

  case 502:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9084 "Parser/parser.cc"
    break;

  case 503:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9090 "Parser/parser.cc"
    break;

  case 508:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9096 "Parser/parser.cc"
    break;

  case 509:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 510:
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9111 "Parser/parser.cc"
    break;

  case 511:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 512:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9126 "Parser/parser.cc"
    break;

  case 513:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9135 "Parser/parser.cc"
    break;

  case 514:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9144 "Parser/parser.cc"
    break;

  case 515:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9153 "Parser/parser.cc"
    break;

  case 517:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9159 "Parser/parser.cc"
    break;

  case 518:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9165 "Parser/parser.cc"
    break;

  case 519:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9175 "Parser/parser.cc"
    break;

  case 520:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9190 "Parser/parser.cc"
    break;

  case 523:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9196 "Parser/parser.cc"
    break;

  case 524:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9202 "Parser/parser.cc"
    break;

  case 525:
#line 2140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9208 "Parser/parser.cc"
    break;

  case 526:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9214 "Parser/parser.cc"
    break;

  case 527:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9220 "Parser/parser.cc"
    break;

  case 528:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9226 "Parser/parser.cc"
    break;

  case 529:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9232 "Parser/parser.cc"
    break;

  case 530:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9238 "Parser/parser.cc"
    break;

  case 531:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9244 "Parser/parser.cc"
    break;

  case 532:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9250 "Parser/parser.cc"
    break;

  case 533:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9256 "Parser/parser.cc"
    break;

  case 534:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9262 "Parser/parser.cc"
    break;

  case 535:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9268 "Parser/parser.cc"
    break;

  case 536:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9274 "Parser/parser.cc"
    break;

  case 537:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9280 "Parser/parser.cc"
    break;

  case 538:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9293 "Parser/parser.cc"
    break;

  case 539:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9299 "Parser/parser.cc"
    break;

  case 542:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9305 "Parser/parser.cc"
    break;

  case 543:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9311 "Parser/parser.cc"
    break;

  case 546:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9317 "Parser/parser.cc"
    break;

  case 548:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9323 "Parser/parser.cc"
    break;

  case 549:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9329 "Parser/parser.cc"
    break;

  case 550:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9335 "Parser/parser.cc"
    break;

  case 551:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9341 "Parser/parser.cc"
    break;

  case 552:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9347 "Parser/parser.cc"
    break;

  case 554:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9353 "Parser/parser.cc"
    break;

  case 556:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9359 "Parser/parser.cc"
    break;

  case 557:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9365 "Parser/parser.cc"
    break;

  case 559:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9371 "Parser/parser.cc"
    break;

  case 560:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9377 "Parser/parser.cc"
    break;

  case 562:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9383 "Parser/parser.cc"
    break;

  case 563:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 564:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9395 "Parser/parser.cc"
    break;

  case 565:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 566:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9407 "Parser/parser.cc"
    break;

  case 567:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9416 "Parser/parser.cc"
    break;

  case 568:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9425 "Parser/parser.cc"
    break;

  case 569:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9433 "Parser/parser.cc"
    break;

  case 570:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9443 "Parser/parser.cc"
    break;

  case 572:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9449 "Parser/parser.cc"
    break;

  case 573:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9455 "Parser/parser.cc"
    break;

  case 574:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9461 "Parser/parser.cc"
    break;

  case 575:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9467 "Parser/parser.cc"
    break;

  case 576:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9473 "Parser/parser.cc"
    break;

  case 577:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9479 "Parser/parser.cc"
    break;

  case 578:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9485 "Parser/parser.cc"
    break;

  case 579:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9491 "Parser/parser.cc"
    break;

  case 580:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9497 "Parser/parser.cc"
    break;

  case 581:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9503 "Parser/parser.cc"
    break;

  case 584:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 585:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9515 "Parser/parser.cc"
    break;

  case 586:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9521 "Parser/parser.cc"
    break;

  case 588:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9527 "Parser/parser.cc"
    break;

  case 589:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9533 "Parser/parser.cc"
    break;

  case 590:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9539 "Parser/parser.cc"
    break;

  case 592:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9545 "Parser/parser.cc"
    break;

  case 593:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9551 "Parser/parser.cc"
    break;

  case 594:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9557 "Parser/parser.cc"
    break;

  case 596:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9563 "Parser/parser.cc"
    break;

  case 599:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9569 "Parser/parser.cc"
    break;

  case 600:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9575 "Parser/parser.cc"
    break;

  case 602:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9581 "Parser/parser.cc"
    break;

  case 603:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9587 "Parser/parser.cc"
    break;

  case 604:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9593 "Parser/parser.cc"
    break;

  case 609:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9599 "Parser/parser.cc"
    break;

  case 611:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9605 "Parser/parser.cc"
    break;

  case 612:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9611 "Parser/parser.cc"
    break;

  case 613:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9617 "Parser/parser.cc"
    break;

  case 614:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9623 "Parser/parser.cc"
    break;

  case 615:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9629 "Parser/parser.cc"
    break;

  case 616:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9635 "Parser/parser.cc"
    break;

  case 622:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9641 "Parser/parser.cc"
    break;

  case 625:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9647 "Parser/parser.cc"
    break;

  case 626:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9653 "Parser/parser.cc"
    break;

  case 627:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9659 "Parser/parser.cc"
    break;

  case 628:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9665 "Parser/parser.cc"
    break;

  case 629:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9671 "Parser/parser.cc"
    break;

  case 630:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9677 "Parser/parser.cc"
    break;

  case 632:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9683 "Parser/parser.cc"
    break;

  case 633:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9689 "Parser/parser.cc"
    break;

  case 634:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9695 "Parser/parser.cc"
    break;

  case 636:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9701 "Parser/parser.cc"
    break;

  case 638:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9707 "Parser/parser.cc"
    break;

  case 639:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9713 "Parser/parser.cc"
    break;

  case 640:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9719 "Parser/parser.cc"
    break;

  case 641:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9725 "Parser/parser.cc"
    break;

  case 642:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9731 "Parser/parser.cc"
    break;

  case 643:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9737 "Parser/parser.cc"
    break;

  case 645:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9743 "Parser/parser.cc"
    break;

  case 646:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9749 "Parser/parser.cc"
    break;

  case 647:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9755 "Parser/parser.cc"
    break;

  case 648:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9766 "Parser/parser.cc"
    break;

  case 649:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9772 "Parser/parser.cc"
    break;

  case 650:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9778 "Parser/parser.cc"
    break;

  case 651:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9784 "Parser/parser.cc"
    break;

  case 652:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9793 "Parser/parser.cc"
    break;

  case 653:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9799 "Parser/parser.cc"
    break;

  case 654:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9805 "Parser/parser.cc"
    break;

  case 655:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9811 "Parser/parser.cc"
    break;

  case 656:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9817 "Parser/parser.cc"
    break;

  case 657:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9823 "Parser/parser.cc"
    break;

  case 658:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9829 "Parser/parser.cc"
    break;

  case 659:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9835 "Parser/parser.cc"
    break;

  case 660:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9841 "Parser/parser.cc"
    break;

  case 661:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9847 "Parser/parser.cc"
    break;

  case 662:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9853 "Parser/parser.cc"
    break;

  case 665:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 666:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 667:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9871 "Parser/parser.cc"
    break;

  case 668:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 9877 "Parser/parser.cc"
    break;

  case 670:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 9883 "Parser/parser.cc"
    break;

  case 671:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9889 "Parser/parser.cc"
    break;

  case 672:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9895 "Parser/parser.cc"
    break;

  case 673:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9901 "Parser/parser.cc"
    break;

  case 674:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 9907 "Parser/parser.cc"
    break;

  case 675:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 676:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 9919 "Parser/parser.cc"
    break;

  case 677:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 9928 "Parser/parser.cc"
    break;

  case 678:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 9937 "Parser/parser.cc"
    break;

  case 679:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 9943 "Parser/parser.cc"
    break;

  case 680:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 9949 "Parser/parser.cc"
    break;

  case 682:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 9955 "Parser/parser.cc"
    break;

  case 687:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 688:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9967 "Parser/parser.cc"
    break;

  case 689:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 9973 "Parser/parser.cc"
    break;

  case 691:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 9979 "Parser/parser.cc"
    break;

  case 692:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9985 "Parser/parser.cc"
    break;

  case 693:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 9991 "Parser/parser.cc"
    break;

  case 694:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9997 "Parser/parser.cc"
    break;

  case 696:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10003 "Parser/parser.cc"
    break;

  case 697:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10009 "Parser/parser.cc"
    break;

  case 698:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10015 "Parser/parser.cc"
    break;

  case 701:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10024 "Parser/parser.cc"
    break;

  case 702:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10030 "Parser/parser.cc"
    break;

  case 703:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10039 "Parser/parser.cc"
    break;

  case 704:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10049 "Parser/parser.cc"
    break;

  case 705:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10058 "Parser/parser.cc"
    break;

  case 706:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10068 "Parser/parser.cc"
    break;

  case 707:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10077 "Parser/parser.cc"
    break;

  case 708:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10087 "Parser/parser.cc"
    break;

  case 709:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10096 "Parser/parser.cc"
    break;

  case 710:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10106 "Parser/parser.cc"
    break;

  case 712:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 713:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 714:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10124 "Parser/parser.cc"
    break;

  case 715:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10130 "Parser/parser.cc"
    break;

  case 716:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10141 "Parser/parser.cc"
    break;

  case 717:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10150 "Parser/parser.cc"
    break;

  case 718:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10159 "Parser/parser.cc"
    break;

  case 719:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10165 "Parser/parser.cc"
    break;

  case 720:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 721:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 722:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10186 "Parser/parser.cc"
    break;

  case 723:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10192 "Parser/parser.cc"
    break;

  case 724:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10198 "Parser/parser.cc"
    break;

  case 725:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10204 "Parser/parser.cc"
    break;

  case 729:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 730:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10216 "Parser/parser.cc"
    break;

  case 731:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10226 "Parser/parser.cc"
    break;

  case 732:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10232 "Parser/parser.cc"
    break;

  case 735:
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10238 "Parser/parser.cc"
    break;

  case 736:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10244 "Parser/parser.cc"
    break;

  case 738:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10250 "Parser/parser.cc"
    break;

  case 739:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10256 "Parser/parser.cc"
    break;

  case 740:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10262 "Parser/parser.cc"
    break;

  case 741:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10268 "Parser/parser.cc"
    break;

  case 746:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10274 "Parser/parser.cc"
    break;

  case 747:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10280 "Parser/parser.cc"
    break;

  case 748:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10286 "Parser/parser.cc"
    break;

  case 749:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10292 "Parser/parser.cc"
    break;

  case 750:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 752:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 753:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 754:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10316 "Parser/parser.cc"
    break;

  case 755:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10322 "Parser/parser.cc"
    break;

  case 756:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10328 "Parser/parser.cc"
    break;

  case 757:
#line 2935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10334 "Parser/parser.cc"
    break;

  case 758:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10340 "Parser/parser.cc"
    break;

  case 759:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10346 "Parser/parser.cc"
    break;

  case 760:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10352 "Parser/parser.cc"
    break;

  case 761:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10358 "Parser/parser.cc"
    break;

  case 762:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10364 "Parser/parser.cc"
    break;

  case 763:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10370 "Parser/parser.cc"
    break;

  case 764:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10376 "Parser/parser.cc"
    break;

  case 765:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10382 "Parser/parser.cc"
    break;

  case 766:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10388 "Parser/parser.cc"
    break;

  case 767:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10394 "Parser/parser.cc"
    break;

  case 768:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10400 "Parser/parser.cc"
    break;

  case 769:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10406 "Parser/parser.cc"
    break;

  case 771:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10412 "Parser/parser.cc"
    break;

  case 772:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10418 "Parser/parser.cc"
    break;

  case 773:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10424 "Parser/parser.cc"
    break;

  case 774:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10430 "Parser/parser.cc"
    break;

  case 775:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10436 "Parser/parser.cc"
    break;

  case 776:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10442 "Parser/parser.cc"
    break;

  case 777:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10448 "Parser/parser.cc"
    break;

  case 778:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10454 "Parser/parser.cc"
    break;

  case 779:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10460 "Parser/parser.cc"
    break;

  case 780:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10466 "Parser/parser.cc"
    break;

  case 781:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10472 "Parser/parser.cc"
    break;

  case 782:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10478 "Parser/parser.cc"
    break;

  case 783:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10484 "Parser/parser.cc"
    break;

  case 784:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10490 "Parser/parser.cc"
    break;

  case 785:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10496 "Parser/parser.cc"
    break;

  case 786:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10502 "Parser/parser.cc"
    break;

  case 790:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10508 "Parser/parser.cc"
    break;

  case 791:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10514 "Parser/parser.cc"
    break;

  case 792:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10520 "Parser/parser.cc"
    break;

  case 793:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10526 "Parser/parser.cc"
    break;

  case 794:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10532 "Parser/parser.cc"
    break;

  case 795:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10538 "Parser/parser.cc"
    break;

  case 796:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10544 "Parser/parser.cc"
    break;

  case 797:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10550 "Parser/parser.cc"
    break;

  case 798:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10556 "Parser/parser.cc"
    break;

  case 799:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10562 "Parser/parser.cc"
    break;

  case 800:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10568 "Parser/parser.cc"
    break;

  case 801:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10574 "Parser/parser.cc"
    break;

  case 802:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10580 "Parser/parser.cc"
    break;

  case 803:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10586 "Parser/parser.cc"
    break;

  case 804:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10592 "Parser/parser.cc"
    break;

  case 805:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10601 "Parser/parser.cc"
    break;

  case 806:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10607 "Parser/parser.cc"
    break;

  case 807:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10613 "Parser/parser.cc"
    break;

  case 809:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 810:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 811:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 812:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 813:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 814:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 815:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10655 "Parser/parser.cc"
    break;

  case 816:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10661 "Parser/parser.cc"
    break;

  case 817:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10667 "Parser/parser.cc"
    break;

  case 818:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10673 "Parser/parser.cc"
    break;

  case 819:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10679 "Parser/parser.cc"
    break;

  case 820:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10685 "Parser/parser.cc"
    break;

  case 821:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10691 "Parser/parser.cc"
    break;

  case 822:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10697 "Parser/parser.cc"
    break;

  case 823:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10703 "Parser/parser.cc"
    break;

  case 824:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10709 "Parser/parser.cc"
    break;

  case 825:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10715 "Parser/parser.cc"
    break;

  case 826:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 827:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10727 "Parser/parser.cc"
    break;

  case 828:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10733 "Parser/parser.cc"
    break;

  case 830:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10739 "Parser/parser.cc"
    break;

  case 831:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10745 "Parser/parser.cc"
    break;

  case 832:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 833:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 834:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10763 "Parser/parser.cc"
    break;

  case 835:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10769 "Parser/parser.cc"
    break;

  case 836:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10775 "Parser/parser.cc"
    break;

  case 837:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10781 "Parser/parser.cc"
    break;

  case 838:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10787 "Parser/parser.cc"
    break;

  case 839:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10793 "Parser/parser.cc"
    break;

  case 840:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 841:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10805 "Parser/parser.cc"
    break;

  case 842:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10811 "Parser/parser.cc"
    break;

  case 843:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10817 "Parser/parser.cc"
    break;

  case 845:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10823 "Parser/parser.cc"
    break;

  case 846:
#line 3204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 847:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10835 "Parser/parser.cc"
    break;

  case 848:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 849:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10847 "Parser/parser.cc"
    break;

  case 850:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 851:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 852:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 853:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 854:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10877 "Parser/parser.cc"
    break;

  case 855:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 857:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 858:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 859:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 10901 "Parser/parser.cc"
    break;

  case 860:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 10907 "Parser/parser.cc"
    break;

  case 861:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10913 "Parser/parser.cc"
    break;

  case 862:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10919 "Parser/parser.cc"
    break;

  case 863:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10925 "Parser/parser.cc"
    break;

  case 865:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10931 "Parser/parser.cc"
    break;

  case 866:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 867:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10943 "Parser/parser.cc"
    break;

  case 868:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 10949 "Parser/parser.cc"
    break;

  case 869:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 870:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10961 "Parser/parser.cc"
    break;

  case 871:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 10967 "Parser/parser.cc"
    break;

  case 872:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 10973 "Parser/parser.cc"
    break;

  case 873:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 10979 "Parser/parser.cc"
    break;

  case 875:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 10985 "Parser/parser.cc"
    break;

  case 876:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 10991 "Parser/parser.cc"
    break;

  case 877:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 10997 "Parser/parser.cc"
    break;

  case 878:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11003 "Parser/parser.cc"
    break;

  case 880:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11009 "Parser/parser.cc"
    break;

  case 881:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11015 "Parser/parser.cc"
    break;

  case 882:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11021 "Parser/parser.cc"
    break;

  case 883:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11027 "Parser/parser.cc"
    break;

  case 884:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11033 "Parser/parser.cc"
    break;

  case 885:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11039 "Parser/parser.cc"
    break;

  case 886:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11045 "Parser/parser.cc"
    break;

  case 887:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11051 "Parser/parser.cc"
    break;

  case 889:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11057 "Parser/parser.cc"
    break;

  case 890:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11063 "Parser/parser.cc"
    break;

  case 891:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11069 "Parser/parser.cc"
    break;

  case 892:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11075 "Parser/parser.cc"
    break;

  case 893:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11081 "Parser/parser.cc"
    break;

  case 894:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11087 "Parser/parser.cc"
    break;

  case 896:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11093 "Parser/parser.cc"
    break;

  case 898:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11099 "Parser/parser.cc"
    break;

  case 899:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11105 "Parser/parser.cc"
    break;

  case 900:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11111 "Parser/parser.cc"
    break;

  case 901:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11117 "Parser/parser.cc"
    break;

  case 902:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11123 "Parser/parser.cc"
    break;

  case 903:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11129 "Parser/parser.cc"
    break;

  case 905:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11135 "Parser/parser.cc"
    break;

  case 906:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11141 "Parser/parser.cc"
    break;

  case 907:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11147 "Parser/parser.cc"
    break;

  case 908:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11153 "Parser/parser.cc"
    break;

  case 909:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11159 "Parser/parser.cc"
    break;

  case 910:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11165 "Parser/parser.cc"
    break;

  case 911:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11171 "Parser/parser.cc"
    break;

  case 913:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11177 "Parser/parser.cc"
    break;

  case 914:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11183 "Parser/parser.cc"
    break;

  case 915:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11189 "Parser/parser.cc"
    break;

  case 916:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11195 "Parser/parser.cc"
    break;

  case 917:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11201 "Parser/parser.cc"
    break;

  case 920:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11207 "Parser/parser.cc"
    break;

  case 923:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 924:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11219 "Parser/parser.cc"
    break;

  case 925:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 926:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11231 "Parser/parser.cc"
    break;

  case 927:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11237 "Parser/parser.cc"
    break;

  case 928:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11243 "Parser/parser.cc"
    break;

  case 929:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11249 "Parser/parser.cc"
    break;

  case 930:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11255 "Parser/parser.cc"
    break;

  case 931:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11261 "Parser/parser.cc"
    break;

  case 932:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11267 "Parser/parser.cc"
    break;

  case 933:
#line 3503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11273 "Parser/parser.cc"
    break;

  case 934:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11279 "Parser/parser.cc"
    break;

  case 935:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11285 "Parser/parser.cc"
    break;

  case 936:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11291 "Parser/parser.cc"
    break;

  case 937:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11297 "Parser/parser.cc"
    break;

  case 938:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11303 "Parser/parser.cc"
    break;

  case 939:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11309 "Parser/parser.cc"
    break;

  case 940:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11315 "Parser/parser.cc"
    break;

  case 941:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11321 "Parser/parser.cc"
    break;

  case 942:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11327 "Parser/parser.cc"
    break;

  case 944:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11333 "Parser/parser.cc"
    break;

  case 948:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11339 "Parser/parser.cc"
    break;

  case 949:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11345 "Parser/parser.cc"
    break;

  case 950:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11351 "Parser/parser.cc"
    break;

  case 951:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11357 "Parser/parser.cc"
    break;

  case 952:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11363 "Parser/parser.cc"
    break;

  case 953:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11369 "Parser/parser.cc"
    break;

  case 954:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11375 "Parser/parser.cc"
    break;

  case 955:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11381 "Parser/parser.cc"
    break;

  case 956:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11387 "Parser/parser.cc"
    break;

  case 957:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11393 "Parser/parser.cc"
    break;

  case 958:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11399 "Parser/parser.cc"
    break;

  case 959:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11405 "Parser/parser.cc"
    break;

  case 960:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11411 "Parser/parser.cc"
    break;

  case 961:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11417 "Parser/parser.cc"
    break;

  case 962:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11423 "Parser/parser.cc"
    break;

  case 963:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11429 "Parser/parser.cc"
    break;

  case 964:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11435 "Parser/parser.cc"
    break;

  case 967:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11441 "Parser/parser.cc"
    break;

  case 968:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11447 "Parser/parser.cc"
    break;


#line 11451 "Parser/parser.cc"

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
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
