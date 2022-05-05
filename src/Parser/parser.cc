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

#line 259 "Parser/parser.cc"

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
#line 230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 627 "Parser/parser.cc"

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
#define YYLAST   20133

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  994
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2017

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
     549,   550,   551,   552,   553,   554,   558,   559,   563,   564,
     569,   573,   574,   585,   587,   589,   593,   594,   596,   598,
     600,   602,   612,   620,   629,   630,   640,   645,   650,   651,
     656,   662,   664,   666,   672,   674,   676,   678,   680,   682,
     684,   686,   688,   690,   692,   694,   696,   698,   700,   702,
     704,   714,   715,   719,   720,   725,   728,   732,   733,   737,
     738,   740,   742,   744,   746,   748,   753,   755,   757,   765,
     766,   774,   777,   778,   780,   785,   801,   803,   805,   807,
     809,   811,   813,   815,   817,   825,   826,   828,   832,   833,
     834,   835,   839,   840,   842,   844,   846,   848,   850,   852,
     854,   861,   862,   863,   864,   868,   869,   873,   874,   879,
     880,   882,   884,   889,   890,   892,   897,   898,   900,   905,
     906,   908,   910,   912,   917,   918,   920,   925,   926,   931,
     932,   937,   938,   943,   944,   949,   950,   955,   956,   959,
     964,   969,   970,   978,   984,   985,   989,   990,   994,   995,
     999,  1000,  1001,  1002,  1003,  1004,  1005,  1006,  1007,  1008,
    1009,  1019,  1021,  1026,  1027,  1029,  1031,  1036,  1037,  1043,
    1044,  1050,  1051,  1052,  1053,  1054,  1055,  1056,  1057,  1058,
    1059,  1060,  1062,  1063,  1069,  1071,  1081,  1083,  1091,  1092,
    1097,  1099,  1101,  1103,  1105,  1109,  1110,  1112,  1117,  1119,
    1126,  1128,  1130,  1140,  1142,  1144,  1149,  1154,  1157,  1162,
    1164,  1166,  1168,  1176,  1177,  1179,  1183,  1185,  1189,  1191,
    1192,  1194,  1196,  1201,  1202,  1206,  1211,  1212,  1216,  1218,
    1223,  1225,  1230,  1232,  1234,  1236,  1241,  1243,  1245,  1247,
    1252,  1254,  1259,  1260,  1282,  1284,  1286,  1289,  1292,  1295,
    1297,  1299,  1301,  1304,  1307,  1309,  1312,  1319,  1321,  1323,
    1325,  1327,  1332,  1334,  1336,  1338,  1343,  1345,  1350,  1352,
    1354,  1356,  1359,  1363,  1366,  1370,  1372,  1374,  1376,  1378,
    1380,  1382,  1384,  1386,  1388,  1390,  1395,  1396,  1400,  1406,
    1411,  1416,  1417,  1421,  1425,  1430,  1431,  1437,  1441,  1443,
    1445,  1447,  1450,  1452,  1457,  1459,  1464,  1466,  1468,  1473,
    1475,  1481,  1482,  1486,  1487,  1488,  1489,  1493,  1498,  1499,
    1501,  1503,  1505,  1509,  1513,  1514,  1518,  1520,  1522,  1524,
    1526,  1532,  1533,  1539,  1540,  1544,  1545,  1550,  1552,  1558,
    1559,  1561,  1566,  1571,  1582,  1583,  1587,  1588,  1594,  1595,
    1599,  1601,  1605,  1607,  1611,  1612,  1616,  1617,  1621,  1622,
    1623,  1627,  1629,  1644,  1645,  1646,  1647,  1649,  1653,  1655,
    1659,  1666,  1668,  1670,  1675,  1676,  1678,  1680,  1682,  1714,
    1717,  1722,  1724,  1730,  1735,  1740,  1751,  1756,  1761,  1766,
    1771,  1780,  1784,  1791,  1793,  1794,  1795,  1801,  1803,  1808,
    1809,  1810,  1819,  1820,  1821,  1825,  1826,  1827,  1836,  1837,
    1838,  1843,  1844,  1853,  1854,  1859,  1860,  1864,  1866,  1868,
    1870,  1872,  1876,  1881,  1882,  1884,  1894,  1895,  1900,  1902,
    1904,  1906,  1908,  1911,  1913,  1915,  1920,  1922,  1924,  1926,
    1928,  1930,  1932,  1934,  1936,  1938,  1940,  1942,  1944,  1946,
    1948,  1950,  1952,  1954,  1956,  1958,  1960,  1962,  1964,  1966,
    1968,  1970,  1972,  1974,  1979,  1980,  1984,  1991,  1992,  1998,
    1999,  2001,  2003,  2005,  2010,  2012,  2017,  2018,  2020,  2022,
    2027,  2029,  2031,  2033,  2035,  2037,  2042,  2043,  2045,  2047,
    2052,  2054,  2053,  2057,  2065,  2066,  2068,  2070,  2075,  2076,
    2078,  2083,  2084,  2086,  2088,  2093,  2094,  2096,  2101,  2103,
    2105,  2107,  2108,  2110,  2115,  2117,  2119,  2124,  2125,  2129,
    2130,  2135,  2134,  2139,  2138,  2146,  2145,  2156,  2155,  2165,
    2170,  2171,  2176,  2182,  2196,  2197,  2201,  2203,  2205,  2211,
    2213,  2215,  2217,  2219,  2221,  2223,  2225,  2231,  2232,  2237,
    2239,  2241,  2250,  2252,  2253,  2254,  2256,  2258,  2259,  2264,
    2265,  2266,  2271,  2273,  2276,  2283,  2284,  2285,  2291,  2296,
    2298,  2304,  2305,  2311,  2312,  2316,  2321,  2324,  2323,  2327,
    2330,  2338,  2337,  2346,  2352,  2356,  2358,  2363,  2365,  2367,
    2369,  2375,  2378,  2384,  2385,  2387,  2388,  2389,  2391,  2393,
    2400,  2401,  2403,  2405,  2410,  2411,  2417,  2418,  2420,  2421,
    2426,  2427,  2428,  2430,  2438,  2439,  2441,  2444,  2446,  2450,
    2451,  2452,  2454,  2456,  2461,  2463,  2468,  2470,  2479,  2481,
    2486,  2487,  2488,  2492,  2493,  2494,  2499,  2500,  2505,  2506,
    2507,  2508,  2512,  2513,  2518,  2519,  2520,  2521,  2522,  2536,
    2537,  2542,  2543,  2549,  2551,  2554,  2556,  2558,  2581,  2582,
    2588,  2589,  2595,  2594,  2604,  2603,  2607,  2613,  2619,  2620,
    2622,  2626,  2631,  2633,  2635,  2637,  2643,  2644,  2648,  2649,
    2654,  2656,  2663,  2665,  2666,  2668,  2673,  2675,  2677,  2682,
    2684,  2689,  2694,  2702,  2704,  2709,  2710,  2715,  2716,  2720,
    2721,  2722,  2727,  2729,  2735,  2737,  2742,  2744,  2750,  2751,
    2755,  2759,  2763,  2765,  2766,  2767,  2772,  2775,  2774,  2786,
    2785,  2797,  2796,  2808,  2807,  2819,  2818,  2832,  2838,  2840,
    2846,  2847,  2852,  2859,  2864,  2870,  2873,  2876,  2880,  2886,
    2889,  2892,  2897,  2898,  2899,  2903,  2909,  2910,  2920,  2921,
    2925,  2926,  2931,  2936,  2937,  2943,  2944,  2946,  2951,  2952,
    2953,  2954,  2955,  2957,  2992,  2994,  2999,  3001,  3002,  3004,
    3009,  3011,  3013,  3015,  3020,  3022,  3024,  3026,  3028,  3030,
    3032,  3037,  3039,  3041,  3043,  3052,  3054,  3055,  3060,  3062,
    3064,  3066,  3068,  3073,  3075,  3077,  3079,  3084,  3086,  3088,
    3090,  3092,  3094,  3106,  3107,  3108,  3112,  3114,  3116,  3118,
    3120,  3125,  3127,  3129,  3131,  3136,  3138,  3140,  3142,  3144,
    3146,  3161,  3166,  3171,  3173,  3174,  3176,  3181,  3183,  3185,
    3187,  3192,  3194,  3196,  3198,  3200,  3202,  3204,  3209,  3211,
    3213,  3215,  3217,  3227,  3229,  3231,  3232,  3234,  3239,  3241,
    3243,  3248,  3250,  3252,  3254,  3259,  3261,  3263,  3277,  3279,
    3281,  3282,  3284,  3289,  3291,  3296,  3298,  3300,  3305,  3307,
    3312,  3314,  3331,  3332,  3334,  3339,  3341,  3343,  3345,  3347,
    3352,  3353,  3355,  3357,  3362,  3364,  3366,  3372,  3374,  3376,
    3379,  3383,  3385,  3387,  3389,  3423,  3424,  3426,  3428,  3433,
    3435,  3437,  3439,  3441,  3446,  3447,  3449,  3451,  3456,  3458,
    3460,  3466,  3467,  3469,  3478,  3481,  3483,  3486,  3488,  3490,
    3504,  3505,  3507,  3512,  3514,  3516,  3518,  3520,  3525,  3526,
    3528,  3530,  3535,  3537,  3545,  3546,  3547,  3552,  3553,  3558,
    3560,  3562,  3564,  3566,  3568,  3575,  3577,  3579,  3581,  3583,
    3586,  3588,  3590,  3592,  3594,  3599,  3601,  3603,  3608,  3634,
    3635,  3637,  3641,  3642,  3646,  3648,  3650,  3652,  3654,  3656,
    3663,  3665,  3667,  3669,  3671,  3673,  3678,  3680,  3682,  3689,
    3691,  3709,  3711,  3716,  3717
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

#define YYPACT_NINF (-1669)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-875)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     177, 12109,   208,   220, 16229,   108, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669, -1669,   117,   936,   123,
   -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669, -1669,    96,   288, -1669,
   -1669, -1669, -1669, -1669, -1669,  4308,  4308,   223, 12109,   251,
     272, -1669, -1669,   286, -1669, -1669, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669,  2833, -1669,   385,   203, -1669, -1669, -1669,
   -1669, -1669, 16079, -1669, -1669,   195,   229,   311,     8, -1669,
    4308,   229,   229,   229,   281,  3962,   513,   935, 12268, -1669,
   -1669, -1669, 15929,  1031, -1669, -1669, -1669,  2515,   519, 12320,
     858,   868,  2515,   917,   386, -1669, -1669, -1669, -1669,   494,
   -1669, -1669, -1669, -1669,   449, -1669, -1669, -1669, -1669, -1669,
     477,   416,   494, -1669,   494,   503, -1669, -1669, -1669, 16785,
    4308, -1669, -1669,  4308, -1669, 12109,   461, 16837, -1669, -1669,
    4020, 17849, -1669,   956,   956,   515,  2578, -1669, -1669, -1669,
   -1669,   366, 14539,  2860,   494, -1669, -1669, -1669, -1669, -1669,
   -1669,   561, -1669,   527,   581,   592, -1669,   640, 19499, 15159,
    3170,  2833,   678,   611,   623,   636,   655,   665,   673, -1669,
   -1669, 16987, 11138,   705, -1669, 16372, -1669, -1669, -1669, -1669,
     710, -1669, -1669,   681, -1669, 18635,   840, 18851, -1669,   729,
    4308,   416,   739,   715,   740,   747, -1669, -1669, -1669,  3412,
    3038,   752,   804,   280, -1669, -1669,   494,   494,   144,   151,
     422,   144, -1669,   494,   494, -1669,  3051, -1669, -1669,   760,
     766,   956,  8308, -1669, -1669, 16079, -1669, -1669,  2515, -1669,
    2333,   386,   777,   850,   151,  4308,   311, -1669, 13515, -1669,
     956,   956,   851,   850,   151,  4308, -1669, 20013, -1669, -1669,
     956, -1669,   956, -1669,   847,  3565,  4308, -1669,  1173,   779,
   -1669, -1669, -1669, 16531,   416,   183, -1669, -1669, 17899, -1669,
     804,   204, -1669, 19499, 17849,  3617,  3051, -1669,   428, -1669,
   -1669, -1669, 16837,  4308, -1669,   853, -1669, -1669, -1669, -1669,
    4308,  2711,   584,   598, -1669,  4308,   527, -1669,   824,   494,
     891, 17039,   745, 14697, 14117,  2515,  2515, -1669,  2515,   956,
    2515,   956, -1669, -1669,   494, -1669,   810, -1669, 17189, -1669,
   -1669, -1669, 17241,   710, -1669,   862,   -49,  1255,   895,   386,
     900, -1669,  2578,   898,   527,  2578,  2517, -1669,   918,   966,
   19571,   938,   941, 19499, 19643,   952, 14905, -1669, -1669, -1669,
   -1669, -1669, -1669, 19715, 19715, 15005,   953,  4096, -1669, -1669,
   -1669, -1669,   511, -1669,   534, -1669,  1105, -1669, 19499, 19499,
   -1669,   944,   612,   865,   975,   436,  1045,   957,   982,   987,
    1005,    81, -1669,   721, -1669,  1029, -1669,  1001,  3667, 15467,
   -1669, -1669,   924,  1029, -1669, -1669,   722, -1669, -1669,  3170,
    1002,  1035,  1038,  1049,  1054,  1060, -1669, -1669,   437,  1095,
   -1669,   738,  1095, -1669, -1669, 16785, -1669,  1101,  1085, 15621,
   -1669, -1669,  4548,  3744,  1130, 14697,  1140,   517,   819, -1669,
   -1669, -1669, -1669, -1669,  4308,  4629, -1669, -1669, -1669, -1669,
   -1669, -1669,  6570,  4404,   953, 18635,  1121,  1139, -1669, -1669,
    1133, 18851,   699, -1669, -1669, -1669, 18923,  1153, -1669, -1669,
   -1669, -1669, -1669,  3412,   646,  1161,  1165,  1178,   807,  1182,
    1227,  1229,  3038, -1669, -1669,   494,  1155,   311,  1166, -1669,
   -1669,  1174, -1669, -1669,   416,   850, -1669, -1669, -1669,   416,
   -1669, -1669,  3051, -1669, 15467, 15467, -1669,   956,  4020, 18627,
   14539, -1669, -1669, -1669, -1669, -1669,   416,   850,   204, -1669,
   -1669,  2515,  1192,   850,   151, -1669,   416,   850, -1669, 20063,
   -1669,   956,   956, -1669, -1669,  1225,   111,  1235,   386,  1260,
   -1669, 18057, -1669,   761, -1669,  1329, 18523, -1669,  4020, 17400,
    8308, -1669, 16531, 19787, -1669, -1669, -1669, -1669, -1669,  3617,
     852,  3051, -1669, 14539,   804, 12109, -1669,  1257, -1669,  1274,
   -1669, -1669, -1669, -1669, -1669,  2578, -1669, -1669,  1351,  4132,
   17241, 11138, -1669, 17452, -1669,   956,   956, -1669, -1669,   710,
   -1669,   951,  1283,  1431, 19499,  2012,  1174,  1288, -1669,   494,
     494, -1669,  1095, -1669, 17039, -1669, -1669, 18338,   956,   956,
   -1669,  4132,   494, -1669, 17706, -1669, -1669, 17189, -1669,   366,
    1309,   235,  1314,  1255,   770, 16837,   771, -1669, -1669, -1669,
   -1669, -1669, -1669,   791, -1669,  1325,  1313, -1669, 15313, -1669,
   17504, 17504, -1669, 15313, -1669, 19499, -1669, 12320, 12320, 15313,
   -1669, -1669, 16583, 17504, 17504,  1001,  1356,  1373,   627,  1465,
   -1669,   802,  1332,  1132,  1339, -1669, 18923, 19499, 18995,  1335,
   19499,  1173, 19499,  1173, -1669,  2198, -1669, -1669, 19067,  2776,
   19499, 19067,  1173, -1669, -1669, 19499, 19499, 19499, 19499, 19499,
   19499, 19499, 19499, 19499, 19499, 19499, 19499, 19499, 19499, 19499,
   19499, 19499, 19499, 19499, 19139,  1328,   640,  4462, 11138, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
    1350, 19499, -1669, -1669,   924,  2174, -1669, -1669,   494,   494,
   -1669, -1669, 15467, -1669,   465,  1095, -1669,   787,  1095, -1669,
   -1669, -1669,  1174, -1669, -1669,  1174, 19859, -1669, -1669, 11138,
    1364,  1368,  3500,  1519,  2653,   488,  1288, -1669,   494,   494,
    1288,   553, -1669,   494,   494, 19499,  4308,  1111,  1146,  1288,
      11, 14065, 14065,  4308, -1669, -1669, 19499,  1133, -1669, 18635,
    1389, -1669,  2444, -1669, -1669, -1669, -1669, -1669,   834, -1669,
   14065,  1173,  4020,  1173,   880,  1390,  1391,  1392,   897,  1395,
    1396,  1397,   573,  1095, -1669, -1669,   585,  1095, -1669, -1669,
   -1669,  4020,   640, -1669,  1095, 19859, -1669,   416, 18057, -1669,
   -1669,   856,  1399,   896,  1400, -1669,  1405, -1669,   416, -1669,
   -1669,   416,   850,  1405, -1669,   416,  1398,  1401,  1402, -1669,
   -1669, 18338, -1669,  1406, -1669, -1669, -1669,  1173,  4308, 10297,
    1477,  1386, 18425, -1669,  1085, -1669, 14065,   930, -1669, -1669,
    1405, -1669, 16837, 15467,  1403, -1669,  1403, -1669, -1669, -1669,
   -1669, 17189, -1669, 11300, 15775, -1669, 18057,  1413,  1414,  1416,
   -1669,  8754,   494, -1669,  2012, -1669, -1669, -1669, -1669,  1174,
   -1669, -1669, -1669,   956, -1669,  3426, -1669, -1669,   386,  1784,
    1422, -1669, 18851, -1669,  1255,  1309, -1669, -1669,  1415,  1420,
    2517, 19067, -1669,  1423,   203,  1418,  1424,  1428,  1425,  1430,
   19499,  1433,  1434,  1436, 11138, 19499, -1669, -1669,  1516, -1669,
   -1669, -1669, 19499, -1669,  1437,  1438, 18707,  1160, -1669, 19067,
    1440, -1669,  1441, -1669, -1669,  4531, -1669, -1669,   937, -1669,
   -1669, -1669, -1669,  4531, -1669, -1669,  1169,   258, -1669, -1669,
     944,   944,   944,   612,   612,   865,   865,   975,   975,   975,
     975,   436,   436,  1045,   957,   982,   987,  1005, 19499,  1176,
   -1669,  1439,  4531, -1669, -1669, 18635, -1669, 18057,  1445,  1446,
    1447,  2174, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
    1174, -1669, -1669,  1174, 18057, 18057, -1669, -1669,  3500,   869,
    1448,  1451,  1452,  1454,  3308,  2653, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
    1453, -1669,  1288, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
   -1669,  1458,  1460, -1669,   311,  4531,  1181,   157, -1669, -1669,
    1443, -1669, 18851, -1669, 19499, -1669, 19211, 14065, -1669, -1669,
   -1669,  1409,   587,  1095, -1669,   601,  1095, -1669, -1669, -1669,
   -1669,  1174, -1669, -1669, -1669,  1174,   804,  1469,  1174, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669,  1444, -1669, -1669,  1405,
   -1669,   416, -1669, -1669, -1669, -1669, -1669, 12898,  1470,  1467,
   -1669,    50, -1669,   418,   380, 10976,  1475, 13894,  1478,  1479,
    2018,  2185,  2339, 19283,  1483, -1669, -1669,  1484,  1486, -1669,
   -1669,   416, 19499, 19499,  1623,  1482,   473, -1669,  1567,  1487,
    1471, -1669, -1669, -1669, 10125, -1669, -1669, -1669, -1669, -1669,
     954, -1669, -1669, -1669,  1552, -1669, -1669, -1669,  1173, -1669,
   -1669, 12745, 16079,  1493, -1669,  4308, -1669,  1476,  1498,  1499,
   -1669,  1184, -1669, -1669, -1669, -1669,  4020, -1669, -1669,  1480,
    1481,   940, 16837,   527,   527, -1669, -1669,   953,  1085, 15621,
   -1669,  1029, -1669, 11462, -1669,   662,  1095, -1669,   956,  9588,
   -1669, -1669,  1255,   494,   494,   366,   235, -1669, -1669,  1309,
    1508,  1511, -1669, -1669,   984,   481, 11138,  1173, -1669,   481,
   16635,   481, -1669, 19499, 19499, 19499, -1669, -1669, -1669, -1669,
   19499, 19499,  1504, 18635, -1669, -1669,  1509,   580, -1669, -1669,
   -1669,  3107, -1669, -1669,  1193, -1669,    75, -1669, 19067,  1199,
   -1669, 18923, -1669, -1669, 19499,  1489,  1240,  1246,  1133, -1669,
     672,  1095, -1669, -1669, 18057, 18057, -1669, -1669,  1514,   677,
    1095, -1669,   690,  2550,   494,   494, -1669, -1669, 18057, 18057,
   -1669,  1513, -1669, 14539, 14539,  1517,  1520,  1522,  1521, -1669,
    1523, 19499, 19499,  1249,  1525, -1669, -1669, -1669, -1669, -1669,
   -1669,  1532, 19499, -1669, -1669, -1669,  1174, -1669, -1669, -1669,
    1174, 18057, 18057,   311,   494,  1254,  1534,  1538, -1669, -1669,
    1539, 13051, 13204, 13357, 16837, 17504, 17504,  1540, -1669,  1497,
    1518,  1086,  5923, -1669,   219,  4308, -1669, -1669,  4308, -1669,
   18779,   439,   485, -1669, -1669, -1669, -1669, 19499,  1541,  1613,
   10813, 10469, -1669,  1524, -1669,  1527, 19499,  1528, 18635,  1529,
   19499, 18923, 19499,  1198, -1669,  1530,   102, -1669,    24,  1544,
   -1669, -1669,  1546, -1669,  1531, -1669,  1533,  1547, 13894,   687,
   13673,   494,   291, -1669, -1669, -1669,  1548, -1669,  1549, -1669,
    1560, -1669,  1554, -1669,  1559, -1669, -1669, -1669, -1669, 11624,
    1562,  1563,  1564, -1669,  1569, -1669, -1669, -1669,  1174, 19499,
   19499,  1085,  1568, -1669,  1309, -1669,  1577,   393, -1669,  1573,
   -1669, -1669, 16837, -1669,  1574,  1570,   985, -1669,  1580, -1669,
   -1669, -1669, -1669, -1669, 18635,  1133, 18923, -1669,  1620,  4531,
   -1669,  1620,  1620, -1669,  4531,  4153,  4287, -1669, -1669,  1259,
   -1669, -1669, -1669,  1592,  1594, -1669, -1669, -1669,  1174, -1669,
   -1669,  1595,  1596,   494, -1669, -1669, -1669,  1174, -1669, -1669,
   -1669,  1597, -1669, -1669, -1669, -1669, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669,  1600, -1669, -1669, -1669, -1669,
    1602,  1598,   494, -1669, 18057, 18057, -1669, -1669, -1669, -1669,
   19499, -1669, -1669,  1614, -1669,  1540,  1540,  1540,   964,  1576,
     368, -1669,  3834,   373, 15467, -1669, -1669, -1669,  3686, 19499,
    3238,   389, -1669, -1669,    84,  1608,  1608,  4308, -1669, -1669,
   18206, -1669, 19499,  1611,  1621, -1669, -1669, -1669, -1669,   996,
    1624, 13894,  1487,  1627, 19499,   195,  1628,   281,  4451, 16837,
   13894, 19499, 19499,   800,   556, -1669, 19499, -1669, -1669,   425,
   -1669,  1133, -1669,   999,  1014,  1016, -1669, -1669, -1669, -1669,
     416,  1198,  1626, -1669, -1669, 19499, -1669,  1630,   640, 10976,
   -1669, -1669, -1669, -1669, 19499,  1675, -1669,  9951, -1669,   494,
   14539, -1669, -1669, 16837, -1669, -1669, -1669, -1669, -1669,  1632,
   -1669, 18057, -1669, -1669,  1635, -1669,  1637,  1636,  1639,  1255,
   -1669, -1669, -1669, -1669, 19499, -1669, 16635, 19499,  1133,  1644,
    1275, -1669,  1277, -1669,  4531, -1669,  4531, -1669, -1669, -1669,
   -1669, 18057,  1643,  1646, -1669, -1669, 18057, 18057,  1647,  1649,
    1280, 14223, 14381, -1669,  1648, -1669, -1669, -1669, -1669,  1650,
    1652,  1285, -1669, -1669, -1669, -1669,   964,  2303,   480, -1669,
   -1669, -1669, -1669,   494,   494, -1669, -1669, -1669,   549, -1669,
    1028,  3686,   696, -1669,  3238,   494, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669,   563, 13894,    56, 19355,  1732, 13894,
    1487, 14855,  1733,  1487,  1634, -1669, -1669, -1669, -1669,  6383,
   19499, 13894, 10641,  1640, -1669,  1661,   434, 13894, -1669, -1669,
    1662, -1669, -1669,  1641,   640,   614,  1659,  1665,  1291,  1725,
   -1669, -1669, -1669, -1669,  4308,  4020, -1669, -1669,  1664,  1666,
   -1669, -1669, -1669,  1255,  1309,  1671, -1669, -1669, -1669,  1674,
   -1669, -1669, -1669,  1296,  1299, -1669, -1669, -1669, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669,  1672, -1669, -1669,  1678,  1679,
   -1669, -1669, -1669,  1681,  1683,  1684,  2303, -1669,   494, -1669,
   -1669, -1669, -1669, -1669,  1685,  3834, -1669, -1669,  6140,   106,
   11789, -1669, 13776, -1669,   -13,  1051, 13894,  1752, 13894, 19499,
    1687, 19499,   931,  1667,   424,  1767, -1669, 19499,  1668, 11950,
   -1669, -1669, -1669, 17654, -1669,  1688,  1673,   167, 13894, -1669,
   19499, 19067,   414, -1669, -1669, -1669,  1694, -1669, -1669,  1309,
    1698, -1669, -1669, -1669, -1669,  1699,  1704,  1705, 14539,  1702,
   -1669, -1669,   697,  1095, -1669, -1669,   964, -1669, -1669,   237,
   -1669,   205, -1669, -1669, -1669,  1710, 12427, -1669, -1669, 13894,
   -1669,    51, -1669, 13894, -1669, -1669,  1487,  1712,  1714, 19499,
   19499, 19499, 13894, -1669, -1669,  1717, 12427, 17654, -1669,  3864,
   17452,  1173,  1711, -1669,  1759,  1720,   654,  1719, -1669,  1793,
   -1669,  1052, 13894,  1729, 13894, 13894, -1669,  1734, -1669, -1669,
   -1669, -1669, -1669, -1669, -1669, -1669,  1174, -1669, 19499, -1669,
   19499, -1669, -1669,  1394, 12586, -1669, -1669, 13894, -1669, -1669,
    1715,  1722,   591, -1669,  1487, -1669, -1669,  1394, -1669,  1723,
    3337,  2847, -1669, -1669, -1669,   167,  1744, 19499,  1726,   167,
     167, 13894, -1669, -1669, 19499,  1795,  1797, -1669, 18057, -1669,
   -1669, 13776, -1669,  1394, -1669, -1669, 19499, 19427, 19499, -1669,
    1723, 19499,  1754,  2847,  1756,   640,  1765, -1669,   658, -1669,
   -1669,  1057,  1725,    48, -1669, -1669,  9363,  1762, 13776,  1487,
   -1669,  1487,  1487,  1775,  1773, -1669,   416,   640,  1776, -1669,
    1755,   640, -1669, -1669, 13894,  1855,  1781, -1669, -1669, -1669,
    9802, -1669,   416, -1669, -1669,  1340, 19499, -1669,  1063, -1669,
   13894, -1669, -1669,   640,  1173,  1782,  1764, -1669, -1669, -1669,
    1082, -1669, -1669,  1768,  1173, -1669, -1669
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
       0,   657,     0,   651,     0,   741,     0,   421,     3,   395,
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
       3,     0,     0,     0,     0,   653,   658,     3,   176,   175,
       3,     0,     0,     2,   783,   786,   788,     2,   840,   843,
     845,   421,   421,   648,   758,     0,     0,     0,   721,   751,
       0,   421,   421,   421,   421,   421,   421,   540,   568,     3,
       3,   569,   501,   558,     0,     0,   798,     2,     0,   364,
      61,     0,     0,   287,   288,   201,   203,     0,     0,     0,
     421,   421,   283,     0,   281,     0,     0,     0,   654,     0,
       0,     0,     0,     0,   155,     0,     0,   342,     0,     0,
       3,   208,     0,   199,     0,   278,     0,     0,     2,     0,
     501,   758,     0,   361,   904,   903,     0,     2,     0,   712,
       2,   707,     0,   708,     0,   690,   671,   675,   673,   421,
       0,     0,     0,     3,     0,     2,   913,   915,   916,     0,
       0,    95,     0,     3,   991,   591,     0,   601,   599,     0,
     589,   703,   421,   767,     0,     0,     0,    34,     0,   107,
     109,   108,   105,   104,   654,   991,     0,    60,    76,     0,
      70,    77,    78,    55,     0,     0,     0,    64,    51,     0,
     148,   371,    30,     0,     0,     2,   889,   891,   892,     3,
       3,     0,     0,   758,     2,   860,   862,   863,     2,   877,
     879,     0,   854,   869,     3,     3,   988,     3,   612,   611,
     615,   990,     2,     2,   989,     0,     3,   755,   664,   665,
       0,     0,   758,   402,   421,   421,     3,     3,   408,   757,
       0,   848,   732,     0,   734,   540,   540,   540,   575,   545,
       0,   581,   569,     0,   421,   532,   567,   563,     0,     0,
       0,     0,   570,   572,   758,   583,   583,     0,   564,   579,
     421,   367,     0,     0,    62,   291,   292,   289,   290,     0,
       0,     2,   219,     0,     0,   221,   375,   220,   501,   421,
       2,     0,   179,   257,     0,   252,   179,   284,   282,     0,
     276,   991,   285,     0,     0,     0,   323,   324,   325,   326,
       0,   316,     0,   317,   293,     0,   294,     0,     0,   421,
     210,   197,   280,   279,     0,   314,   333,     0,   365,   758,
     421,   730,   691,   421,     2,     2,   965,   966,   967,     0,
     918,   421,     3,     3,     0,   926,     0,     0,     0,     0,
     600,   588,     3,    93,     0,    31,   421,     0,   991,     0,
       0,    80,     0,    68,     0,    74,     0,    72,    39,   153,
     894,   421,     0,     0,   799,   817,   421,   421,     0,     0,
       0,   421,   421,   667,     0,   388,   390,     3,     3,     0,
       0,     0,   736,   536,   538,   534,     0,   933,     0,   576,
     938,   578,   930,   758,   758,   562,   582,   566,     0,   565,
       0,     0,     0,   585,     0,   758,   559,   573,   584,   574,
     580,   619,   623,   622,     0,     2,     0,     0,   240,     2,
     222,   501,   248,   258,     0,   273,   274,   275,   272,   261,
       0,     2,   421,     0,   277,     0,     0,     2,   300,   327,
       0,   318,     2,     0,     0,     0,     0,   305,     0,   301,
     195,   194,   389,   706,     0,     0,   968,     3,     0,     0,
     925,   927,   590,     0,   991,     2,    37,    35,    36,     0,
      58,   172,    71,     0,     0,     3,   800,   818,     3,     3,
     865,   880,   392,     2,   609,     3,   608,   666,     0,     0,
     791,   849,   899,     0,     0,     0,   934,   935,   758,   561,
     931,   932,   560,   541,     0,     0,   209,   299,     0,     0,
       0,   233,     2,   211,     0,     0,     2,   242,     2,   179,
     266,     0,   262,     0,   259,   250,   253,   179,     0,     0,
     214,   298,     2,   421,   295,     0,     0,   343,     2,   303,
       0,    61,     0,   315,   711,   713,     0,   928,   929,   991,
       0,   704,    59,    75,    73,     0,     0,     0,   421,     0,
     792,   850,   758,   941,   943,   936,     0,   571,   228,   223,
     226,     0,   225,   232,   231,     0,   421,   235,   234,     2,
     244,     0,   241,     2,   249,   254,   263,   274,   272,     0,
     179,     0,     2,   256,   286,     0,   421,   421,     3,   328,
     422,   332,     0,   336,     0,     0,     0,   344,   345,   217,
     306,     0,     2,     0,     2,     2,   919,     0,   593,   895,
     866,   881,   613,     2,   937,   939,   940,   577,     0,   230,
       0,   229,   213,   236,   421,   356,   245,     2,   246,   243,
     268,   267,   264,   255,   260,   251,   216,   236,     3,   321,
       0,   933,   329,   330,   331,   343,     0,     0,     0,   343,
       0,     2,   304,   311,     0,   308,   310,   592,   421,   224,
     227,     2,     3,   237,   357,   247,     0,     0,     0,     3,
     321,     0,     0,   934,     0,     0,     0,   337,     0,   346,
     218,     0,   301,     0,     3,   205,     0,     0,     2,   270,
     271,   269,   265,     0,     0,   322,     0,   349,     0,   347,
       0,   349,   307,   309,     2,     0,     0,   207,   206,   212,
       0,   215,     0,   319,   350,     0,     0,   338,     0,   312,
       2,   942,   320,     0,     0,     0,     0,   313,   351,   352,
       0,   348,   339,     0,     0,   340,   353
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1669,  5905,  6187, -1669,    -1,   493,   963,  -160, -1669,  1566,
   -1669,   320, -1669,  -682,   599,   693,  -927,  -769, -1669,   256,
    3001,  1913, -1669,  1033, -1669,  1276,   523,   806,   808,   639,
     805,  1236,  1238,  1239,  1234,  1244, -1669,  -147,  -159,  8434,
     816, -1669,  -383, -1669, -1669,  -653,  3362, -1096,  1043, -1669,
     159, -1669,   809,   -17, -1669, -1669, -1669,   381,    62, -1669,
   -1483, -1238,   260,    47, -1669, -1669, -1669,   268,   180, -1669,
   -1669, -1669, -1669,    12, -1645,   164, -1669, -1669,    15, -1669,
   -1669, -1669,    28,   411,   413,   118, -1669, -1669, -1669, -1669,
    -691, -1669,    58,     9, -1669,   125, -1669,     7, -1669, -1669,
   -1669,   828,  -700,  -729, -1283, -1669,    67, -1152,    69,  1922,
    -712,  -676, -1669,  -282, -1669,    44,  -149,   134,  -299,  -220,
    3817,  7082,  -593, -1669,    33,   408,   571,  1388, -1669,  1946,
   -1669,    65,  4002,  -298, -1669, -1669,    55, -1669, -1669,   785,
     211,  4276,  3030,   -31,  1746,  -230, -1669, -1669, -1669, -1669,
   -1669,  -262,  5201,  5237, -1669,  -359,   147, -1669,   486,   222,
   -1669,   166,   680, -1669,   483,    -5, -1669, -1669, -1669,  5359,
    -595, -1059,  -663,  -415,  -489,  1075, -1669, -1231,  -154,   231,
    1106,   849,  7615,  -327,  -453,  -241,  -187,  -435,  1221, -1669,
    1542,   429,  1141,  1450, -1669, -1669, -1669, -1669,   341,  -168,
      17,  -847, -1669,   448, -1669, -1669,   594,   426, -1669, -1669,
   -1669,  2021,  -687,  -443,  -839,   -23, -1669, -1669, -1669, -1669,
   -1669, -1669,   206,  -714,  -132, -1668,  -175,  7285,   -66,  6543,
   -1669,  1102, -1669,   -45,  -209,  -205,  -178,  -177,     1,   -69,
     -67,   -61,   301,   -33,    36,    68,  -144,   -44,  -108,   -94,
     -93,  -678,  -733,  -677,  -592,  -666,  -139,  -590, -1669, -1669,
    -698,  1289,  1297,  1298,  1799,  7420,  -542,  -569,  -555,  -532,
    -707, -1669, -1417, -1607, -1577, -1576,  -604,   -74,  -208, -1669,
   -1669,    -4,   184,   -62, -1669,  7917,    86,  -597,  -265
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1138,   213,   382,   383,    80,    81,   384,   359,   385,
    1426,  1427,   386,   958,   959,   960,  1244,  1245,  1246,  1438,
     408,   388,   389,   390,   668,   669,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1057,   670,
    1365,   731,   207,   733,   404,   798,  1139,  1140,  1141,  1142,
    1143,  1144,  1145,  1966,  1146,  1147,  1370,  1543,  1840,  1841,
    1781,  1782,  1783,  1942,  1943,  1148,  1554,  1555,  1700,  1149,
    1150,  1151,  1152,  1153,  1154,  1378,  1718,  1885,  1813,  1155,
    1156,  1571,  1952,  1572,  1573,  1868,  1157,  1158,  1159,  1368,
    1876,  1877,  1878,  1995,  2010,  1903,  1904,   284,   285,   859,
     860,  1111,    83,    84,    85,    86,    87,    88,   441,    90,
      91,    92,    93,    94,   221,   558,   443,   412,   444,    97,
     294,    99,   100,   101,   324,   325,   104,   105,   166,   106,
     878,   326,   152,   109,   241,   110,   153,   250,   328,   329,
     330,   154,   405,   115,   116,   332,   117,   549,   848,   846,
     847,  1515,   333,   334,   120,   121,  1107,  1333,  1521,  1522,
    1658,  1659,  1334,  1510,  1677,  1523,   122,   632,  1608,   335,
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
     611,   612,  1661,  1662,  1663,  1664,   597,   454,   340,   341,
     342,   417,   199,   141,   142,   143,   344,   790,   613
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   557,   131,    79,   183,   354,   184,   181,   336,   967,
     901,   493,   185,   322,   485,   529,   673,   358,   297,   403,
     475,   516,   592,   947,   350,   732,  1181,  1359,   182,  1020,
     787,   402,   887,   910,   102,   176,  1249,  1002,   915,   623,
     186,   486,   487,   626,   190,    95,   888,   497,   674,  1763,
     223,   231,  1478,  1479,    79,    79,   111,    79,  1030,   131,
    1849,   832,   834,   895,  1037,  1256,   107,  1545,   658,   889,
      89,   148,    79,   149,  1812,   488,   940,   289,   198,  1764,
    1765,    79,   836,   196,   868,  1021,  1026,   493,   339,    79,
     485,   102,   843,   615,    79,   209,   228,    79,  1027,   253,
     179,    79,    95,   263,   513,   298,  1577,  1843,  -874,   187,
    1842,   489,   420,   111,   421,   564,   566,   486,   487,  1053,
     422,  1984,   292,   107,  1907,   490,   491,    89,  1290,   436,
     625,  1778,  1779,   870,   628,    96,  1128,  1068,   150,    79,
     592,   188,    79,   198,    79,  1164,   131,  1418,   423,    79,
     183,   488,   184,   483,    57,    79,  1102,   260,   185,  1160,
     565,  1850,    79,   210,   658,  -740,    57,   246,   201,   256,
    1022,   257,  1023,  1578,   484,   223,   494,  -714,   102,    79,
      79,   196,   917,  1173,   604,   140,   186,   489,   140,    95,
    1060,   298,    96,   887,    79,  1445,   457,  1575,  1546,  1546,
     111,   490,   491,  1170,   466,  1338,  1899,   888,   144,    79,
     107,   713,   112,  1780,    89,   615,   194,   424,    79,    79,
    -715,   196,   895,   155,  1339,  1908,   183,  1446,   184,   569,
     889,   209,  1842,   201,   185,    79,   505,   498,  -358,  1214,
    1767,   522,   140,   544,    79,   162,   196,   598,   912,   425,
     570,   298,   494,   714,    79,   187,   823,    79,  1317,   533,
     179,   527,  1844,  1320,    79,   418,   156,  1237,  -537,   112,
    -359,   537,   161,   287,    79,    79,  1576,    79,   805,    96,
     863,   194,   357,   819,   791,  1020,   140,   188,  1291,  1006,
     280,   880,  1052,  1052,    79,    79,  1545,   196,   522,  1848,
     496,  -740,    79,  1263,   771,   806,   807,    19,  -358,    79,
      79,  1052,  1030,  1763,    79,   900,   560,  1812,  1210,  1209,
    1874,   923,  1292,   925,   926,   434,   927,   530,   906,   140,
     965,   523,   929,   587,  1388,   931,   932,   933,   111,   808,
    -359,  1021,   896,  1764,  1765,  1228,    79,  1276,   107,  1835,
      57,    79,  1201,   565,    79,   642,   112,   191,  1610,  1277,
    1900,  1901,   819,   615,   805,   598,  1051,  1051,  1898,   202,
     478,   587,   175,   758,  1527,   809,   518,  1052,  1328,   521,
     830,   204,   882,   197,    -3,  1051,   835,   615,   523,   810,
     811,   806,   807,  1528,   615,  1329,   229,  1099,   765,   254,
     177,  1346,  1292,   264,  1300,   198,  1252,   600,   911,   103,
    1478,  1479,  1318,  1248,   420,  1345,   421,    96,    79,   907,
     457,   178,   422,   453,   322,   808,  1022,  1546,  1268,   278,
    1164,  1330,   190,   498,   504,   179,   521,   509,   804,  1341,
    1342,    79,    79,   561,  1160,   868,  1338,   223,   820,   887,
     423,  1051,  1923,    79,    79,   216,  1694,   204,  1848,   526,
    1703,   809,    79,   888,   466,  1588,   103,   298,   205,   536,
      62,    63,  1440,   298,   157,   810,   811,   158,   159,  1503,
     160,   197,    79,  1201,   206,  1848,   889,  1882,  1407,   189,
      63,    79,    57,   457,   112,   201,   458,   146,    57,   339,
     420,  1031,   421,  1377,  1767,  1034,   248,    57,   422,  1778,
    1779,    79,  1622,   298,  1047,  1048,   236,    79,    75,   424,
    1883,   197,  -415,  1666,   867,   600,   298,   820,  1527,    13,
      14,    15,    16,    17,   672,    57,  1835,   279,   274,   204,
     598,   842,  1667,   194,  1675,   274,   197,  1669,   170,   170,
    1546,   425,   869,   103,  1343,    79,   991,    79,    57,   534,
    1258,   703,   704,  1676,    57,  1470,   911,  1340,    79,   757,
      79,   506,   457,   279,    79,   498,   131,   572,  1052,   942,
     942,   498,  1185,   170,    79,   901,   749,    57,    79,  1861,
     498,  1799,  1328,  1328,  1328,   560,   279,  1892,  1449,  1704,
     418,   418,  1061,  1793,   276,   705,   706,  1414,   102,  1329,
    1329,  1329,  1419,  1535,  1007,   260,  1041,   111,   498,    95,
      79,   615,   179,    57,   884,   771,   278,   107,   402,  -642,
     111,  1232,    79,   170,   293,  1768,   170,  1028,  1233,  1056,
     107,   602,   279,    57,    89,  1330,  1330,  1330,  -419,   170,
    1544,  1556,  1051,   615,  1769,    57,   348,    57,  1533,  1537,
    -358,  1453,  1087,   826,   680,  1941,   531,  1670,   829,   681,
    1070,    57,  -729,  1186,   266,  1623,  1625,  1627,   267,  1941,
      79,   270,    79,   272,    79,   837,   544,   682,    79,  1086,
     150,    79,   683,  1855,  1546,   844,    96,  1743,   311,  1744,
     902,  1863,  1035,   170,  1675,  1968,   602,  1701,  1564,    96,
     352,   827,  1702,   538,  1546,   580,    79,  1019,   942,   765,
     458,   418,  1078,  1772,   550,  1208,   498,   868,  1437,  1396,
     355,   248,    57,   838,  1082,  1248,  1303,  1776,   498,   841,
     498,   356,    57,   845,   581,   582,   942,    57,   170,   584,
    1307,   453,  1546,   585,   498,   357,  1948,   298,   170,   140,
      57,    79,   427,    79,  1913,  1806,  1204,    57,   547,   170,
    1807,   552,   696,   112,   428,    79,   298,  1282,   935,   697,
     698,   634,    79,  1620,   636,   322,   112,   429,   466,   936,
     937,    79,   884,   458,  1477,   278,   170,   426,  1435,   498,
      79,    79,    79,   170,   170,  1928,   430,   672,   170,  1980,
    1929,  1405,   672,  1289,  1981,   602,   431,  1607,   672,   266,
      79,  1455,   453,   912,   432,   498,  1464,   278,   418,   426,
     498,    13,    14,    15,    16,    17,   461,   672,  1619,  1468,
     170,  1253,   857,   602,   279,   170,  1893,   426,   170,   498,
     498,   792,   793,   248,   456,   794,    79,    79,   466,   460,
     339,   474,   243,     6,     7,     8,     9,    10,    11,    12,
     480,  -416,   715,   741,    72,  1296,   716,   742,   476,  1544,
      13,    14,    15,    16,    17,  1275,   771,   753,   479,    57,
     900,   498,  1162,  1428,   601,   481,   266,   267,   602,   619,
     496,   272,   482,    95,    79,    77,   603,   495,    79,   514,
     868,  1313,   856,    79,   111,   515,   857,  1056,   604,   642,
    -417,   916,   918,  1561,   107,   585,   585,   554,    89,    13,
      14,    15,    16,    17,   525,   170,  1011,   209,    57,  1174,
     498,  1175,   919,  1695,  1696,  1697,   920,   170,   170,   453,
    -420,   543,    63,   941,    79,   942,   506,   942,   815,   418,
     498,   622,    79,   615,  1705,  1698,  1509,   248,    13,    14,
      15,    16,    17,  1019,  1699,   588,   274,   447,  1409,  1274,
     765,   237,   238,   103,   239,  1065,  1090,    57,   240,  1066,
     453,    79,   157,    96,   466,   158,   159,  1098,   160,  1618,
    1100,   572,  1556,   426,  1103,   498,  1176,  1092,   535,  -406,
     576,   942,   453,   453,  1734,   354,   354,    79,  1028,  -873,
     426,  1739,   602,    79,    79,   531,    57,   699,   700,   278,
    1374,   453,  -406,   498,   912,   243,     6,     7,     8,     9,
      10,    11,    12,   140,  1389,   590,   506,  1094,  1101,   146,
     498,   942,  -587,    72,    79,    61,   140,   633,   168,   169,
      64,    65,    66,    67,    68,    69,    70,   322,  1547,   646,
     112,   635,  1498,   734,  1695,  1857,  1697,   498,   170,   572,
      72,   647,   879,   498,    77,    78,   942,   650,  1247,   236,
     651,  1395,  1248,    72,   265,   742,  1858,   453,   701,   702,
     601,   655,   266,  1335,   602,  -180,   595,  1450,   676,   618,
     695,    77,    78,  1656,   904,   598,   466,   498,   709,    79,
      79,    79,  1525,   595,    77,    78,   170,   595,  1375,  1881,
    1480,   298,  1486,  1487,   712,  1423,  1615,  1820,  1819,  1248,
    1616,   710,   339,   466,   402,   402,  1905,  1686,  1162,    79,
    1706,   942,  1755,   743,   942,   191,   676,    79,   711,    95,
      79,    79,   253,   263,    79,  1707,  1905,  1708,   467,  1066,
     111,   942,   707,   708,   951,    79,   953,  1162,   956,  1773,
     107,   717,   964,   742,    89,   968,   744,    61,    95,   745,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   111,
     746,    79,  1851,  1932,  1944,   747,   942,  1248,  1982,   107,
     993,   748,   942,    89,  2006,    72,    79,   260,  2003,   970,
     971,   972,  1887,   684,   595,   685,   686,   687,  1471,   256,
     246,   257,   466,  2013,  1954,  1518,    74,  2014,  1958,    -3,
      79,  1331,  1519,  1653,  1654,  1655,    77,    78,   433,    96,
     322,  1321,  1322,  1323,   688,   590,   676,   689,   690,   248,
    1319,   775,   691,   692,   629,  1043,  1044,   103,   418,   170,
     531,  -418,    79,  1344,    61,   902,   170,   -17,    96,    64,
      65,    66,    67,    68,    69,    70,  1524,  1526,   789,  1428,
    1363,   248,   944,   945,  1069,   788,  1071,  1547,   453,   140,
    1045,  1046,   799,  1525,   822,   447,  1566,  1567,  1568,  1569,
    1570,   493,   812,   485,  1235,  1066,   813,  1335,  1335,  1335,
     824,  1511,  1335,  1250,  1251,   339,   112,   286,   140,   814,
      79,   942,  1254,   816,    79,  -151,  -151,    79,  1045,  1387,
     486,   487,   977,   978,   979,   980,   140,  1443,  1444,   840,
    1110,   170,   170,  1448,  1444,   112,    61,   466,   447,   168,
     169,    64,    65,    66,    67,    68,    69,    70,  -116,  -116,
    -116,  -116,  -116,  -116,   488,   595,   447,   466,   817,    79,
     818,   533,  -535,  1549,  1549,  -115,  -115,  -115,  -115,  -115,
    -115,   148,  -533,   149,  1452,  1444,   652,   858,  1203,   595,
    1017,  1436,   170,  1488,  1436,   111,   111,   170,  1017,  1500,
     489,   871,   595,  1628,  1066,   107,   107,   849,  1715,    89,
      89,   693,   694,   873,   490,   491,   785,   877,   467,  1741,
    1066,  1742,  1444,   466,  1752,  1753,  1480,   890,    79,  1762,
     942,   322,   693,    79,    79,    79,  1810,  1811,   892,   530,
    1823,  1444,  1671,  1824,  1444,  1331,  1331,  1331,   150,  1508,
    1512,   604,   254,   264,   909,   819,   805,  1524,  1526,  1778,
    1779,   914,   693,  1672,   921,  1524,   494,    13,    14,    15,
      16,    17,   939,   943,    96,    96,   249,   922,  1480,  1174,
     946,  1175,   949,   806,   807,  2003,  2004,   269,  1441,  1442,
    1536,  1538,   990,   447,  1869,   973,   974,   995,   140,   975,
     976,    79,   981,   982,   453,   453,   339,    79,  1016,    79,
    1678,  1678,  1017,   103,  1397,  1398,    79,   808,    13,    14,
      15,    16,    17,  1227,   140,   140,  1024,  1063,  1586,   249,
     466,  1072,  1073,  1074,   447,  1165,  1075,  1076,  1077,   466,
    1093,  1095,   103,  -718,  1805,  1104,  1176,  -618,  1105,  1106,
    1166,   112,   112,   809,  1195,  1196,  1525,  1197,  1869,   248,
    1207,  1212,  1211,  1217,  1215,  1218,  1182,   810,   811,  1219,
    1220,  1221,  1302,   249,  1223,  1224,   466,  1225,  1230,  1231,
    1255,  1295,  -719,  1815,  1238,  1239,  1260,  1261,  1262,  1269,
     418,   531,  1270,  1271,   260,  1272,   140,  1280,    79,  -606,
     170,  -605,  1549,   170,   170,   170,   256,   246,   257,  1839,
    1314,  1336,  1337,    79,  1347,    79,  1774,  1350,  1351,  1524,
     820,   402,  1360,  1361,   111,  1362,  1367,   170,  -641,  1369,
     615,  1377,   942,   170,   107,  1371,   249,  1875,    89,  1381,
    1383,   552,  1384,  1385,  1391,  1393,  1420,   855,   170,  1421,
    1174,  1434,  1175,  1451,  1436,  1463,   595,  1476,  1481,   618,
      79,  1516,  1484,    79,  1480,  1482,   249,  1483,  1444,  1489,
     298,   534,   249,  1492,   466,  1501,  1502,  1504,   466,  1514,
    1340,  1540,  1517,  1579,  1581,   170,  1584,  1591,  1557,  1589,
     466,  1558,  1560,  1562,  1574,  1582,   466,  1583,  1592,  1594,
    1424,   249,   493,    96,  1595,   485,  1596,  1597,  1598,   447,
    1600,  1611,  1605,    79,    79,  1613,  1614,  1176,   969,  1709,
    1524,  1526,    79,   140,  1609,  1549,  1617,  1922,  1621,  1939,
    1629,  1839,   486,   487,   819,  1630,  1634,  1635,   426,  1645,
    1665,   402,   467,   402,  1643,   785,  1488,   111,   103,   103,
     530,  1682,  1652,   140,  1519,  1875,  1248,   107,  1956,  1875,
    1875,    89,  1685,  1687,    79,  1712,   488,   140,  1689,  1714,
     402,   466,  1719,   210,  1732,   466,  1726,   466,   531,  1730,
     112,  1731,  1740,  1205,  1746,  1978,  1733,  1747,  1750,  1871,
    1751,  1760,  1757,  1761,  1975,  1786,  1788,   466,  1789,  1798,
    1808,  1802,   489,  1128,  1797,  1804,  1809,  1994,  1817,  1821,
    1818,  1994,  1822,  -607,   298,  1853,   490,   491,   170,  1830,
    1831,   170,  1832,   249,  1833,  1834,    96,  2005,   498,  -518,
    1862,  1860,  1864,  2008,  1872,  1886,  1888,  1873,   466,   402,
    1889,   183,   466,   184,   569,  1890,  1891,  1753,  1902,   185,
    1926,   466,  1910,  1871,  1911,  1916,  1931,  1925,    79,  1927,
      79,   170,   453,   453,  1930,   570,   298,   494,  1934,  1549,
    1946,   466,  1937,   466,   466,    61,   140,  1947,   168,   169,
      64,    65,    66,    67,    68,    69,    70,  1951,  1955,  1549,
    1957,   111,  1962,  1091,  1963,  1976,   466,   249,   298,   820,
    1989,   107,  1977,   112,    82,    89,  1979,   147,  1243,    79,
      79,   111,   196,  1991,  1992,  1996,  1243,   249,  2000,  1997,
     466,   107,  2001,  2011,   855,    89,  1737,  1549,  2012,  1534,
     466,  1447,  2015,   679,   938,   983,   986,   249,   984,  1366,
     985,  1990,    79,  1373,   457,  1243,   248,   987,   467,   111,
    1716,  1194,  1940,  1222,  1949,   466,  1800,   466,  1226,   107,
    1796,    82,  1859,    89,   595,  1985,  1884,  1983,  1974,  1234,
      96,   249,  1710,   466,  1711,  1918,   180,   103,  1959,   466,
    1998,  1917,   855,  1382,   167,    82,   524,  1837,  1668,   466,
      96,   447,  1897,    79,   193,   249,  1513,  1379,   220,  1679,
    1062,   245,   249,    79,   795,    82,  1612,  1184,  1243,  1723,
     170,     3,  1213,   998,    13,    14,    15,    16,    17,   508,
     140,   999,  1000,     0,   170,   875,     0,     0,    96,     0,
       0,     0,     0,   170,     0,   249,   269,     0,     0,     0,
     140,     0,   147,     0,     0,     0,     0,   112,    82,   453,
     147,     0,     0,   296,   302,     0,     0,     0,     0,   193,
       0,     0,  1259,     0,     0,   321,     0,   112,     0,     0,
     170,     0,    57,     0,   193,     0,     0,     0,   140,  1266,
    1267,     0,   409,   180,   180,     0,     0,     0,     0,   531,
       0,   193,   170,   855,   147,   439,     0,     0,   245,     0,
     103,     0,     0,     0,   442,   112,     0,     0,     0,    61,
     855,   855,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,   220,   220,     0,  1993,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,   652,   296,
       0,  2002,     0,     0,     0,     0,     0,     0,    82,     0,
    1349,   601,     0,     0,     0,   602,     0,   193,     0,     0,
       0,   245,    77,   603,     0,     0,     0,     0,   170,     0,
       0,     0,   170,     0,   447,     0,    13,    14,    15,    16,
      17,     0,  1352,     0,   170,     0,   467,     0,     0,     0,
     170,   302,     0,     0,  1243,     0,     0,   302,   296,   296,
       0,   740,     0,     0,     0,   147,     0,   170,     0,     0,
       0,     0,   693,     0,   193,     0,   170,   751,     0,     0,
     754,     0,     0,     0,   321,   605,   614,     0,     0,   249,
       0,     0,     0,   193,    57,     0,     0,     0,     0,     0,
     249,   321,     0,     0,   103,   321,  1429,  1430,  1431,     0,
       0,     0,     0,  1432,  1433,     0,     0,     0,     0,     0,
       0,   249,     0,     0,   103,   170,     0,     0,     0,   170,
       0,   170,     0,     0,     0,     0,    61,   508,   409,   168,
     169,    64,    65,    66,    67,    68,    69,    70,     0,    61,
       0,   170,     0,    72,    64,    65,    66,    67,    68,    69,
      70,   954,   103,     0,     0,    13,    14,    15,    16,    17,
       0,   467,   409,   734,     0,   735,     0,   498,     0,     0,
       0,   193,   180,     0,    77,    78,     0,     0,     0,  1459,
    1460,     0,   170,     0,     0,     0,   170,     0,   147,     0,
      18,   955,   439,  1474,  1475,   170,   764,     0,   614,  1354,
       0,   193,     0,     0,  1924,     0,     0,     0,     0,     0,
     855,   855,     0,    57,     0,   170,     0,   170,   170,     0,
       0,     0,     0,     0,   855,   855,  1496,  1497,    47,    48,
      49,    50,    51,    52,    53,    54,   220,   467,     0,     0,
     170,     0,  1243,     0,     0,   220,     0,  1243,  1243,  1243,
       0,     0,     0,     0,     0,     0,     0,   855,   855,     0,
       0,  1585,     0,     0,   170,   296,     0,   409,   409,     0,
       0,   296,    72,   321,   170,     0,   193,   193,     0,     0,
      61,     0,   442,   168,   169,    64,    65,    66,    67,    68,
      69,    70,  1656,     0,     0,     0,   498,     0,     0,   170,
       0,   170,     0,    77,    78,     0,     0,     0,     0,     0,
       0,   296,     0,     0,     0,     0,     0,   170,     0,     0,
       0,     0,   296,   170,   296,     0,   321,     0,    82,     0,
       0,     0,     0,   170,     0,   193,     0,  2009,     0,  1356,
       0,     0,     0,   321,   439,     0,   614,  2016,     0,     0,
       0,     0,     0,   442,   605,     0,     0,     0,   605,   243,
       6,     7,     8,     9,    10,    11,    12,   321,     0,   637,
       0,     0,     0,   740,   740,     0,   193,   614,     0,     0,
     321,     0,     0,  1009,     0,    61,  1012,     0,   147,   249,
      64,    65,    66,    67,    68,    69,    70,   193,     0,     0,
       0,   409,     0,   147,   147,     0,   409,     0,     0,  1647,
    1648,     0,   409,     0,     0,   147,   147,   147,     0,     0,
       0,   249,     0,     0,  1688,     0,     0,  1243,   595,  1243,
       0,     0,     0,  1692,    74,     0,     0,   784,     0,     0,
     855,   855,     0,   638,     0,     0,     0,   508,     0,     0,
       0,  1080,     0,     0,     0,  1084,     0,  1717,   639,     0,
       0,   640,   641,    64,    65,    66,    67,    68,    69,    70,
    1721,   439,     0,     0,     0,     0,  1683,     0,     0,     0,
     442,   306,   307,   308,   309,     0,     0,   735,   735,     0,
       0,    61,   595,     0,     0,   409,    64,    65,    66,    67,
      68,    69,    70,     0,   193,    13,    14,    15,    16,    17,
       0,     0,   439,     0,     0,   764,  1727,   764,     0,    61,
       0,   442,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,   321,   321,     0,     0,     0,  1273,
      74,     0,     0,   442,   442,     0,  1745,   855,     0,     0,
       0,  1748,  1749,   321,     0,   296,     0,     0,     0,     0,
     249,     0,   442,    57,     0,     0,     0,     0,  1777,     0,
       0,   310,  1787,     0,   296,     0,     0,   855,     0,     0,
       0,     0,   855,   855,  1795,     0,     0,     0,     0,   311,
    1801,     0,     0,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   249,     0,
       0,     0,   409,     0,     0,     0,     0,     0,     0,   321,
       0,     0,    72,     0,     0,   147,   409,     0,   442,     0,
       0,     0,     0,    18,   321,   193,  1189,     0,     0,     0,
     740,     0,   762,    74,     0,     0,   602,   605,     0,     0,
       0,     0,    61,    77,   763,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,  1847,     0,     0,     0,  1852,
       0,  1854,     0,     0,     0,    51,    52,    53,    54,     0,
       0,     0,     0,  1880,     0,     0,     0,   439,     0,     0,
       0,  1879,     0,     0,     0,     0,   193,     0,     0,    13,
      14,    15,    16,    17,     0,     0,     0,     0,   578,     0,
       0,  1305,     0,     0,  1309,     0,     0,    61,     0,     0,
       0,     0,    64,    65,    66,    67,    68,    69,    70,   962,
       0,     0,  1906,     0,     0,     0,  1909,     0,     0,     0,
       0,     0,     0,    57,     0,  1915,     0,     0,     0,     0,
       0,     0,     0,     0,   735,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,  1933,     0,  1935,  1936,   963,
       0,   764,     0,     0,    61,     0,   249,     0,   764,    64,
      65,    66,    67,    68,    69,    70,     0,     0,    61,     0,
    1945,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,    61,    72,     0,   345,   346,    64,    65,    66,    67,
      68,    69,    70,     0,  1960,     0,    72,     0,     0,     0,
     321,     0,    73,    74,  1965,     0,     0,     0,     0,   442,
       0,     0,     0,    77,    78,     0,  1920,    74,     0,     0,
     498,     0,     0,     0,     0,     0,     0,    77,    78,  1988,
       0,  1965,    75,  1964,     0,     0,     0,   347,     0,     0,
     147,     0,     0,     0,     0,     0,     0,  1999,   409,     0,
       0,   114,     0,  1988,   114,     0,     0,     0,     0,     0,
       0,     0,     0,  2007,   855,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,   409,     0,  1457,
       0,     0,     0,    13,    14,    15,    16,    17,  1466,     0,
       0,     0,     0,     0,   245,    82,     0,     0,     0,   249,
       0,     0,     0,     0,     0,     0,     0,     0,   114,   296,
       0,     0,     0,     0,     0,   147,     0,     0,     0,     0,
       0,     0,   439,     0,   193,     0,     0,     0,    57,     0,
       0,   193,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,   251,   439,
       0,     0,   114,   147,     0,     0,     0,     0,   193,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    72,     0,   114,
       0,     0,     0,     0,     0,   114,     0,   114,     0,   387,
      72,   251,    13,    14,    15,    16,    17,   219,    74,     0,
       0,   318,   114,   349,     0,     0,   321,   321,    77,    78,
     295,    74,     0,     0,     0,   442,   442,     0,    61,   413,
       0,    77,    78,    64,    65,    66,    67,    68,    69,    70,
    1240,   114,   413,     0,  1241,   251,  1242,     0,     0,     0,
       0,     0,     0,     0,   147,   147,   147,   147,   147,   147,
      57,     0,     0,     0,  1520,   302,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,    74,   249,     0,
    1439,     0,     0,   409,   409,     0,     0,     0,     0,     0,
       0,    61,   114,     0,     0,   114,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,   251,     0,
       0,     0,     0,   245,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,   548,     0,  1660,    57,     0,
       0,     0,   439,   114,     0,     0,     0,     0,   251,    73,
      74,   193,     0,     0,   251,     0,     0,     0,     0,     0,
      77,    78,   114,     0,     0,   147,     0,     0,     0,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,   114,     0,   251,   114,     0,     0,     0,     0,     0,
       0,   649,     0,     0,   387,   654,     0,    72,   114,     0,
       0,     0,   114,     0,   660,   661,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,  1518,    74,   387,
     387,     0,     0,     0,     0,     0,     0,     0,    77,    78,
       0,     0,     0,     0,     0,   413,     0,    57,     0,    61,
     387,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,  1657,     0,     0,     0,  1520,     0,   409,     0,     0,
       0,  1520,     0,  1520,     0,     0,   193,     0,    61,   413,
     387,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,  1660,  1660,  1273,    74,     0,
       0,   302,   147,     0,     0,   114,    72,     0,     0,   413,
       0,     0,     0,     0,     0,   251,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,  1920,    74,     0,     0,
     498,     0,   409,     0,     0,     0,     0,    77,    78,     0,
       0,     0,     0,   321,     0,     0,   147,     0,     0,     0,
       0,     0,   193,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,    61,     0,   147,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,    72,     0,     0,   413,   413,     0,     0,     0,   251,
     114,     0,     0,     0,   321,   321,     0,     0,     0,     0,
       0,   219,    74,   442,   442,  1660,     0,     0,     0,  1657,
    1657,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,   114,     0,  1202,  1520,     0,   114,  1520,     0,   251,
     114,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,   114,   302,   114,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   409,     0,     0,     0,   349,
     114,   413,     0,   251,     0,     0,     0,     0,     0,    72,
       0,  1895,     0,     0,     0,  1660,     0,     0,   296,     0,
       0,     0,     0,     0,   114,     0,     0,   251,     0,  1018,
      74,   548,     0,   602,   251,     0,     0,   114,     0,   908,
      77,    78,     0,     0,     0,   114,    61,     0,  1660,   545,
     546,    64,    65,    66,    67,    68,    69,    70,   413,  1657,
     114,   114,     0,   413,     0,     0,     0,    57,  1520,   413,
       0,     0,   114,   114,   114,     0,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,     0,   147,    75,    61,  1660,
    1660,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,   671,     0,     0,
       0,   321,     0,     0,     0,     0,    72,     0,   413,  1657,
     442,     0,  1660,     0,     0,     0,    57,     0,     0,   147,
       0,     0,     0,     0,     0,     0,   295,    74,     0,     0,
       0,     0,   413,     0,     0,     0,   387,    77,    78,   147,
     147,     0,  1921,   302,     0,     0,     0,    61,     0,   413,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   719,
     720,   721,   722,   723,   724,   725,   726,   727,   728,   729,
       0,   114,   114,   204,     0,    72,     0,   147,    98,     0,
       0,   151,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,  1921,  1921,  1518,    74,     0,     0,     0,
     730,     0,     0,     0,     0,    61,    77,    78,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,   114,     0,
       0,     0,     0,     0,     0,     0,  1921,     0,     0,     0,
       0,     0,     0,    72,     0,    98,   831,   833,     0,     0,
       0,   251,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,   251,   762,    74,     0,   114,   602,     0,   195,
       0,     0,   114,   413,    77,   763,     0,     0,     0,     0,
       0,   114,     0,  1191,   413,     0,   114,   604,     0,   258,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,     0,     0,     0,    61,   387,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,   387,     0,     0,
       0,     0,     0,     0,     0,     0,   288,     0,     0,     0,
       0,     0,    98,    72,   413,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,   323,
       0,     0,     0,  1518,    74,     0,     0,     0,     0,   387,
    1519,     0,     0,    72,    77,    78,     0,   419,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,   288,   445,
     671,     0,     0,  1920,    74,   671,     0,   498,     0,     0,
       0,   671,     0,     0,    77,    78,     0,   114,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   492,     0,     0,
     671,     0,     0,     0,   114,   114,     0,     0,     0,     0,
       0,     0,     0,   512,     0,     0,     0,     0,   517,   519,
     108,     0,   195,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,   989,     0,     0,     0,
       0,     0,     0,     0,   539,     0,     0,   541,     0,   542,
       0,    72,     0,     0,     0,     0,     0,   114,     0,     0,
     559,     0,     0,     0,   259,     0,     0,     0,     0,     0,
       0,   219,    74,   571,     0,     0,   387,     0,     0,     0,
       0,    61,    77,    78,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,   114,   593,     0,
       0,   617,     0,     0,     0,   413,     0,   108,     0,    72,
       0,     0,     0,     0,     0,   624,     0,     0,     0,   624,
       0,     0,     0,     0,   327,     0,     0,     0,     0,   295,
      74,     0,     0,     0,   413,     0,     0,     0,     0,     0,
      77,    78,     0,   657,     0,     0,     0,     0,     0,     0,
     387,   251,   114,     0,   446,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,   114,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,  1191,   387,   387,   387,     0,     0,     0,
       0,   387,   387,    61,     0,  1417,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   678,   413,     0,    75,   376,
     114,     0,   288,     0,    61,   387,   593,     0,     0,    64,
      65,    66,    67,    68,    69,    70,  1240,     0,     0,   540,
    1241,     0,  1242,     0,     0,     0,     0,   113,     0,   657,
       0,     0,     0,     0,    75,   108,     0,     0,     0,     0,
       0,     0,   387,   387,   114,   114,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,  1624,     0,   114,   114,
       0,     0,     0,   114,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   594,     0,     0,   259,     0,     0,     0,
       0,     0,     0,     0,   113,     0,     0,   445,     0,     0,
     594,   114,   114,     0,   594,     0,     0,     0,     0,     0,
       0,   114,   114,   114,   114,   114,   114,     0,     0,     0,
       0,     0,   251,     0,     0,     0,     0,     0,   851,     0,
       0,     0,     0,   519,     0,     0,     0,   862,   261,   559,
     413,   413,     0,     0,     0,     0,     0,     0,    61,     0,
     323,     0,    98,    64,    65,    66,    67,    68,    69,    70,
    1240,     0,     0,     0,  1241,     0,  1242,   624,   883,    61,
     251,     0,   168,   169,    64,    65,    66,    67,    68,    69,
      70,   113,   894,     0,     0,     0,     0,     0,     0,   413,
       0,   593,     0,     0,     0,     0,   903,    74,   331,     0,
    1626,   594,     0,     0,   624,     0,     0,     0,     0,     0,
       0,     0,   114,     0,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,   448,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -422,  -422,     0,
    -422,    45,    46,     0,  -422,    61,     0,     0,   189,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,    57,   446,     0,   114,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   445,     0,     0,     0,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
       0,     0,  1001,   327,    74,    62,    63,   784,     0,   113,
     114,     0,   259,    61,   108,     0,   543,    63,    64,    65,
      66,    67,    68,    69,    70,   446,   883,   108,   251,   114,
      72,  1025,     0,     0,     0,   387,     0,     0,     0,     0,
       0,     0,   594,   446,     0,     0,     0,   596,   445,   445,
     261,     0,     0,    75,   244,     0,     0,     0,     0,   413,
       0,    77,    78,     0,   596,   992,   594,   445,   596,     0,
     114,     0,     0,   114,     0,     0,     0,     0,     0,   594,
       0,   114,    61,     0,     0,     0,     0,    64,    65,    66,
      67,    68,    69,    70,  1240,   851,   114,     0,  1241,    61,
    1242,     0,   168,   169,    64,    65,    66,    67,    68,    69,
      70,   114,     0,     0,     0,     0,   114,   114,     0,     0,
       0,   114,   114,     0,     0,     0,  1161,     0,     0,     0,
       0,    74,     0,   445,     0,     0,     0,     0,     0,   151,
       0,     0,     0,     0,     0,     0,     0,   456,   624,     0,
       0,  1193,     0,   851,     0,     0,     0,     0,  1199,  1539,
       0,     0,  1542,  1553,     0,   596,     0,     0,  1559,     0,
     446,   251,  1563,     0,  1565,     0,     0,     0,     0,     0,
      61,     0,   413,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   323,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   446,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   460,   387,
       0,     0,     0,   327,   327,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   448,     0,     0,     0,
       0,     0,   327,     0,     0,     0,     0,     0,     0,     0,
       0,   387,     0,     0,   851,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   331,     0,     0,
     327,   851,   851,   114,     0,     0,   261,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   448,
       0,   113,     0,     0,     0,     0,     0,     0,   114,     0,
       0,   108,  1651,     0,     0,     0,   596,   448,   327,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
       0,     0,     0,   594,   445,     0,   259,     0,   327,     0,
     596,     0,     0,     0,  1684,     0,   114,   114,     0,   387,
     251,   387,     0,   596,     0,     0,  1690,     0,     0,     0,
       0,     0,     0,  1693,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1332,     0,     0,     0,   387,     0,
       0,     0,  1161,     0,   114,     0,   446,     0,     0,     0,
       0,  1542,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,     0,     0,     0,
       0,  1161,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1380,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   448,     0,     0,   387,     0,   327,
       0,     0,     0,     0,     0,     0,   593,     0,     0,     0,
       0,     0,     0,     0,     0,   517,   327,   327,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   323,     0,   448,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1785,
       0,     0,     0,     0,     0,     0,     0,   331,   331,     0,
       0,  1792,  1794,     0,  1553,     0,     0,     0,     0,   327,
       0,     0,     0,     0,     0,     0,   331,     0,     0,     0,
       0,   851,   851,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   851,   851,     0,     0,     0,
     445,   445,     0,     0,   331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   851,   851,
       0,     0,     0,     0,     0,   113,     0,     0,  1332,  1332,
    1332,   151,   331,     0,     0,     0,   108,     0,     0,     0,
       0,     0,     0,  1856,     0,     0,     0,   596,     0,     0,
     261,     0,   331,     0,   259,     0,     0,  1548,  1548,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,   118,     0,     0,     0,     0,
     448,     0,     0,     0,     0,     0,   323,     0,   446,     0,
       0,  1912,     0,  1914,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,   151,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,   327,   327,     0,     0,
       0,     0,     0,   331,     0,     0,     0,     0,     0,     0,
     327,   327,     0,   118,     0,   327,   327,     0,     0,     0,
     331,   331,     0,     0,     0,   119,  1961,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,     0,  1969,  1971,
    1972,   851,   851,   327,   327,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1674,     0,   119,
     118,     0,     0,   331,     0,     0,   118,   851,   118,     0,
       0,     0,   108,   108,     0,     0,     0,     0,     0,     0,
     123,     0,     0,   123,     0,     0,  1691,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
     118,     0,   119,     0,   119,     0,     0,     0,     0,     0,
       0,   113,   118,     0,     0,     0,  1548,     0,     0,     0,
       0,   446,     0,     0,     0,     0,     0,   323,     0,     0,
     151,     0,     0,     0,     0,     0,   119,   123,   851,     0,
     113,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   261,     0,
       0,   123,     0,   118,     0,     0,   118,     0,   851,     0,
       0,   118,     0,   851,   851,     0,     0,     0,   445,   445,
       0,   123,     0,     0,     0,   596,     0,     0,     0,     0,
       0,     0,     0,     0,  1766,     0,     0,     0,     0,   119,
       0,     0,   119,     0,   118,     0,     0,   119,     0,     0,
       0,     0,   448,     0,     0,     0,   327,   327,   123,     0,
       0,     0,     0,   118,   123,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1548,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   327,     0,     0,     0,     0,     0,   123,   119,
     331,   331,     0,     0,     0,     0,     0,     0,     0,     0,
     123,   259,     0,     0,   331,   331,     0,     0,     0,   331,
     331,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,   331,   331,     0,
       0,     0,   327,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   327,   123,     0,     0,     0,     0,   123,
     118,     0,   119,     0,     0,     0,     0,     0,     0,     0,
    1870,     0,     0,     0,     0,     0,   113,   113,     0,     0,
       0,     0,     0,   327,     0,     0,   118,     0,   327,   327,
       0,     0,   123,   327,   327,   445,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,  1548,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,   448,     0,     0,     0,     0,
       0,     0,     0,  1548,  1870,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,   118,     0,     0,     0,
       0,  1548,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,  1953,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   119,     0,     0,   851,     0,   118,     0,     0,
       0,   118,     0,   118,     0,     0,     0,     0,   123,     0,
     331,   331,     0,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   123,     0,     0,   119,     0,   119,
       0,     0,     0,     0,     0,   594,   331,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   261,     0,     0,     0,     0,
     327,     0,     0,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,   118,
       0,   118,   118,     0,   118,   113,     0,     0,     0,     0,
     118,     0,     0,   118,   118,   118,   331,     0,   108,   594,
       0,     0,   119,   123,   123,     0,     0,   331,     0,     0,
       0,     0,     0,     0,     0,   119,     0,   119,   119,     0,
     119,     0,     0,     0,     0,     0,   119,     0,     0,   119,
     119,   119,     0,     0,     0,     1,   108,   331,   145,     0,
       0,     0,   331,   331,     0,   123,     0,   331,   331,   123,
       0,   123,     0,     0,     0,     0,   242,     0,     0,     0,
       0,     0,     0,     0,   123,    13,    14,    15,    16,    17,
     327,     0,    19,   118,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -422,
    -422,     0,  -422,    45,    46,     0,  -422,     0,   113,   119,
       0,   192,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,   123,
     123,     0,   123,     0,     0,     0,     0,     0,   123,     0,
       0,   123,   123,   123,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,   118,    75,   301,     0,     0,   596,
       0,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,   123,     0,     0,   331,     0,     0,     0,     0,   119,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   283,     0,     0,     0,     0,     0,
       0,  1838,   113,   596,     0,     0,     0,     0,     0,   520,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   283,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   283,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,     0,   551,   555,     0,     0,     0,   360,     0,
     562,   563,   361,     0,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   573,     0,     0,     0,
       0,   363,     0,     0,   331,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,   591,     0,     0,     0,     0,
       0,   123,   123,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,   203,     0,     0,     0,     0,     0,   214,
     215,   677,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,   118,     0,
       0,     0,   718,   277,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   756,     0,
       0,     0,   759,     0,   119,   118,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,   781,     0,   118,     0,   782,   783,     0,     0,   786,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,   118,   800,   801,   802,   803,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,   825,     0,     0,     0,     0,     0,     0,
       0,   828,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   360,     0,     0,     0,   361,     0,   362,     0,   283,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
     866,     0,     0,     0,   123,     0,     0,   551,     0,   567,
       0,   364,   365,   872,   366,     0,   367,  1790,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   123,   373,   374,   886,   891,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,   123,   118,   118,   118,   118,   118,   118,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
       0,   123,   377,    77,    78,   378,   379,   380,   381,     0,
       0,   118,   118,     0,     0,     0,  1791,  -179,   119,   119,
     119,   119,   119,   119,     0,     0,     0,     0,     0,     0,
     934,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,    13,    14,    15,    16,    17,   119,   119,    19,
     165,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,   165,     0,     0,     0,
      45,    46,     0,   118,     0,   760,     0,   761,     0,     0,
       0,     0,     0,     0,     0,     0,   777,   778,     0,   997,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1014,     0,     0,     0,  1015,   119,
       0,     0,   165,     0,     0,     0,     0,   886,     0,     0,
       0,   656,     0,     0,     0,   165,     0,   165,     0,     0,
     123,   123,   123,   123,   123,   123,     0,     0,     0,  1055,
       0,     0,     0,     0,     0,     0,     0,     0,  1064,     0,
       0,     0,     0,     0,  1067,     0,     0,   351,     0,   123,
     123,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,   351,     0,   -16,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,   861,     0,     0,     0,     1,     0,
     118,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,   165,     0,     0,   165,
     165,     0,     0,   165,     0,     1,   165,   165,     0,     0,
     118,   123,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1216,
     119,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,     0,   123,     0,
       0,     0,  1264,     0,     0,     0,  1265,     0,     0,     0,
       0,     0,     0,   886,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1278,     0,     0,     0,     0,   123,   119,
    1279,     0,     0,     0,     0,     0,     0,     0,     0,  1283,
       0,  1284,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1040,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,  1311,     0,     0,     0,  1312,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   145,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   351,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,  1108,
    1109,     0,     0,     0,     0,     0,     0,   118,     0,     0,
    1167,  1168,  1169,     0,     0,  1171,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,  1399,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,   118,     0,     0,     0,     0,
       0,     0,   351,     0,     0,     0,  1422,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1236,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,   165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,  1257,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   268,     0,   271,     0,   273,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,  1494,     0,
       0,     0,  1495,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,  1281,     0,
       0,     0,     0,   247,     0,   271,   273,  1285,  1286,  1287,
    1288,     0,  1530,     0,     0,  1293,  1294,     0,     0,     0,
       0,     0,     0,     0,     0,  1301,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1315,   247,  1316,     0,
       0,   165,   165,     0,     0,     0,     0,   165,     0,     0,
       0,     0,  1590,     0,     0,  1593,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
    1601,   165,   165,     0,   165,     0,   165,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1372,   163,     0,     0,     0,     0,     0,     0,     0,
     247,     0,   271,   273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,  1386,     0,   165,
    1631,     0,     0,     0,  1390,     0,  1392,  1394,     0,  1636,
     247,     0,     0,  1637,     0,  1400,   247,  1401,     0,  1402,
       0,  1404,     0,     0,     0,     0,  1412,  1641,  1642,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   275,   247,     0,     0,     0,     0,
       0,   620,     0,   273,     0,     0,     0,   281,     0,   282,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1454,     0,     0,     0,
       0,     0,     0,  1461,  1462,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1485,     0,     0,
       0,     0,     0,     0,  1490,     0,     0,  1491,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1724,
    1725,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   502,   503,     0,     0,   507,     0,   214,   510,   511,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   620,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1580,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,   338,     0,     0,     0,     0,     0,     0,     0,
    1599,     0,     0,     0,   247,     0,     0,     0,  1604,   247,
    1606,   247,     0,     0,   589,     0,     0,     0,     0,     0,
       0,   435,   338,     0,     0,   165,     0,  1803,   165,   621,
       0,   247,     0,   247,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1593,   247,     0,   501,     0,     0,  1632,  1633,     0,     0,
     501,     0,     0,   247,     0,     0,     0,     0,  1828,     0,
       0,  1638,  1639,     0,  1640,     0,     0,     0,     0,     0,
     171,   174,     0,  1644,     0,   247,     0,   620,   273,     0,
       0,     0,     0,  1649,  1650,  1846,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
     620,     0,     0,     0,  1866,   212,   247,  1867,     0,     0,
       0,     0,     0,   750,     0,     0,     0,     0,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   247,
     268,   338,   606,     0,     0,     0,   165,   165,     0,     0,
       0,     0,     0,     0,     0,   290,     0,     0,   291,     0,
       0,     0,   627,     0,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,     0,     0,     0,     0,     0,     0,
     821,     0,     0,     0,     0,     0,     0,     0,     0,  1728,
    1729,     0,     0,     0,     0,     0,     0,     0,  1938,  1735,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,   165,     0,   165,   165,     0,
       0,     0,     0,     0,     0,   477,     0,     0,     0,     0,
       0,     0,   501,     0,  1758,  1759,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   501,   752,
       0,   501,   755,     0,     0,     0,     0,   165,     0,   338,
       0,     0,     0,   606,     0,     0,     0,     0,     0,     0,
     528,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,   897,   898,     0,     0,     0,     0,
       0,     0,     0,     0,   501,     0,     0,   905,   501,     0,
       0,     0,     0,     0,  1816,     0,     0,     0,   574,     0,
       0,     0,     0,     0,   165,   577,   579,     0,     0,     0,
     586,     0,  1825,   247,     0,  1826,  1827,     0,     0,     0,
     338,     0,  1829,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   631,     0,     0,   247,     0,   312,     0,     0,
     312,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
     501,     0,     0,   338,     0,     0,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
     881,   338,     0,     0,     0,   255,     0,     0,     0,     0,
       0,   606,     0,  1003,  1004,   606,     0,     0,     0,  1008,
       0,     0,   899,     0,   338,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1029,     0,     0,  1032,  1033,  1919,  1036,   212,  1038,  1039,
       0,   165,     0,     0,   200,     0,     0,   165,   303,   779,
     780,     0,     0,     0,     0,     0,     0,     0,     0,   343,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,  1079,     0,     0,
       0,  1083,     0,     0,     0,  1950,     0,   247,     0,   455,
       0,     0,   459,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1967,
       0,     0,   165,     0,     0,     0,  1973,     0,   338,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1986,     0,     0,   501,   501,     0,     0,     0,     0,
       0,     0,   200,     0,   501,  1010,     0,   501,  1013,     0,
       0,     0,     0,     0,     0,   255,     0,  1200,     0,   338,
       0,     0,   606,     0,   606,   606,     0,     0,     0,     0,
       0,   606,     0,     0,     0,     0,     0,     0,     0,     0,
     312,   338,   338,     0,     0,     0,   165,   165,     0,     0,
       0,   459,     0,     0,   351,     0,     0,     0,   165,   200,
     338,     0,     0,     0,   501,     0,     0,     0,   501,     0,
       0,     0,   501,  1081,     0,     0,   501,  1085,   599,     0,
     616,     0,     0,   247,  1088,     0,     0,     0,   631,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,   338,   501,     0,     0,
       0,     0,   675,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1200,
       0,   165,     0,     0,   606,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,    18,   200,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,   338,     0,   599,  1304,    45,    46,
    1308,     0,   776,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,   165,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1042,     0,     0,     0,     0,     0,     0,  1054,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,   247,     0,     0,     0,     0,     0,
       0,   501,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,   200,     0,     0,     0,     0,   455,   606,   606,
       0,     0,     0,     0,     0,   606,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,   247,   165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1112,     0,     0,     0,     0,     0,     0,
    1406,     0,     0,     0,     0,     0,     0,   338,  1415,  1416,
     343,     0,   501,  1306,     0,   501,  1310,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,     0,
     885,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1206,     0,     0,     0,     0,   631,
       0,   599,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1456,     0,     0,     0,     0,
       0,     0,   200,     0,  1465,     0,     0,  1469,     0,  1472,
    1473,     0,     0,     0,     0,   675,     0,   675,   675,     0,
     675,     0,     0,     0,     0,     0,   675,     0,     0,   675,
     675,   675,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1499,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   338,
       0,     0,     0,   411,     0,   606,  1408,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   440,     0,     0,     0,
     247,     0,     0,     0,     0,   455,   338,     0,     0,   468,
       0,   468,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,  1587,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   455,     0,     0,     0,
     501,  1458,     0,     0,     0,     0,     0,     0,     0,   501,
    1467,     0,   606,     0,     0,     0,     0,     0,   455,   455,
       0,     0,     0,   338,   338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   568,     0,     0,
       0,     0,     0,     0,     0,  1353,  1355,  1357,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1469,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,  1198,
       0,     0,     0,     0,     0,  1376,    13,    14,    15,    16,
      17,     0,     0,   247,     0,     0,     0,  1646,     0,     0,
    1112,     0,     0,   455,     0,     0,     0,     0,     0,     0,
     200,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     776,     0,   360,     0,     0,     0,   361,     0,   362,     0,
       0,     0,     0,     0,     0,     0,     0,   631,     0,   338,
       0,     0,     0,     0,    57,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,   343,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,  1722,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,     0,     0,   375,     0,   468,    75,   376,     0,     0,
     797,     0,     0,   377,   438,    78,   378,   379,   380,   381,
       0,     0,     0,     0,     0,     0,     0,     0,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   501,     0,     0,     0,  1770,  1771,
    1529,     0,   247,  1531,     0,     0,     0,     0,     0,     0,
    1775,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   455,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   865,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   675,   440,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,   893,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1836,     0,     0,     0,     0,     0,     0,
       0,   338,   338,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   501,   501,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   928,
       0,     0,   501,     0,     0,     0,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,   599,     0,     0,     0,
     797,   948,     0,     0,   950,     0,   952,  1894,     0,     0,
       0,     0,   961,     0,   966,   961,     0,     0,     0,     0,
       0,     0,     0,   343,     0,     0,     0,   675,     0,     0,
       0,     0,  1680,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   994,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   996,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1005,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   501,     0,     0,     0,
     440,     0,     0,   994,   501,     0,     0,     0,     0,     0,
     455,   455,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1058,     0,     0,   468,   631,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   675,   675,
     675,     0,   675,   675,     0,     0,     0,     0,   338,   459,
       0,     0,   501,  1896,     0,     0,   501,     0,     0,  1089,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   501,
       0,     0,     0,     0,     0,     0,     0,   255,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   411,     0,     0,
       0,     0,     0,     0,     0,     0,   343,  1190,  1192,     0,
       0,     0,     0,     0,     0,   440,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1814,
     501,   501,     0,     0,     0,     0,   468,     0,   631,     0,
       0,     0,     0,     0,     0,   961,     0,     0,     0,     0,
       0,     0,     0,     0,  1987,     0,     0,     0,   994,     0,
       0,     0,     0,   501,     0,     0,  1229,     0,     0,     0,
    1348,     0,     0,   961,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   360,     0,     0,     0,   361,     0,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,   200,  1114,     0,   363,    -2,     0,  1116,  -238,  -238,
    1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  1128,  -301,  1129,  1130,  1131,  1132,  1133,     0,  1134,
       0,   364,   365,     0,   462,   255,   367,  1135,  1136,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1137,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,   468,     0,  1297,     0,
    1299,     0,     0,     0,     0,     0,     0,   343,     0,     0,
       0,  -238,   375,     0,     0,    75,   376,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   675,     0,     0,     0,  -179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,   455,
       0,     0,     0,     0,     0,     0,  1364,  1364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1410,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,     0,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1403,     0,     0,
       0,     0,     0,  1413,     0,     0,   360,     0,     0,     0,
     361,     0,   362,     0,     0,     0,     0,     0,     0,     0,
     440,     0,     0,     0,     0,     0,     0,     0,    57,   363,
       0,     0,     0,     0,     0,     0,     0,   468,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   961,     0,     0,   797,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
     675,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1493,   375,     0,     0,
      75,   376,     0,     0,     0,   455,     0,   377,  1411,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   961,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   675,     0,     0,   459,     0,     0,
       0,     0,   468,     0,     0,   797,     0,     0,     0,     0,
       0,     0,     0,  1987,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1348,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   948,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1602,  1603,     0,     0,     0,     0,     0,
     360,     0,     0,     0,   361,     0,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
     797,  1114,     0,   363,    -2,     0,  1116,  -239,  -239,  1117,
    1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,
    1128,  -301,  1129,  1130,  1131,  1132,  1133,     0,  1134,     0,
     364,   365,     0,   462,     0,   367,  1135,  1136,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,  1137,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   411,     0,
    -239,   375,  1720,  1673,    75,   376,     0,     0,     0,   279,
       0,   377,    77,    78,   378,   379,   380,   381,  1348,     0,
       0,     0,     0,     0,     0,     0,  -179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,   361,     0,   362,     0,     0,     0,  1713,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1114,     0,   363,    -2,     0,  1116,     0,     0,  1117,  1118,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    -301,  1129,  1130,  1131,  1132,  1133,     0,  1134,  1736,   364,
     365,  1738,   462,     0,   367,  1135,  1136,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,  1137,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,    75,   376,     0,     0,     0,   279,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     0,     0,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,  1113,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   360,     0,    45,    46,   361,     0,   362,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,  1114,    57,  1115,    -2,     0,  1116,
       0,     0,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  -301,  1129,  1130,  1131,  1132,  1133,
       0,  1134,     0,   364,   365,    60,   462,     0,   367,  1135,
    1136,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,  1137,   370,   371,   372,   961,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -3,   375,     0,     0,    75,   407,     0,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,     0,     0,     0,     0,  -179,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1113,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   360,     0,    45,    46,   361,
       0,   362,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,  1114,    57,  1115,    -2,
       0,  1116,     0,     0,  1117,  1118,  1119,  1120,  1121,  1122,
    1123,  1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,  1131,
    1132,  1133,     0,  1134,     0,   364,   365,    60,   462,     0,
     367,  1135,  1136,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,  1137,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     407,     0,     0,     0,   279,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,     0,     0,     0,
       0,  -179,     4,   243,     6,     7,     8,     9,    10,    11,
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
    1550,    75,   407,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,     0,
       0,     0,  1551,  1552,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   360,
       0,    45,    46,   361,     0,   362,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
     365,    60,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,    75,   407,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,     0,     0,     0,  1551,  1552,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   360,     0,    45,    46,   361,     0,   362,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,     0,    57,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,   365,    60,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,  1541,    75,   407,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     4,
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
       0,     0,     0,     0,     0,   375,     0,     0,    75,   407,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
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
      75,   437,     0,     0,     0,     0,     0,   377,   438,    78,
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
       0,     0,    75,  1187,     0,     0,     0,     0,     0,   377,
    1188,    78,   378,   379,   380,   381,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     360,     0,    45,    46,   361,     0,   362,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,     0,
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
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
    1845,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,
       0,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,  1865,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,     0,    57,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    59,     0,     0,     0,    60,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -422,  -422,     0,  -422,    45,    46,
       0,  -422,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,    61,
      45,    46,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,   244,     0,     0,     0,  -731,     0,     0,    77,    78,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,     0,     0,
       0,     0,  -354,  -354,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -354,     0,     0,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,     0,     0,     0,
       0,  -355,  -355,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -355,     0,     0,     0,    75,    76,
       0,     0,     0,     0,     0,     0,    77,    78,   242,   243,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,     0,    75,   244,     0,
       0,  1324,     0,     0,     0,    77,    78,  1325,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,  1326,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1327,     0,     0,     0,
      75,   924,     0,     0,  1324,     0,     0,     0,    77,    78,
    1325,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,  1326,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1505,
       0,     0,     0,    75,   924,     0,     0,  1324,     0,     0,
       0,    77,    78,  1325,     0,     0,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,  1326,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1506,     0,     0,     0,    75,   924,     0,     0,
    1324,     0,     0,     0,    77,    78,  1325,     0,     0,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,  1326,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1507,     0,     0,     0,    75,
     924,     0,     0,     0,     0,     0,     0,    77,    78,   243,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,     0,    75,   244,     0,
       0,     0,  -735,     0,     0,    77,    78,   243,     6,     7,
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
       0,     0,     0,  1348,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,   360,    75,   244,     0,   361,     0,
     362,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1114,     0,   363,     0,     0,
    1116,  1778,  1779,  1117,  1118,  1119,  1120,  1121,  1122,  1123,
    1124,  1125,  1126,  1127,  1128,  -301,  1129,  1130,  1131,  1132,
    1133,     0,  1134,     0,   364,   365,     0,   462,     0,   367,
    1135,  1136,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,  1137,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,  1348,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   376,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,   360,     0,     0,     0,   361,     0,   362,     0,
    -179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1114,     0,   363,     0,     0,  1116,     0,
       0,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,
    1126,  1127,  1128,  -301,  1129,  1130,  1131,  1132,  1133,     0,
    1134,     0,   364,   365,     0,   462,     0,   367,  1135,  1136,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
    1137,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,   279,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,     0,     0,     0,     0,  -179,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,  1049,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -603,    75,   320,     0,
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
       0,     0,    72,     0,  1754,     0,     0,     0,     0,     0,
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
      72,     0,  1756,     0,     0,     0,     0,     0,     0,     0,
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
     301,     0,     0,     0,     0,     0,     0,    77,    78,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -422,  -422,     0,  -422,    45,    46,     0,  -422,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,    57,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   656,    75,   244,     0,
       0,     0,     0,     0,     0,    77,    78,    13,    14,    15,
      16,    17,    18,   662,    19,   663,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   360,     0,    45,    46,   361,     0,   362,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   664,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   665,     0,
       0,     0,   279,     0,   377,    77,    78,   666,   667,   380,
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
     406,    75,   407,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   360,     0,    45,    46,   361,     0,   362,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   665,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,    13,
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
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     407,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,    13,    14,    15,    16,    17,    18,     0,
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
     377,    77,    78,   378,   379,   380,   381,    13,    14,    15,
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
     381,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,    76,     0,     0,     0,  -733,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,    76,     0,    13,    14,    15,    16,    17,    77,
      78,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -422,  -422,
       0,  -422,    45,    46,     0,  -422,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   301,     0,     0,     0,     0,
       0,     0,    77,    78,   556,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,    75,     0,    45,    46,    62,    63,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,  1425,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   930,    75,   924,     0,     0,    62,
      63,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   924,     0,
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
      63,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   286,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     433,     0,     0,    62,    63,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   320,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,    62,    63,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   286,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   433,     0,     0,     0,     0,     0,
       0,    77,    78,   242,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -422,  -422,     0,  -422,
      45,    46,     0,  -422,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,    75,     0,    45,    46,    62,    63,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   301,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   924,     0,     0,
       0,     0,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   924,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
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
       0,    75,   301,    62,    63,     0,     0,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,    77,
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
       0,     0,     0,     0,     0,     0,     0,     0,   850,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -616,    75,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1681,     0,     0,
       0,     0,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,    75,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    62,    63,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -422,  -422,     0,  -422,    45,    46,     0,  -422,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,    75,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -422,  -422,    75,  -422,    45,    46,     0,
    -422,     0,     0,   360,     0,     0,     0,   361,     0,   362,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,   364,   365,     0,   462,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,    75,
       0,     0,     0,     0,   375,    74,     0,   463,   464,     0,
       0,     0,   465,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,  1232,     0,    75,
     376,     0,     0,     0,  1233,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,   957,
    1532,    75,   376,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     375,     0,     0,    75,   376,     0,     0,     0,   465,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   375,   796,     0,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,   957,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,   988,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     375,  1298,     0,    75,   376,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
    1358,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   375,     0,  1784,    75,   376,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,  1970,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     648,     0,     0,    75,   376,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   653,     0,     0,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   659,     0,     0,    75,   376,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,   864,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,     0,     0,   377,   438,
      78,   378,   379,   380,   381,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -421,
    -421,     0,  -421,    45,    46,     0,  -421,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -422,
    -422,     0,  -422,    45,    46,     0,  -422,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57
};

static const yytype_int16 yycheck[] =
{
       1,   283,     1,     4,    73,   173,    73,    73,   162,   691,
     614,   220,    73,   162,   219,   256,   375,   177,   150,   178,
     207,   241,   321,   676,   163,   408,   873,  1123,    73,   762,
     465,   178,   601,   630,     1,    58,   963,   735,   633,   338,
      73,   219,   219,   342,    75,     1,   601,   222,   375,  1656,
      95,    95,  1283,  1284,    55,    56,     1,    58,   765,    58,
      73,   514,   515,   605,   771,   992,     1,  1350,   366,   601,
       1,     4,    73,     4,  1719,   219,   669,   139,    82,  1656,
    1656,    82,   525,    82,   573,   762,   764,   296,   162,    90,
     295,    58,   535,   323,    95,    87,    95,    98,   764,    98,
     149,   102,    58,   102,   236,   150,    82,     1,   157,    73,
    1778,   219,   181,    58,   181,   290,   291,   295,   295,   782,
     181,    73,   145,    58,    73,   219,   219,    58,  1055,   191,
     338,    75,    76,   576,   342,     1,    88,   800,     4,   140,
     439,    73,   143,   147,   145,   859,   145,  1206,   181,   150,
     219,   295,   219,   219,    70,   156,   843,   102,   219,   859,
     149,   174,   163,   155,   462,   157,    70,    98,    82,   102,
     762,   102,   762,   149,   219,   220,   220,     0,   145,   180,
     181,   180,   635,   870,   173,     1,   219,   295,     4,   145,
     787,   236,    58,   762,   195,   120,   195,    95,  1350,  1351,
     145,   295,   295,   866,   205,   155,     1,   762,     0,   210,
     145,   130,     1,   157,   145,   445,    82,   181,   219,   220,
       0,   220,   764,   115,   174,   174,   295,   152,   295,   295,
     762,    87,  1900,   147,   295,   236,   229,   153,    87,   921,
    1657,   245,    58,   274,   245,   149,   245,   321,   631,   181,
     295,   296,   296,   172,   255,   219,   497,   258,  1097,   258,
     149,   254,   156,  1102,   265,   179,   149,   949,   157,    58,
      87,   264,   149,   139,   275,   276,   174,   278,   483,   145,
     562,   147,   115,   492,   471,  1018,   102,   219,   131,   742,
     131,   590,   781,   782,   295,   296,  1579,   296,   302,  1782,
      96,   157,   303,  1001,   443,   483,   483,    19,   157,   310,
     311,   800,  1019,  1920,   315,   614,   283,  1962,   915,   914,
     153,   648,   165,   650,   651,   191,   653,   258,   627,   145,
     689,   245,   659,   316,  1173,   662,   663,   664,   283,   483,
     157,  1018,   607,  1920,  1920,   938,   347,  1025,   283,  1766,
      70,   352,   894,   149,   355,   356,   145,   154,  1417,  1025,
     155,   156,   571,   593,   569,   439,   781,   782,   131,   174,
     211,   354,   149,   435,   155,   483,   242,   866,  1107,   245,
     512,   146,   590,    82,   155,   800,   518,   617,   302,   483,
     483,   569,   569,   174,   624,  1107,    95,   840,   443,    98,
     149,  1115,   165,   102,  1067,   409,   148,   321,   173,     1,
    1641,  1642,  1099,   155,   483,  1115,   483,   283,   419,   627,
     419,   149,   483,   192,   573,   569,  1018,  1579,  1018,   149,
    1144,  1107,   463,   153,   228,   149,   302,   231,   483,    59,
      60,   442,   443,   284,  1144,   934,   155,   492,   492,  1018,
     483,   866,  1869,   454,   455,   174,  1552,   146,  1941,   253,
    1556,   569,   463,  1018,   465,   174,    58,   512,   157,   263,
     104,   105,  1241,   518,    56,   569,   569,    59,    60,  1318,
      62,   180,   483,  1025,   173,  1968,  1018,    73,  1195,   104,
     105,   492,    70,   492,   283,   409,   195,     4,    70,   573,
     569,   766,   569,    89,  1921,   770,    98,    70,   569,    75,
      76,   512,  1439,   558,   779,   780,     3,   518,   152,   483,
     106,   220,     3,   155,   569,   439,   571,   571,   155,    12,
      13,    14,    15,    16,   375,    70,  1953,   157,   152,   146,
     614,   534,   174,   409,   155,   152,   245,   174,    55,    56,
    1702,   483,   575,   145,   174,   556,   716,   558,    70,   258,
     995,   125,   126,   174,    70,  1272,   173,   149,   569,   435,
     571,   149,   571,   157,   575,   153,   575,   149,  1067,   155,
     155,   153,   881,    90,   585,  1189,   149,    70,   589,   165,
     153,   157,  1321,  1322,  1323,   562,   157,  1828,  1251,   174,
     514,   515,   789,  1699,   155,   169,   170,  1202,   575,  1321,
    1322,  1323,  1209,   174,   149,   560,   775,   562,   153,   575,
     621,   851,   149,    70,   591,   764,   149,   562,   775,   156,
     575,   150,   633,   140,   173,   155,   143,   149,   157,   786,
     575,   153,   157,    70,   575,  1321,  1322,  1323,   131,   156,
    1350,  1351,  1067,   883,   174,    70,   163,    70,  1340,   174,
     157,  1258,   822,   504,   153,  1903,   258,  1514,   509,   158,
     802,    70,   157,   881,   103,  1444,  1445,  1446,   107,  1917,
     681,   110,   683,   112,   685,   526,   717,   153,   689,   821,
     556,   692,   158,  1789,  1846,   536,   562,  1624,   171,  1626,
     614,  1797,   149,   210,   155,  1943,   153,   151,  1361,   575,
     149,   505,   156,   265,  1866,   131,   717,   762,   155,   764,
     419,   635,   149,   174,   276,   912,   153,  1216,   148,  1182,
     149,   323,    70,   527,   149,   155,   149,   174,   153,   533,
     153,   149,    70,   537,   160,   161,   155,    70,   255,   151,
     149,   520,  1904,   155,   153,   115,   165,   802,   265,   575,
      70,   762,   151,   764,  1860,   151,   905,    70,   275,   276,
     156,   278,   160,   562,   151,   776,   821,  1042,   151,   167,
     168,   352,   783,  1436,   355,   934,   575,   151,   789,   162,
     163,   792,   759,   492,  1283,   149,   303,   151,  1233,   153,
     801,   802,   803,   310,   311,   151,   151,   648,   315,   151,
     156,   149,   653,  1054,   156,   153,   151,  1414,   659,   248,
     821,   149,   591,  1206,   151,   153,   149,   149,   742,   151,
     153,    12,    13,    14,    15,    16,   155,   678,  1435,   149,
     347,   988,   155,   153,   157,   352,   149,   151,   355,   153,
     153,   152,   153,   445,   149,   156,   857,   858,   859,   149,
     934,    21,     4,     5,     6,     7,     8,     9,    10,    11,
     155,     3,   151,   151,   129,  1062,   155,   155,   149,  1579,
      12,    13,    14,    15,    16,  1024,  1025,   149,   149,    70,
    1189,   153,   859,  1220,   149,   155,   325,   326,   153,   328,
      96,   330,   155,   859,   905,   160,   161,   155,   909,   149,
    1399,  1086,   151,   914,   859,   149,   155,  1064,   173,   920,
       3,   151,   151,  1358,   859,   155,   155,   148,   859,    12,
      13,    14,    15,    16,   157,   442,   149,    87,    70,   872,
     153,   872,   151,   143,   144,   145,   155,   454,   455,   718,
     131,   104,   105,   151,   955,   155,   149,   155,   151,   873,
     153,   151,   963,  1193,  1561,   165,  1325,   559,    12,    13,
      14,    15,    16,  1018,   174,   151,   152,   192,  1198,  1024,
    1025,    46,    47,   575,    49,   151,   827,    70,    53,   155,
     759,   992,    56,   859,   995,    59,    60,   838,    62,  1434,
     841,   149,  1702,   151,   845,   153,   872,   151,   157,   151,
     157,   155,   781,   782,  1609,  1183,  1184,  1018,   149,   157,
     151,  1618,   153,  1024,  1025,   617,    70,   162,   163,   149,
      76,   800,   174,   153,  1417,     4,     5,     6,     7,     8,
       9,    10,    11,   859,  1176,   154,   149,   151,   842,   556,
     153,   155,   157,   129,  1055,   101,   872,   157,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1216,  1350,   151,
     859,   173,  1313,   149,   143,   144,   145,   153,   585,   149,
     129,   115,   589,   153,   160,   161,   155,   149,   151,     3,
     149,   151,   155,   129,    63,   155,   165,   866,   123,   124,
     149,   149,   531,  1107,   153,   174,   321,  1254,   155,   324,
     166,   160,   161,   149,   621,  1189,  1117,   153,   161,  1120,
    1121,  1122,  1331,   338,   160,   161,   633,   342,   174,  1811,
    1284,  1176,  1291,  1292,   129,   151,   151,  1734,  1733,   155,
     155,   159,  1216,  1144,  1291,  1292,  1846,   151,  1115,  1150,
     151,   155,  1641,   151,   155,   154,   155,  1158,   171,  1115,
    1161,  1162,  1161,  1162,  1165,   151,  1866,   151,   205,   155,
    1115,   155,   127,   128,   681,  1176,   683,  1144,   685,   151,
    1115,   152,   689,   155,  1115,   692,   151,   101,  1144,   151,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1144,
     151,  1202,   151,   151,  1904,   151,   155,   155,   151,  1144,
     717,   151,   155,  1144,   151,   129,  1217,  1162,   155,   696,
     697,   698,  1819,   118,   439,   120,   121,   122,  1273,  1162,
    1161,  1162,  1233,   151,  1925,   149,   150,   155,  1929,   154,
    1241,  1107,   156,  1505,  1506,  1507,   160,   161,   153,  1115,
    1399,  1104,  1105,  1106,   149,   154,   155,   152,   153,   851,
    1101,   131,   157,   158,     9,   154,   155,   859,  1182,   776,
     862,   131,  1273,  1114,   101,  1189,   783,   156,  1144,   106,
     107,   108,   109,   110,   111,   112,  1331,  1331,   155,  1616,
    1131,   883,   160,   161,   801,   156,   803,  1579,  1067,  1115,
     154,   155,   149,  1512,   149,   520,   108,   109,   110,   111,
     112,  1520,   151,  1518,   154,   155,   151,  1321,  1322,  1323,
     154,  1325,  1326,   154,   155,  1399,  1115,   153,  1144,   151,
    1331,   155,   156,   151,  1335,   154,   155,  1338,   154,   155,
    1518,  1518,   703,   704,   705,   706,  1162,   154,   155,   157,
     857,   858,   859,   154,   155,  1144,   101,  1358,   573,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    12,    13,
      14,    15,    16,    17,  1518,   590,   591,  1378,   151,  1380,
     151,  1380,   157,  1350,  1351,    12,    13,    14,    15,    16,
      17,  1324,   157,  1324,   154,   155,   363,    68,   905,   614,
     154,   155,   909,   154,   155,  1350,  1351,   914,   154,   155,
    1518,   154,   627,   154,   155,  1350,  1351,   157,  1578,  1350,
    1351,   388,   389,   149,  1518,  1518,   463,    76,   465,   154,
     155,   154,   155,  1434,   154,   155,  1590,   154,  1439,   154,
     155,  1590,   409,  1444,  1445,  1446,   155,   156,    17,  1380,
     154,   155,  1518,   154,   155,  1321,  1322,  1323,  1324,  1325,
    1326,   173,  1161,  1162,   155,  1674,  1671,  1512,  1512,    75,
      76,   157,   439,  1518,   149,  1520,  1520,    12,    13,    14,
      15,    16,    17,   151,  1350,  1351,    98,   174,  1642,  1422,
     151,  1422,   157,  1671,  1671,   155,   156,   109,  1242,  1243,
    1341,  1342,   174,   718,  1803,   699,   700,   157,  1324,   701,
     702,  1512,   707,   708,  1283,  1284,  1590,  1518,   154,  1520,
    1525,  1526,   154,  1115,  1183,  1184,  1527,  1671,    12,    13,
      14,    15,    16,    17,  1350,  1351,    17,   148,  1379,   151,
    1541,   151,   151,   151,   759,    68,   151,   151,   151,  1550,
     151,   151,  1144,   148,  1714,   157,  1422,   151,   157,   157,
     174,  1350,  1351,  1671,   151,   151,  1775,   151,  1867,  1161,
     148,   151,   157,   155,   151,   151,   173,  1671,  1671,   151,
     155,   151,   173,   195,   151,   151,  1587,   151,   151,   151,
     151,   148,   148,  1725,   154,   154,   151,   151,   151,   151,
    1514,  1193,   151,   151,  1549,   151,  1422,   154,  1609,   151,
    1117,   151,  1579,  1120,  1121,  1122,  1549,  1548,  1549,  1778,
     151,   151,   155,  1624,   149,  1626,  1671,   149,   149,  1674,
    1674,  1778,   149,   149,  1579,   149,    13,  1144,   156,    72,
    1870,    89,   155,  1150,  1579,   174,   258,  1807,  1579,   156,
     174,  1158,   154,   154,   174,   174,   148,   551,  1165,   148,
    1593,   157,  1593,   174,   155,   151,   881,   154,   151,   884,
    1671,   174,   151,  1674,  1828,   155,   288,   155,   155,   154,
    1725,  1380,   294,   151,  1685,   151,   148,   148,  1689,   149,
     149,    78,   174,   149,   148,  1202,   149,   148,   174,   151,
    1701,   174,   174,   174,   174,   174,  1707,   174,   148,   155,
    1217,   323,  1921,  1579,   155,  1920,   154,   154,   154,   934,
     151,   148,   154,  1724,  1725,   151,   156,  1593,   695,  1570,
    1775,  1775,  1733,  1549,   157,  1702,   156,  1869,   118,  1898,
     148,  1900,  1920,  1920,  1953,   151,   151,   151,   151,   151,
     174,  1898,   789,  1900,   154,   792,   154,  1702,  1350,  1351,
    1691,  1530,   148,  1579,   156,  1925,   155,  1702,  1927,  1929,
    1930,  1702,   151,   149,  1775,   149,  1920,  1593,   151,   149,
    1927,  1782,   107,   155,   148,  1786,   154,  1788,  1380,   154,
    1579,   154,   148,     9,   151,  1955,   157,   151,   151,  1803,
     151,   151,   154,   151,  1951,    73,    73,  1808,   174,   148,
     151,   149,  1920,    88,   174,   174,   151,  1977,   154,   148,
     154,  1981,   148,   151,  1869,    73,  1920,  1920,  1335,   151,
     151,  1338,   151,   445,   151,   151,  1702,  1996,   153,   152,
      73,   174,   174,  2003,   156,   151,   148,   174,  1849,  1996,
     151,  1920,  1853,  1920,  1920,   151,   151,   155,   148,  1920,
     101,  1862,   150,  1867,   150,   148,    73,   156,  1869,   149,
    1871,  1378,  1641,  1642,   155,  1920,  1921,  1921,   149,  1846,
     165,  1882,   148,  1884,  1885,   101,  1702,   165,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   174,   154,  1866,
     174,  1846,   107,   828,   107,   151,  1907,   519,  1953,  1953,
     148,  1846,   156,  1702,     1,  1846,   151,     4,   955,  1920,
    1921,  1866,  1921,   148,   151,   149,   963,   539,    73,   174,
    1931,  1866,   151,   151,   828,  1866,  1616,  1904,   174,  1340,
    1941,  1248,   174,   377,   668,   709,   712,   559,   710,  1133,
     711,  1968,  1953,  1144,  1953,   992,  1548,   713,   995,  1904,
    1579,   886,  1900,   930,  1917,  1966,  1706,  1968,   935,  1904,
    1702,    58,  1792,  1904,  1189,  1963,  1812,  1962,  1950,   946,
    1846,   593,  1571,  1984,  1571,  1867,    73,  1579,  1930,  1990,
    1981,  1866,   886,  1165,    48,    82,   250,  1775,  1512,  2000,
    1866,  1216,  1836,  2004,    82,   617,  1326,  1158,    95,  1526,
     789,    98,   624,  2014,   472,   102,  1422,   876,  1055,  1593,
    1527,     0,   920,   734,    12,    13,    14,    15,    16,   230,
    1846,   734,   734,    -1,  1541,   585,    -1,    -1,  1904,    -1,
      -1,    -1,    -1,  1550,    -1,   657,   658,    -1,    -1,    -1,
    1866,    -1,   139,    -1,    -1,    -1,    -1,  1846,   145,  1828,
     147,    -1,    -1,   150,   151,    -1,    -1,    -1,    -1,   147,
      -1,    -1,   997,    -1,    -1,   162,    -1,  1866,    -1,    -1,
    1587,    -1,    70,    -1,   162,    -1,    -1,    -1,  1904,  1014,
    1015,    -1,   179,   180,   181,    -1,    -1,    -1,    -1,  1691,
      -1,   179,  1609,   997,   191,   192,    -1,    -1,   195,    -1,
    1702,    -1,    -1,    -1,   192,  1904,    -1,    -1,    -1,   101,
    1014,  1015,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   219,   220,    -1,  1976,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,  1115,   236,
      -1,  1992,    -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,
    1117,   149,    -1,    -1,    -1,   153,    -1,   245,    -1,    -1,
      -1,   258,   160,   161,    -1,    -1,    -1,    -1,  1685,    -1,
      -1,    -1,  1689,    -1,  1399,    -1,    12,    13,    14,    15,
      16,    -1,   174,    -1,  1701,    -1,  1233,    -1,    -1,    -1,
    1707,   288,    -1,    -1,  1241,    -1,    -1,   294,   295,   296,
      -1,   412,    -1,    -1,    -1,   302,    -1,  1724,    -1,    -1,
      -1,    -1,  1189,    -1,   302,    -1,  1733,   428,    -1,    -1,
     431,    -1,    -1,    -1,   321,   322,   323,    -1,    -1,   851,
      -1,    -1,    -1,   321,    70,    -1,    -1,    -1,    -1,    -1,
     862,   338,    -1,    -1,  1846,   342,  1223,  1224,  1225,    -1,
      -1,    -1,    -1,  1230,  1231,    -1,    -1,    -1,    -1,    -1,
      -1,   883,    -1,    -1,  1866,  1782,    -1,    -1,    -1,  1786,
      -1,  1788,    -1,    -1,    -1,    -1,   101,   488,   375,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   101,
      -1,  1808,    -1,   129,   106,   107,   108,   109,   110,   111,
     112,   113,  1904,    -1,    -1,    12,    13,    14,    15,    16,
      -1,  1358,   409,   149,    -1,   412,    -1,   153,    -1,    -1,
      -1,   409,   419,    -1,   160,   161,    -1,    -1,    -1,  1264,
    1265,    -1,  1849,    -1,    -1,    -1,  1853,    -1,   435,    -1,
      17,   153,   439,  1278,  1279,  1862,   443,    -1,   445,   174,
      -1,   439,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,
    1264,  1265,    -1,    70,    -1,  1882,    -1,  1884,  1885,    -1,
      -1,    -1,    -1,    -1,  1278,  1279,  1311,  1312,    55,    56,
      57,    58,    59,    60,    61,    62,   483,  1434,    -1,    -1,
    1907,    -1,  1439,    -1,    -1,   492,    -1,  1444,  1445,  1446,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1311,  1312,    -1,
      -1,  1378,    -1,    -1,  1931,   512,    -1,   514,   515,    -1,
      -1,   518,   129,   520,  1941,    -1,   514,   515,    -1,    -1,
     101,    -1,   520,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   149,    -1,    -1,    -1,   153,    -1,    -1,  1966,
      -1,  1968,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   558,    -1,    -1,    -1,    -1,    -1,  1984,    -1,    -1,
      -1,    -1,   569,  1990,   571,    -1,   573,    -1,   575,    -1,
      -1,    -1,    -1,  2000,    -1,   573,    -1,  2004,    -1,   160,
      -1,    -1,    -1,   590,   591,    -1,   593,  2014,    -1,    -1,
      -1,    -1,    -1,   591,   601,    -1,    -1,    -1,   605,     4,
       5,     6,     7,     8,     9,    10,    11,   614,    -1,    12,
      -1,    -1,    -1,   734,   735,    -1,   614,   624,    -1,    -1,
     627,    -1,    -1,   744,    -1,   101,   747,    -1,   635,  1161,
     106,   107,   108,   109,   110,   111,   112,   635,    -1,    -1,
      -1,   648,    -1,   650,   651,    -1,   653,    -1,    -1,  1494,
    1495,    -1,   659,    -1,    -1,   662,   663,   664,    -1,    -1,
      -1,  1193,    -1,    -1,  1541,    -1,    -1,  1624,  1803,  1626,
      -1,    -1,    -1,  1550,   150,    -1,    -1,   153,    -1,    -1,
    1494,  1495,    -1,    86,    -1,    -1,    -1,   808,    -1,    -1,
      -1,   812,    -1,    -1,    -1,   816,    -1,  1584,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1587,   718,    -1,    -1,    -1,    -1,  1530,    -1,    -1,    -1,
     718,    63,    64,    65,    66,    -1,    -1,   734,   735,    -1,
      -1,   101,  1867,    -1,    -1,   742,   106,   107,   108,   109,
     110,   111,   112,    -1,   742,    12,    13,    14,    15,    16,
      -1,    -1,   759,    -1,    -1,   762,  1601,   764,    -1,   101,
      -1,   759,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,   781,   782,    -1,    -1,    -1,   149,
     150,    -1,    -1,   781,   782,    -1,  1631,  1601,    -1,    -1,
      -1,  1636,  1637,   800,    -1,   802,    -1,    -1,    -1,    -1,
    1332,    -1,   800,    70,    -1,    -1,    -1,    -1,  1685,    -1,
      -1,   153,  1689,    -1,   821,    -1,    -1,  1631,    -1,    -1,
      -1,    -1,  1636,  1637,  1701,    -1,    -1,    -1,    -1,   171,
    1707,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,  1380,    -1,
      -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,   866,
      -1,    -1,   129,    -1,    -1,   872,   873,    -1,   866,    -1,
      -1,    -1,    -1,    17,   881,   873,   883,    -1,    -1,    -1,
    1001,    -1,   149,   150,    -1,    -1,   153,   894,    -1,    -1,
      -1,    -1,   101,   160,   161,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,  1782,    -1,    -1,    -1,  1786,
      -1,  1788,    -1,    -1,    -1,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,  1810,    -1,    -1,    -1,   934,    -1,    -1,
      -1,  1808,    -1,    -1,    -1,    -1,   934,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    -1,    -1,   157,    -1,
      -1,  1072,    -1,    -1,  1075,    -1,    -1,   101,    -1,    -1,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,  1849,    -1,    -1,    -1,  1853,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,  1862,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1001,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1882,    -1,  1884,  1885,   153,
      -1,  1018,    -1,    -1,   101,    -1,  1548,    -1,  1025,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   101,    -1,
    1907,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   101,   129,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,  1931,    -1,   129,    -1,    -1,    -1,
    1067,    -1,   149,   150,  1941,    -1,    -1,    -1,    -1,  1067,
      -1,    -1,    -1,   160,   161,    -1,   149,   150,    -1,    -1,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,  1966,
      -1,  1968,   152,  1938,    -1,    -1,    -1,   157,    -1,    -1,
    1107,    -1,    -1,    -1,    -1,    -1,    -1,  1984,  1115,    -1,
      -1,     1,    -1,  1990,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2000,  1938,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,  1144,    -1,  1260,
      -1,    -1,    -1,    12,    13,    14,    15,    16,  1269,    -1,
      -1,    -1,    -1,    -1,  1161,  1162,    -1,    -1,    -1,  1691,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,  1176,
      -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,
      -1,    -1,  1189,    -1,  1182,    -1,    -1,    -1,    70,    -1,
      -1,  1189,    82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    98,  1216,
      -1,    -1,   102,  1220,    -1,    -1,    -1,    -1,  1216,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   129,    -1,   139,
      -1,    -1,    -1,    -1,    -1,   145,    -1,   147,    -1,   178,
     129,   151,    12,    13,    14,    15,    16,   149,   150,    -1,
      -1,   161,   162,   163,    -1,    -1,  1283,  1284,   160,   161,
     149,   150,    -1,    -1,    -1,  1283,  1284,    -1,   101,   179,
      -1,   160,   161,   106,   107,   108,   109,   110,   111,   112,
     113,   191,   192,    -1,   117,   195,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1321,  1322,  1323,  1324,  1325,  1326,
      70,    -1,    -1,    -1,  1331,  1332,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,   150,  1870,    -1,
     153,    -1,    -1,  1350,  1351,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   242,    -1,    -1,   245,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,   258,    -1,
      -1,    -1,    -1,  1380,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,   275,    -1,  1508,    70,    -1,
      -1,    -1,  1399,   283,    -1,    -1,    -1,    -1,   288,   149,
     150,  1399,    -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,
     160,   161,   302,    -1,    -1,  1422,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   321,    -1,   323,   324,    -1,    -1,    -1,    -1,    -1,
      -1,   360,    -1,    -1,   363,   364,    -1,   129,   338,    -1,
      -1,    -1,   342,    -1,   373,   374,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,   388,
     389,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   375,    -1,    70,    -1,   101,
     409,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1508,    -1,    -1,    -1,  1512,    -1,  1514,    -1,    -1,
      -1,  1518,    -1,  1520,    -1,    -1,  1514,    -1,   101,   409,
     439,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,  1656,  1657,   149,   150,    -1,
      -1,  1548,  1549,    -1,    -1,   435,   129,    -1,    -1,   439,
      -1,    -1,    -1,    -1,    -1,   445,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,   149,   150,    -1,    -1,
     153,    -1,  1579,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,  1590,    -1,    -1,  1593,    -1,    -1,    -1,
      -1,    -1,  1590,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,   101,    -1,  1616,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   129,    -1,    -1,   514,   515,    -1,    -1,    -1,   519,
     520,    -1,    -1,    -1,  1641,  1642,    -1,    -1,    -1,    -1,
      -1,   149,   150,  1641,  1642,  1766,    -1,    -1,    -1,  1656,
    1657,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   551,    -1,   157,  1671,    -1,   556,  1674,    -1,   559,
     560,    -1,   562,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,   573,  1691,   575,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,  1702,    -1,    -1,    -1,   589,
     590,   591,    -1,   593,    -1,    -1,    -1,    -1,    -1,   129,
      -1,  1832,    -1,    -1,    -1,  1836,    -1,    -1,  1725,    -1,
      -1,    -1,    -1,    -1,   614,    -1,    -1,   617,    -1,   149,
     150,   621,    -1,   153,   624,    -1,    -1,   627,    -1,   629,
     160,   161,    -1,    -1,    -1,   635,   101,    -1,  1869,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   648,  1766,
     650,   651,    -1,   653,    -1,    -1,    -1,    70,  1775,   659,
      -1,    -1,   662,   663,   664,    -1,   695,   696,   697,   698,
     699,   700,   701,   702,   703,   704,   705,   706,   707,   708,
     709,   710,   711,   712,   713,    -1,  1803,   152,   101,  1920,
    1921,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,
      -1,  1828,    -1,    -1,    -1,    -1,   129,    -1,   718,  1836,
    1828,    -1,  1953,    -1,    -1,    -1,    70,    -1,    -1,  1846,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
      -1,    -1,   742,    -1,    -1,    -1,   775,   160,   161,  1866,
    1867,    -1,  1869,  1870,    -1,    -1,    -1,   101,    -1,   759,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
      -1,   781,   782,   146,    -1,   129,    -1,  1904,     1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     800,    -1,    -1,  1920,  1921,   149,   150,    -1,    -1,    -1,
     173,    -1,    -1,    -1,    -1,   101,   160,   161,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   828,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1953,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    58,   514,   515,    -1,    -1,
      -1,   851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   859,
      -1,    -1,   862,   149,   150,    -1,   866,   153,    -1,    82,
      -1,    -1,   872,   873,   160,   161,    -1,    -1,    -1,    -1,
      -1,   881,    -1,   883,   884,    -1,   886,   173,    -1,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   930,    -1,    -1,    -1,   101,   935,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   946,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,
      -1,    -1,   145,   129,   934,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   162,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,   988,
     156,    -1,    -1,   129,   160,   161,    -1,   180,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,   191,   192,
     648,    -1,    -1,   149,   150,   653,    -1,   153,    -1,    -1,
      -1,   659,    -1,    -1,   160,   161,    -1,   997,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   220,    -1,    -1,
     678,    -1,    -1,    -1,  1014,  1015,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   236,    -1,    -1,    -1,    -1,   241,   242,
      58,    -1,   245,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   714,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   267,    -1,    -1,   270,    -1,   272,
      -1,   129,    -1,    -1,    -1,    -1,    -1,  1067,    -1,    -1,
     283,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,   296,    -1,    -1,  1115,    -1,    -1,    -1,
      -1,   101,   160,   161,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,  1107,   321,    -1,
      -1,   324,    -1,    -1,    -1,  1115,    -1,   145,    -1,   129,
      -1,    -1,    -1,    -1,    -1,   338,    -1,    -1,    -1,   342,
      -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,  1144,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,   366,    -1,    -1,    -1,    -1,    -1,    -1,
    1189,  1161,  1162,    -1,   192,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,  1189,
      -1,    -1,    -1,  1193,  1223,  1224,  1225,    -1,    -1,    -1,
      -1,  1230,  1231,   101,    -1,  1205,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   149,  1216,    -1,   152,   153,
    1220,    -1,   435,    -1,   101,  1254,   439,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,   267,
     117,    -1,   119,    -1,    -1,    -1,    -1,     1,    -1,   462,
      -1,    -1,    -1,    -1,   152,   283,    -1,    -1,    -1,    -1,
      -1,    -1,  1291,  1292,  1264,  1265,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,    -1,  1278,  1279,
      -1,    -1,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   321,    -1,    -1,   324,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    58,    -1,    -1,   520,    -1,    -1,
     338,  1311,  1312,    -1,   342,    -1,    -1,    -1,    -1,    -1,
      -1,  1321,  1322,  1323,  1324,  1325,  1326,    -1,    -1,    -1,
      -1,    -1,  1332,    -1,    -1,    -1,    -1,    -1,   551,    -1,
      -1,    -1,    -1,   556,    -1,    -1,    -1,   560,   102,   562,
    1350,  1351,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
     573,    -1,   575,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,   117,    -1,   119,   590,   591,   101,
    1380,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   145,   605,    -1,    -1,    -1,    -1,    -1,    -1,  1399,
      -1,   614,    -1,    -1,    -1,    -1,   619,   150,   162,    -1,
     153,   439,    -1,    -1,   627,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1422,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,   192,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    70,   520,    -1,  1494,  1495,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   718,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   735,   551,   150,   104,   105,   153,    -1,   283,
    1530,    -1,   560,   101,   562,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   573,   759,   575,  1548,  1549,
     129,   764,    -1,    -1,    -1,  1584,    -1,    -1,    -1,    -1,
      -1,    -1,   590,   591,    -1,    -1,    -1,   321,   781,   782,
     324,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,  1579,
      -1,   160,   161,    -1,   338,   153,   614,   800,   342,    -1,
    1590,    -1,    -1,  1593,    -1,    -1,    -1,    -1,    -1,   627,
      -1,  1601,   101,    -1,    -1,    -1,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   828,  1616,    -1,   117,   101,
     119,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1631,    -1,    -1,    -1,    -1,  1636,  1637,    -1,    -1,
      -1,  1641,  1642,    -1,    -1,    -1,   859,    -1,    -1,    -1,
      -1,   150,    -1,   866,    -1,    -1,    -1,    -1,    -1,   872,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   881,    -1,
      -1,   884,    -1,   886,    -1,    -1,    -1,    -1,   891,  1347,
      -1,    -1,  1350,  1351,    -1,   439,    -1,    -1,  1356,    -1,
     718,  1691,  1360,    -1,  1362,    -1,    -1,    -1,    -1,    -1,
     101,    -1,  1702,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   759,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,  1778,
      -1,    -1,    -1,   781,   782,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   520,    -1,    -1,    -1,
      -1,    -1,   800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1810,    -1,    -1,   997,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   551,    -1,    -1,
     828,  1014,  1015,  1803,    -1,    -1,   560,    -1,   562,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   573,
      -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,  1828,    -1,
      -1,   859,  1500,    -1,    -1,    -1,   590,   591,   866,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1846,    -1,    -1,    -1,
      -1,    -1,    -1,   881,  1067,    -1,   884,    -1,   886,    -1,
     614,    -1,    -1,    -1,  1532,    -1,  1866,  1867,    -1,  1898,
    1870,  1900,    -1,   627,    -1,    -1,  1544,    -1,    -1,    -1,
      -1,    -1,    -1,  1551,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,  1927,    -1,
      -1,    -1,  1115,    -1,  1904,    -1,   934,    -1,    -1,    -1,
      -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1951,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1144,    -1,    -1,    -1,    -1,    -1,    -1,  1938,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   718,    -1,    -1,  1996,    -1,   997,
      -1,    -1,    -1,    -1,    -1,    -1,  1189,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1198,  1014,  1015,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1216,    -1,   759,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1687,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   781,   782,    -1,
      -1,  1699,  1700,    -1,  1702,    -1,    -1,    -1,    -1,  1067,
      -1,    -1,    -1,    -1,    -1,    -1,   800,    -1,    -1,    -1,
      -1,  1264,  1265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1278,  1279,    -1,    -1,    -1,
    1283,  1284,    -1,    -1,   828,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1311,  1312,
      -1,    -1,    -1,    -1,    -1,   859,    -1,    -1,  1321,  1322,
    1323,  1324,   866,    -1,    -1,    -1,  1144,    -1,    -1,    -1,
      -1,    -1,    -1,  1791,    -1,    -1,    -1,   881,    -1,    -1,
     884,    -1,   886,    -1,  1162,    -1,    -1,  1350,  1351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
     934,    -1,    -1,    -1,    -1,    -1,  1399,    -1,  1216,    -1,
      -1,  1859,    -1,  1861,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,  1422,
      -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,
      -1,    -1,    -1,    -1,    -1,    -1,  1264,  1265,    -1,    -1,
      -1,    -1,    -1,   997,    -1,    -1,    -1,    -1,    -1,    -1,
    1278,  1279,    -1,    82,    -1,  1283,  1284,    -1,    -1,    -1,
    1014,  1015,    -1,    -1,    -1,    58,  1934,    -1,    -1,    -1,
      -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,  1946,  1947,
    1948,  1494,  1495,  1311,  1312,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1520,    -1,   102,
     139,    -1,    -1,  1067,    -1,    -1,   145,  1530,   147,    -1,
      -1,    -1,  1350,  1351,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,     4,    -1,    -1,  1549,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,
     179,    -1,   145,    -1,   147,    -1,    -1,    -1,    -1,    -1,
      -1,  1115,   191,    -1,    -1,    -1,  1579,    -1,    -1,    -1,
      -1,  1399,    -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,
    1593,    -1,    -1,    -1,    -1,    -1,   179,    58,  1601,    -1,
    1144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1162,    -1,
      -1,    82,    -1,   242,    -1,    -1,   245,    -1,  1631,    -1,
      -1,   250,    -1,  1636,  1637,    -1,    -1,    -1,  1641,  1642,
      -1,   102,    -1,    -1,    -1,  1189,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1657,    -1,    -1,    -1,    -1,   242,
      -1,    -1,   245,    -1,   283,    -1,    -1,   250,    -1,    -1,
      -1,    -1,  1216,    -1,    -1,    -1,  1494,  1495,   139,    -1,
      -1,    -1,    -1,   302,   145,    -1,   147,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1702,
     283,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1530,    -1,    -1,    -1,    -1,    -1,   179,   302,
    1264,  1265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     191,  1549,    -1,    -1,  1278,  1279,    -1,    -1,    -1,  1283,
    1284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,
      -1,  1579,    -1,    -1,    -1,    -1,    -1,  1311,  1312,    -1,
      -1,    -1,  1590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   242,    -1,  1601,   245,    -1,    -1,    -1,    -1,   250,
     409,    -1,   375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1803,    -1,    -1,    -1,    -1,    -1,  1350,  1351,    -1,    -1,
      -1,    -1,    -1,  1631,    -1,    -1,   435,    -1,  1636,  1637,
      -1,    -1,   283,  1641,  1642,  1828,   409,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   302,    -1,  1846,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   435,    -1,    -1,  1399,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1866,  1867,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   514,   515,    -1,    -1,    -1,
      -1,  1904,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,  1921,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   514,   515,    -1,    -1,  1938,    -1,   556,    -1,    -1,
      -1,   560,    -1,   562,    -1,    -1,    -1,    -1,   409,    -1,
    1494,  1495,    -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   556,   435,    -1,    -1,   560,    -1,   562,
      -1,    -1,    -1,    -1,    -1,  1803,  1530,    -1,    -1,    -1,
      -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1549,    -1,    -1,    -1,    -1,
    1828,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1846,   648,
      -1,   650,   651,    -1,   653,  1579,    -1,    -1,    -1,    -1,
     659,    -1,    -1,   662,   663,   664,  1590,    -1,  1866,  1867,
      -1,    -1,   635,   514,   515,    -1,    -1,  1601,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   648,    -1,   650,   651,    -1,
     653,    -1,    -1,    -1,    -1,    -1,   659,    -1,    -1,   662,
     663,   664,    -1,    -1,    -1,     0,  1904,  1631,     3,    -1,
      -1,    -1,  1636,  1637,    -1,   556,    -1,  1641,  1642,   560,
      -1,   562,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   575,    12,    13,    14,    15,    16,
    1938,    -1,    19,   742,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,  1702,   742,
      -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,   635,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   648,    -1,   650,
     651,    -1,   653,    -1,    -1,    -1,    -1,    -1,   659,    -1,
      -1,   662,   663,   664,    -1,    -1,    -1,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   872,   873,   152,   153,    -1,    -1,  1803,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,
      -1,   742,    -1,    -1,  1828,    -1,    -1,    -1,    -1,   872,
     873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   229,    -1,    -1,    -1,    -1,    -1,
      -1,     1,  1866,  1867,    -1,    -1,    -1,    -1,    -1,   244,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   254,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   264,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1904,    -1,    -1,   278,   279,    -1,    -1,    -1,    48,    -1,
     285,   286,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   301,    -1,    -1,    -1,
      -1,    71,    -1,    -1,  1938,    -1,    -1,    -1,   859,    -1,
      -1,    -1,    -1,    -1,    -1,   320,    -1,    -1,    -1,    -1,
      -1,   872,   873,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    92,
      93,   376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,  1107,    -1,
      -1,    -1,   407,   126,    -1,    -1,  1115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   433,    -1,
      -1,    -1,   437,    -1,  1107,  1144,    -1,    -1,    -1,    -1,
      -1,    -1,  1115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   456,    -1,  1162,    -1,   460,   461,    -1,    -1,   464,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1144,    -1,  1182,   479,   480,   481,   482,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1162,
      -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   506,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1182,
      -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,   534,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,  1220,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,
     565,    -1,    -1,    -1,  1115,    -1,    -1,   572,    -1,   292,
      -1,    98,    99,   578,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,  1144,   121,   122,   601,   602,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1162,  1321,  1322,  1323,  1324,  1325,  1326,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,  1182,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,  1350,  1351,    -1,    -1,    -1,   173,   174,  1321,  1322,
    1323,  1324,  1325,  1326,    -1,    -1,    -1,    -1,    -1,    -1,
     665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,
      -1,    -1,    12,    13,    14,    15,    16,  1350,  1351,    19,
      47,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    73,    -1,    -1,    -1,
      50,    51,    -1,  1422,    -1,   438,    -1,   440,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   449,   450,    -1,   734,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   749,    -1,    -1,    -1,   753,  1422,
      -1,    -1,   119,    -1,    -1,    -1,    -1,   762,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   132,    -1,   134,    -1,    -1,
    1321,  1322,  1323,  1324,  1325,  1326,    -1,    -1,    -1,   784,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,
      -1,    -1,    -1,    -1,   799,    -1,    -1,   164,    -1,  1350,
    1351,    -1,    -1,    -1,    -1,  1514,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   181,    -1,   156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   836,    -1,    -1,   557,    -1,    -1,    -1,   843,    -1,
    1549,  1514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   219,    -1,    -1,    -1,   223,    -1,    -1,   226,
     227,    -1,    -1,   230,    -1,   870,   233,   234,    -1,    -1,
    1579,  1422,    -1,    -1,    -1,    -1,  1549,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1593,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1579,  1616,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   924,
    1593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   295,    -1,
      -1,   298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1616,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1514,    -1,    -1,    -1,   334,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,  1549,    -1,
      -1,    -1,  1007,    -1,    -1,    -1,  1011,    -1,    -1,    -1,
      -1,    -1,    -1,  1018,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1028,    -1,    -1,    -1,    -1,  1579,  1702,
    1035,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1044,
      -1,  1046,  1593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   428,    -1,    -1,    -1,  1616,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1078,    -1,    -1,    -1,  1082,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1096,    -1,    -1,  1099,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   495,   852,
     853,    -1,    -1,    -1,    -1,    -1,    -1,  1846,    -1,    -1,
     863,   864,   865,    -1,    -1,   868,    -1,    -1,    -1,    -1,
      -1,  1702,    -1,    -1,    -1,    -1,    -1,  1866,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1846,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1187,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1866,    -1,  1904,    -1,    -1,    -1,    -1,
      -1,    -1,   569,    -1,    -1,    -1,  1211,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   948,    -1,    -1,    -1,    -1,
      -1,  1904,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   609,   610,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   622,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      98,   994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,   111,    -1,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1846,    -1,    -1,  1303,    -1,
      -1,    -1,  1307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1866,    -1,    -1,  1041,    -1,
      -1,    -1,    -1,   151,    -1,   153,   154,  1050,  1051,  1052,
    1053,    -1,  1337,    -1,    -1,  1058,  1059,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1068,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1904,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1089,   195,  1091,    -1,
      -1,   738,   739,    -1,    -1,    -1,    -1,   744,    -1,    -1,
      -1,    -1,  1387,    -1,    -1,  1390,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   765,    -1,
    1405,   768,   769,    -1,   771,    -1,   773,   774,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1144,    47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     258,    -1,   260,   261,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   812,    -1,  1170,    -1,   816,
    1455,    -1,    -1,    -1,  1177,    -1,  1179,  1180,    -1,  1464,
     288,    -1,    -1,  1468,    -1,  1188,   294,  1190,    -1,  1192,
      -1,  1194,    -1,    -1,    -1,    -1,  1199,  1482,  1483,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   323,    -1,    -1,    -1,    -1,
      -1,   329,    -1,   331,    -1,    -1,    -1,   132,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   892,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1259,    -1,    -1,    -1,
      -1,    -1,    -1,  1266,  1267,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1290,    -1,    -1,
      -1,    -1,    -1,    -1,  1297,    -1,    -1,  1300,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1594,
    1595,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   226,   227,    -1,    -1,   230,    -1,  1330,   233,   234,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   445,    -1,   447,
     448,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1370,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1024,    -1,    -1,
      -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1403,    -1,    -1,    -1,   512,    -1,    -1,    -1,  1411,   517,
    1413,   519,    -1,    -1,   319,    -1,    -1,    -1,    -1,    -1,
      -1,   191,   192,    -1,    -1,  1072,    -1,  1712,  1075,   334,
      -1,   539,    -1,   541,   542,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1735,   559,    -1,   223,    -1,    -1,  1459,  1460,    -1,    -1,
     230,    -1,    -1,   571,    -1,    -1,    -1,    -1,  1753,    -1,
      -1,  1474,  1475,    -1,  1477,    -1,    -1,    -1,    -1,    -1,
      55,    56,    -1,  1486,    -1,   593,    -1,   595,   596,    -1,
      -1,    -1,    -1,  1496,  1497,  1780,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   617,
     618,    -1,    -1,    -1,  1799,    90,   624,  1802,    -1,    -1,
      -1,    -1,    -1,   428,    -1,    -1,    -1,    -1,   298,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1195,   657,
     658,   321,   322,    -1,    -1,    -1,  1203,  1204,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   140,    -1,    -1,   143,    -1,
      -1,    -1,   342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     495,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1602,
    1603,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1893,  1612,
      -1,    -1,    -1,  1260,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1269,    -1,    -1,  1272,    -1,  1274,  1275,    -1,
      -1,    -1,    -1,    -1,    -1,   210,    -1,    -1,    -1,    -1,
      -1,    -1,   412,    -1,  1647,  1648,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   428,   429,
      -1,   431,   432,    -1,    -1,    -1,    -1,  1314,    -1,   439,
      -1,    -1,    -1,   443,    -1,    -1,    -1,    -1,    -1,    -1,
     255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   276,    -1,    -1,   609,   610,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   484,    -1,    -1,   622,   488,    -1,
      -1,    -1,    -1,    -1,  1727,    -1,    -1,    -1,   303,    -1,
      -1,    -1,    -1,    -1,  1381,   310,   311,    -1,    -1,    -1,
     315,    -1,  1745,   851,    -1,  1748,  1749,    -1,    -1,    -1,
     520,    -1,  1755,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   347,    -1,    -1,   883,    -1,   352,    -1,    -1,
     355,    -1,    -1,    -1,    -1,    -1,   894,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   903,    -1,    -1,    -1,    -1,
     570,    -1,    -1,   573,    -1,    -1,    -1,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,  1463,    -1,    -1,    -1,
     590,   591,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,   601,    -1,   738,   739,   605,    -1,    -1,    -1,   744,
      -1,    -1,   612,    -1,   614,  1492,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     765,    -1,    -1,   768,   769,  1868,   771,   442,   773,   774,
      -1,  1518,    -1,    -1,   147,    -1,    -1,  1524,   151,   454,
     455,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
      -1,    -1,    -1,  1001,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   179,   812,    -1,    -1,
      -1,   816,    -1,    -1,    -1,  1918,    -1,  1025,    -1,   192,
      -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1942,
      -1,    -1,  1589,    -1,    -1,    -1,  1949,    -1,   718,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1964,    -1,    -1,   734,   735,    -1,    -1,    -1,    -1,
      -1,    -1,   245,    -1,   744,   745,    -1,   747,   748,    -1,
      -1,    -1,    -1,    -1,    -1,   258,    -1,   892,    -1,   759,
      -1,    -1,   762,    -1,   764,   765,    -1,    -1,    -1,    -1,
      -1,   771,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     585,   781,   782,    -1,    -1,    -1,  1663,  1664,    -1,    -1,
      -1,   294,    -1,    -1,  1671,    -1,    -1,    -1,  1675,   302,
     800,    -1,    -1,    -1,   804,    -1,    -1,    -1,   808,    -1,
      -1,    -1,   812,   813,    -1,    -1,   816,   817,   321,    -1,
     323,    -1,    -1,  1161,   824,    -1,    -1,    -1,   633,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1193,    -1,    -1,    -1,    -1,
      -1,  1199,    -1,    -1,    -1,    -1,   866,   867,    -1,    -1,
      -1,    -1,   375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1024,
      -1,  1768,    -1,    -1,   894,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    17,   409,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   934,    -1,   439,  1072,    50,    51,
    1075,    -1,   445,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,  1832,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   776,    -1,    -1,    -1,    -1,    -1,    -1,   783,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,  1332,    -1,    -1,    -1,    -1,    -1,
      -1,  1001,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   514,   515,    -1,    -1,    -1,    -1,   520,  1018,  1019,
      -1,    -1,    -1,    -1,    -1,  1025,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,  1380,  1920,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   858,    -1,    -1,    -1,    -1,    -1,    -1,
    1195,    -1,    -1,    -1,    -1,    -1,    -1,  1067,  1203,  1204,
     573,    -1,  1072,  1073,    -1,  1075,  1076,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   591,    -1,
     593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,   914,
      -1,   614,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1260,    -1,    -1,    -1,    -1,
      -1,    -1,   635,    -1,  1269,    -1,    -1,  1272,    -1,  1274,
    1275,    -1,    -1,    -1,    -1,   648,    -1,   650,   651,    -1,
     653,    -1,    -1,    -1,    -1,    -1,   659,    -1,    -1,   662,
     663,   664,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1314,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1189,
      -1,    -1,    -1,   179,    -1,  1195,  1196,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,
    1548,    -1,    -1,    -1,    -1,   718,  1216,    -1,    -1,   205,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   742,
      -1,    -1,    -1,    -1,    -1,    -1,  1381,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   759,    -1,    -1,    -1,
    1260,  1261,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1269,
    1270,    -1,  1272,    -1,    -1,    -1,    -1,    -1,   781,   782,
      -1,    -1,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   800,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   293,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1120,  1121,  1122,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1463,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1674,    -1,    -1,     5,
      -1,    -1,    -1,    -1,    -1,  1150,    12,    13,    14,    15,
      16,    -1,    -1,  1691,    -1,    -1,    -1,  1492,    -1,    -1,
    1165,    -1,    -1,   866,    -1,    -1,    -1,    -1,    -1,    -1,
     873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     883,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1202,    -1,  1399,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1766,    -1,
      -1,   934,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,  1589,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   465,
      -1,    -1,    -1,   149,    -1,   471,   152,   153,    -1,    -1,
     476,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1508,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1524,    -1,    -1,    -1,  1663,  1664,
    1335,    -1,  1870,  1338,    -1,    -1,    -1,    -1,    -1,    -1,
    1675,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1067,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   563,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1107,   591,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1953,    -1,    -1,   604,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1768,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1641,  1642,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1656,  1657,  1161,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   655,
      -1,    -1,  1672,    -1,    -1,    -1,    -1,    -1,    -1,  1182,
      -1,    -1,    -1,    -1,    -1,    -1,  1189,    -1,    -1,    -1,
     676,   677,    -1,    -1,   680,    -1,   682,  1832,    -1,    -1,
      -1,    -1,   688,    -1,   690,   691,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1216,    -1,    -1,    -1,  1220,    -1,    -1,
      -1,    -1,  1527,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   731,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   742,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1766,    -1,    -1,    -1,
     756,    -1,    -1,   759,  1774,    -1,    -1,    -1,    -1,    -1,
    1283,  1284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     786,    -1,    -1,   789,  1609,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1321,  1322,
    1323,    -1,  1325,  1326,    -1,    -1,    -1,    -1,  1828,  1332,
      -1,    -1,  1832,  1833,    -1,    -1,  1836,    -1,    -1,   825,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1869,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1380,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   873,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1399,   883,   884,    -1,
      -1,    -1,    -1,    -1,    -1,   891,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1724,
    1920,  1921,    -1,    -1,    -1,    -1,   912,    -1,  1733,    -1,
      -1,    -1,    -1,    -1,    -1,   921,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,   934,    -1,
      -1,    -1,    -1,  1953,    -1,    -1,   942,    -1,    -1,    -1,
      17,    -1,    -1,   949,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   995,
      -1,  1514,    69,    -1,    71,    72,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
      -1,    98,    99,    -1,   101,  1548,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,  1062,    -1,  1064,    -1,
    1066,    -1,    -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,
      -1,   148,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,  1616,    -1,    -1,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1641,  1642,
      -1,    -1,    -1,    -1,    -1,    -1,  1132,  1133,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    -1,  1691,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1193,    -1,    -1,
      -1,    -1,    -1,  1199,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1233,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1248,    -1,    -1,  1251,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
    1803,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1302,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,  1828,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1340,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1867,    -1,    -1,  1870,    -1,    -1,
      -1,    -1,  1358,    -1,    -1,  1361,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1399,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1409,  1410,    -1,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1434,    -1,
    1436,    69,    -1,    71,    72,    -1,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    -1,    96,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1514,    -1,
     148,   149,     1,  1519,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,  1575,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    71,    72,    -1,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,  1614,    98,
      99,  1617,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    69,    70,    71,    72,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,  1811,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    48,    -1,    50,    51,    52,
      -1,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    67,    -1,    69,    70,    71,    72,
      -1,    74,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,   100,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   174,     3,     4,     5,     6,     7,     8,     9,    10,
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
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   173,   174,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    48,
      -1,    50,    51,    52,    -1,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,   100,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   173,   174,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    48,    -1,    50,    51,    52,    -1,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      67,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    98,    99,   100,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,     3,
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
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    55,    56,    57,    58,    59,    60,
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
     161,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    55,    56,    57,    58,    59,
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
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,   100,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    70,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,   101,
      50,    51,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    67,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,   152,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
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
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,    -1,   160,   161,     4,     5,     6,
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
      -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    48,   152,   153,    -1,    52,    -1,
      54,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    -1,    -1,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    71,    -1,    -1,    74,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     4,
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
      -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,   152,   153,    -1,
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
     129,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    19,    70,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
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
     151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
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
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
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
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    12,    13,    14,    15,    16,   160,
     161,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
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
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    12,    13,    14,    15,    16,
      17,    70,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    12,    13,    14,
      15,    16,    17,    70,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,   152,    -1,    50,    51,   104,   105,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    76,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,   153,    -1,    -1,   104,
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
      -1,   160,   161,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    17,
      70,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,   104,   105,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    12,    13,    14,    15,
      16,    17,    70,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,   152,    -1,    50,    51,   104,   105,    -1,    55,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
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
      -1,   152,   153,   104,   105,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,   152,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,   104,   105,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,   152,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,   152,    49,    50,    51,    -1,
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
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
     151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
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
      71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,   156,    -1,    -1,   159,   160,
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
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
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
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    19,    70,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70
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
     191,   131,   165,   177,   177,   148,   361,   214,   150,   214,
     347,   177,   173,   149,   402,   439,   440,   149,   402,   439,
     440,   176,   176,   401,   151,   177,   177,   389,   387,   225,
     389,   331,   331,   331,     3,     9,    71,   148,   278,   285,
     286,   292,   295,   332,   337,   456,   151,   155,   155,   174,
     149,    59,    60,   174,   225,   277,   398,   149,    17,   223,
     149,   149,   174,   357,   174,   357,   160,   357,   157,   222,
     149,   149,   149,   225,   214,   215,   215,    13,   264,    72,
     231,   174,   177,   227,    76,   174,   357,    89,   250,   356,
     295,   156,   276,   174,   154,   154,   177,   155,   389,   399,
     177,   174,   177,   174,   177,   151,   359,   373,   373,   176,
     177,   177,   177,   214,   177,   149,   402,   445,   440,   294,
       5,   160,   177,   214,   345,   402,   402,   317,   346,   462,
     148,   148,   176,   151,   180,    76,   185,   186,   358,   198,
     198,   198,   198,   198,   157,   362,   155,   148,   194,   153,
     192,   194,   194,   154,   155,   120,   152,   190,   154,   220,
     212,   174,   154,   462,   177,   149,   402,   439,   440,   350,
     350,   177,   177,   151,   149,   402,   439,   440,   149,   402,
     445,   408,   402,   402,   350,   350,   154,   349,   352,   352,
     353,   151,   155,   155,   151,   177,   213,   213,   154,   154,
     177,   177,   151,   214,   176,   176,   350,   350,   360,   402,
     155,   151,   148,   389,   148,   148,   148,   148,   292,   330,
     338,   456,   292,   337,   149,   326,   174,   174,   149,   156,
     196,   333,   334,   340,   408,   409,   422,   155,   174,   357,
     176,   357,   151,   188,   189,   174,   225,   174,   225,   221,
      78,   151,   221,   232,   277,   279,   282,   288,   295,   299,
     151,   173,   174,   221,   241,   242,   277,   174,   174,   221,
     174,   362,   174,   221,   220,   221,   108,   109,   110,   111,
     112,   256,   258,   259,   174,    95,   174,    82,   149,   149,
     177,   148,   174,   174,   149,   223,   225,   402,   174,   151,
     176,   148,   148,   176,   155,   155,   154,   154,   154,   177,
     151,   176,   214,   214,   177,   154,   177,   462,   343,   157,
     346,   148,   381,   151,   156,   151,   155,   156,   362,   462,
     220,   118,   191,   192,   153,   192,   153,   192,   154,   148,
     151,   176,   177,   177,   151,   151,   176,   176,   177,   177,
     177,   176,   176,   154,   177,   151,   402,   350,   350,   177,
     177,   221,   148,   326,   326,   326,   149,   196,   335,   336,
     439,   447,   448,   449,   450,   174,   155,   174,   333,   174,
     376,   403,   408,   214,   295,   155,   174,   339,   340,   339,
     357,   131,   354,   355,   221,   151,   151,   149,   223,   151,
     221,   295,   223,   221,   222,   143,   144,   145,   165,   174,
     243,   151,   156,   222,   174,   462,   151,   151,   151,   225,
     258,   259,   149,   214,   149,   182,   232,   198,   251,   107,
       1,   223,   402,   382,   176,   176,   154,   350,   177,   177,
     154,   154,   148,   157,   345,   177,   214,   186,   214,   462,
     148,   154,   154,   191,   191,   350,   151,   151,   350,   350,
     151,   151,   154,   155,   131,   349,   131,   154,   177,   177,
     151,   151,   154,   448,   449,   450,   295,   447,   155,   174,
     402,   402,   174,   151,   408,   402,   174,   223,    75,    76,
     157,   235,   236,   237,   151,   221,    73,   223,    73,   174,
     104,   173,   221,   222,   221,   223,   242,   174,   148,   157,
     237,   223,   149,   176,   174,   182,   151,   156,   151,   151,
     155,   156,   249,   253,   357,   399,   177,   154,   154,   345,
     462,   148,   148,   154,   154,   177,   177,   177,   176,   177,
     151,   151,   151,   151,   151,   447,   402,   334,     1,   213,
     233,   234,   400,     1,   156,     1,   176,   223,   235,    73,
     174,   151,   223,    73,   223,   222,   221,   144,   165,   243,
     174,   165,    73,   222,   174,     1,   176,   176,   260,   293,
     295,   456,   156,   174,   153,   182,   265,   266,   267,   223,
     198,   188,    73,   106,   250,   252,   151,   462,   148,   151,
     151,   151,   352,   149,   402,   439,   440,   336,   131,     1,
     155,   156,   148,   270,   271,   277,   223,    73,   174,   223,
     150,   150,   221,   222,   221,   223,   148,   270,   260,   177,
     149,   196,   399,   447,   180,   156,   101,   149,   151,   156,
     155,    73,   151,   223,   149,   223,   223,   148,   176,   213,
     233,   236,   238,   239,   277,   223,   165,   165,   165,   238,
     177,   174,   257,   295,   265,   154,   213,   174,   265,   267,
     223,   221,   107,   107,   350,   223,   228,   177,   236,   221,
     150,   221,   221,   177,   257,   212,   151,   156,   182,   151,
     151,   156,   151,   253,    73,   248,   177,     1,   223,   148,
     228,   148,   151,   225,   182,   268,   149,   174,   268,   223,
      73,   151,   225,   155,   156,   213,   151,   223,   182,   180,
     269,   151,   174,   151,   155,   174,   180
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
       0,     4,     3,     4,     4,     4,     4,     5,     5,     5,
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
#line 529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7038 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7044 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7050 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7056 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7062 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7068 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7074 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7080 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7086 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7092 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7102 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7108 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7114 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7120 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7126 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7132 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7138 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7144 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7154 "Parser/parser.cc"
    break;

  case 32:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Adjacent identifiers are not meaningful in an expression. "
											   "Possible problem is identifier \"", *(yyvsp[-1].tok).str,
											   "\" is a misspelled typename or an incorrectly specified type name, "
											   "e.g., missing generic parameter or missing struct/union/enum before typename." ) );
			(yyval.en) = nullptr;
 		}
#line 7166 "Parser/parser.cc"
    break;

  case 33:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7176 "Parser/parser.cc"
    break;

  case 35:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7187 "Parser/parser.cc"
    break;

  case 36:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7196 "Parser/parser.cc"
    break;

  case 37:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7202 "Parser/parser.cc"
    break;

  case 39:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7208 "Parser/parser.cc"
    break;

  case 40:
#line 661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7214 "Parser/parser.cc"
    break;

  case 41:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7220 "Parser/parser.cc"
    break;

  case 42:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7226 "Parser/parser.cc"
    break;

  case 43:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7236 "Parser/parser.cc"
    break;

  case 44:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7242 "Parser/parser.cc"
    break;

  case 45:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7248 "Parser/parser.cc"
    break;

  case 46:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7254 "Parser/parser.cc"
    break;

  case 47:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7260 "Parser/parser.cc"
    break;

  case 48:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7266 "Parser/parser.cc"
    break;

  case 49:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7272 "Parser/parser.cc"
    break;

  case 50:
#line 685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7278 "Parser/parser.cc"
    break;

  case 51:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7284 "Parser/parser.cc"
    break;

  case 52:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7290 "Parser/parser.cc"
    break;

  case 53:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7296 "Parser/parser.cc"
    break;

  case 54:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7302 "Parser/parser.cc"
    break;

  case 55:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 56:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7314 "Parser/parser.cc"
    break;

  case 57:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 58:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7326 "Parser/parser.cc"
    break;

  case 59:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7332 "Parser/parser.cc"
    break;

  case 60:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7342 "Parser/parser.cc"
    break;

  case 61:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7348 "Parser/parser.cc"
    break;

  case 64:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7354 "Parser/parser.cc"
    break;

  case 65:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7360 "Parser/parser.cc"
    break;

  case 68:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7366 "Parser/parser.cc"
    break;

  case 70:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7372 "Parser/parser.cc"
    break;

  case 71:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7378 "Parser/parser.cc"
    break;

  case 72:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7384 "Parser/parser.cc"
    break;

  case 73:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7390 "Parser/parser.cc"
    break;

  case 74:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7396 "Parser/parser.cc"
    break;

  case 75:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7402 "Parser/parser.cc"
    break;

  case 76:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7408 "Parser/parser.cc"
    break;

  case 77:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7414 "Parser/parser.cc"
    break;

  case 78:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7422 "Parser/parser.cc"
    break;

  case 79:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7428 "Parser/parser.cc"
    break;

  case 80:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7437 "Parser/parser.cc"
    break;

  case 83:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7443 "Parser/parser.cc"
    break;

  case 84:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7449 "Parser/parser.cc"
    break;

  case 85:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7469 "Parser/parser.cc"
    break;

  case 86:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7475 "Parser/parser.cc"
    break;

  case 87:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7481 "Parser/parser.cc"
    break;

  case 88:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7487 "Parser/parser.cc"
    break;

  case 89:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7493 "Parser/parser.cc"
    break;

  case 90:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7499 "Parser/parser.cc"
    break;

  case 91:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7505 "Parser/parser.cc"
    break;

  case 92:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7511 "Parser/parser.cc"
    break;

  case 93:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7517 "Parser/parser.cc"
    break;

  case 94:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7526 "Parser/parser.cc"
    break;

  case 95:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7532 "Parser/parser.cc"
    break;

  case 96:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7538 "Parser/parser.cc"
    break;

  case 97:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7544 "Parser/parser.cc"
    break;

  case 98:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7550 "Parser/parser.cc"
    break;

  case 99:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7556 "Parser/parser.cc"
    break;

  case 100:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7562 "Parser/parser.cc"
    break;

  case 101:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7568 "Parser/parser.cc"
    break;

  case 103:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7574 "Parser/parser.cc"
    break;

  case 104:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7580 "Parser/parser.cc"
    break;

  case 105:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7586 "Parser/parser.cc"
    break;

  case 106:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7592 "Parser/parser.cc"
    break;

  case 107:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7598 "Parser/parser.cc"
    break;

  case 108:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7604 "Parser/parser.cc"
    break;

  case 109:
#line 853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7610 "Parser/parser.cc"
    break;

  case 110:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7616 "Parser/parser.cc"
    break;

  case 118:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7622 "Parser/parser.cc"
    break;

  case 120:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7628 "Parser/parser.cc"
    break;

  case 121:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7634 "Parser/parser.cc"
    break;

  case 122:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7640 "Parser/parser.cc"
    break;

  case 124:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7646 "Parser/parser.cc"
    break;

  case 125:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7652 "Parser/parser.cc"
    break;

  case 127:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7658 "Parser/parser.cc"
    break;

  case 128:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7664 "Parser/parser.cc"
    break;

  case 130:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7670 "Parser/parser.cc"
    break;

  case 131:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7676 "Parser/parser.cc"
    break;

  case 132:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7682 "Parser/parser.cc"
    break;

  case 133:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7688 "Parser/parser.cc"
    break;

  case 135:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7694 "Parser/parser.cc"
    break;

  case 136:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7700 "Parser/parser.cc"
    break;

  case 138:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7706 "Parser/parser.cc"
    break;

  case 140:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7712 "Parser/parser.cc"
    break;

  case 142:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 144:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7724 "Parser/parser.cc"
    break;

  case 146:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7730 "Parser/parser.cc"
    break;

  case 148:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7736 "Parser/parser.cc"
    break;

  case 149:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7742 "Parser/parser.cc"
    break;

  case 152:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7754 "Parser/parser.cc"
    break;

  case 153:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7760 "Parser/parser.cc"
    break;

  case 154:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7766 "Parser/parser.cc"
    break;

  case 158:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7772 "Parser/parser.cc"
    break;

  case 159:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7778 "Parser/parser.cc"
    break;

  case 160:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7784 "Parser/parser.cc"
    break;

  case 161:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7790 "Parser/parser.cc"
    break;

  case 162:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7796 "Parser/parser.cc"
    break;

  case 163:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7802 "Parser/parser.cc"
    break;

  case 164:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7808 "Parser/parser.cc"
    break;

  case 165:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7814 "Parser/parser.cc"
    break;

  case 166:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7820 "Parser/parser.cc"
    break;

  case 167:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7826 "Parser/parser.cc"
    break;

  case 168:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7832 "Parser/parser.cc"
    break;

  case 169:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7838 "Parser/parser.cc"
    break;

  case 170:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7844 "Parser/parser.cc"
    break;

  case 171:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7850 "Parser/parser.cc"
    break;

  case 172:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7856 "Parser/parser.cc"
    break;

  case 174:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7862 "Parser/parser.cc"
    break;

  case 175:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7868 "Parser/parser.cc"
    break;

  case 176:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7874 "Parser/parser.cc"
    break;

  case 178:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7880 "Parser/parser.cc"
    break;

  case 179:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7886 "Parser/parser.cc"
    break;

  case 191:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7892 "Parser/parser.cc"
    break;

  case 193:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7898 "Parser/parser.cc"
    break;

  case 194:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7904 "Parser/parser.cc"
    break;

  case 195:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 7915 "Parser/parser.cc"
    break;

  case 196:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7921 "Parser/parser.cc"
    break;

  case 197:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7927 "Parser/parser.cc"
    break;

  case 199:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7933 "Parser/parser.cc"
    break;

  case 200:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7939 "Parser/parser.cc"
    break;

  case 201:
#line 1100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 202:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 203:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7957 "Parser/parser.cc"
    break;

  case 206:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7963 "Parser/parser.cc"
    break;

  case 207:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 7969 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 209:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 210:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7987 "Parser/parser.cc"
    break;

  case 211:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 212:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8007 "Parser/parser.cc"
    break;

  case 213:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8013 "Parser/parser.cc"
    break;

  case 214:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8019 "Parser/parser.cc"
    break;

  case 215:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8028 "Parser/parser.cc"
    break;

  case 216:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8034 "Parser/parser.cc"
    break;

  case 217:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 218:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8046 "Parser/parser.cc"
    break;

  case 219:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8052 "Parser/parser.cc"
    break;

  case 220:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8058 "Parser/parser.cc"
    break;

  case 221:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8064 "Parser/parser.cc"
    break;

  case 222:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8070 "Parser/parser.cc"
    break;

  case 223:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8076 "Parser/parser.cc"
    break;

  case 224:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8082 "Parser/parser.cc"
    break;

  case 226:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8088 "Parser/parser.cc"
    break;

  case 227:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8094 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8100 "Parser/parser.cc"
    break;

  case 229:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8106 "Parser/parser.cc"
    break;

  case 230:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8112 "Parser/parser.cc"
    break;

  case 231:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8118 "Parser/parser.cc"
    break;

  case 232:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8124 "Parser/parser.cc"
    break;

  case 234:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8130 "Parser/parser.cc"
    break;

  case 235:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8136 "Parser/parser.cc"
    break;

  case 236:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8142 "Parser/parser.cc"
    break;

  case 238:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8148 "Parser/parser.cc"
    break;

  case 239:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8154 "Parser/parser.cc"
    break;

  case 240:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8160 "Parser/parser.cc"
    break;

  case 241:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8169 "Parser/parser.cc"
    break;

  case 242:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8175 "Parser/parser.cc"
    break;

  case 243:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8181 "Parser/parser.cc"
    break;

  case 244:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8187 "Parser/parser.cc"
    break;

  case 245:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8196 "Parser/parser.cc"
    break;

  case 246:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 247:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8208 "Parser/parser.cc"
    break;

  case 248:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8214 "Parser/parser.cc"
    break;

  case 249:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 8223 "Parser/parser.cc"
    break;

  case 250:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8229 "Parser/parser.cc"
    break;

  case 251:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8235 "Parser/parser.cc"
    break;

  case 253:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8254 "Parser/parser.cc"
    break;

  case 254:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8260 "Parser/parser.cc"
    break;

  case 255:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8266 "Parser/parser.cc"
    break;

  case 256:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8272 "Parser/parser.cc"
    break;

  case 257:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8279 "Parser/parser.cc"
    break;

  case 258:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8286 "Parser/parser.cc"
    break;

  case 259:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8292 "Parser/parser.cc"
    break;

  case 260:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8298 "Parser/parser.cc"
    break;

  case 261:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8304 "Parser/parser.cc"
    break;

  case 262:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8311 "Parser/parser.cc"
    break;

  case 263:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 264:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 265:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8330 "Parser/parser.cc"
    break;

  case 266:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8339 "Parser/parser.cc"
    break;

  case 267:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8345 "Parser/parser.cc"
    break;

  case 268:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8351 "Parser/parser.cc"
    break;

  case 269:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8357 "Parser/parser.cc"
    break;

  case 270:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8363 "Parser/parser.cc"
    break;

  case 271:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8369 "Parser/parser.cc"
    break;

  case 272:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8375 "Parser/parser.cc"
    break;

  case 273:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8381 "Parser/parser.cc"
    break;

  case 274:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8387 "Parser/parser.cc"
    break;

  case 275:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8393 "Parser/parser.cc"
    break;

  case 276:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8399 "Parser/parser.cc"
    break;

  case 277:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8405 "Parser/parser.cc"
    break;

  case 278:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8411 "Parser/parser.cc"
    break;

  case 279:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8417 "Parser/parser.cc"
    break;

  case 280:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8423 "Parser/parser.cc"
    break;

  case 281:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8429 "Parser/parser.cc"
    break;

  case 282:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8435 "Parser/parser.cc"
    break;

  case 283:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8441 "Parser/parser.cc"
    break;

  case 284:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8447 "Parser/parser.cc"
    break;

  case 285:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8453 "Parser/parser.cc"
    break;

  case 286:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8459 "Parser/parser.cc"
    break;

  case 287:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8465 "Parser/parser.cc"
    break;

  case 288:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8471 "Parser/parser.cc"
    break;

  case 289:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8477 "Parser/parser.cc"
    break;

  case 290:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8483 "Parser/parser.cc"
    break;

  case 291:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8489 "Parser/parser.cc"
    break;

  case 292:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8495 "Parser/parser.cc"
    break;

  case 293:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8501 "Parser/parser.cc"
    break;

  case 294:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8507 "Parser/parser.cc"
    break;

  case 295:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8513 "Parser/parser.cc"
    break;

  case 298:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8519 "Parser/parser.cc"
    break;

  case 299:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 300:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8531 "Parser/parser.cc"
    break;

  case 301:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8537 "Parser/parser.cc"
    break;

  case 303:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8543 "Parser/parser.cc"
    break;

  case 304:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8549 "Parser/parser.cc"
    break;

  case 306:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8555 "Parser/parser.cc"
    break;

  case 307:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8561 "Parser/parser.cc"
    break;

  case 308:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8567 "Parser/parser.cc"
    break;

  case 309:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8573 "Parser/parser.cc"
    break;

  case 310:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8579 "Parser/parser.cc"
    break;

  case 311:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8585 "Parser/parser.cc"
    break;

  case 312:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8591 "Parser/parser.cc"
    break;

  case 313:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8597 "Parser/parser.cc"
    break;

  case 314:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8603 "Parser/parser.cc"
    break;

  case 315:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8609 "Parser/parser.cc"
    break;

  case 316:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8615 "Parser/parser.cc"
    break;

  case 317:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8621 "Parser/parser.cc"
    break;

  case 318:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8627 "Parser/parser.cc"
    break;

  case 319:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8633 "Parser/parser.cc"
    break;

  case 320:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8639 "Parser/parser.cc"
    break;

  case 321:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8645 "Parser/parser.cc"
    break;

  case 322:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8651 "Parser/parser.cc"
    break;

  case 323:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8657 "Parser/parser.cc"
    break;

  case 324:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8663 "Parser/parser.cc"
    break;

  case 325:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8669 "Parser/parser.cc"
    break;

  case 326:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8675 "Parser/parser.cc"
    break;

  case 327:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8681 "Parser/parser.cc"
    break;

  case 329:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8687 "Parser/parser.cc"
    break;

  case 330:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8693 "Parser/parser.cc"
    break;

  case 331:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8699 "Parser/parser.cc"
    break;

  case 336:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 337:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 338:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 339:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8723 "Parser/parser.cc"
    break;

  case 340:
#line 1527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8729 "Parser/parser.cc"
    break;

  case 341:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8735 "Parser/parser.cc"
    break;

  case 342:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8741 "Parser/parser.cc"
    break;

  case 343:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8747 "Parser/parser.cc"
    break;

  case 346:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8753 "Parser/parser.cc"
    break;

  case 347:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8759 "Parser/parser.cc"
    break;

  case 348:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8765 "Parser/parser.cc"
    break;

  case 349:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8771 "Parser/parser.cc"
    break;

  case 350:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8777 "Parser/parser.cc"
    break;

  case 351:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8783 "Parser/parser.cc"
    break;

  case 352:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8792 "Parser/parser.cc"
    break;

  case 353:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8801 "Parser/parser.cc"
    break;

  case 354:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8807 "Parser/parser.cc"
    break;

  case 357:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8813 "Parser/parser.cc"
    break;

  case 358:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8819 "Parser/parser.cc"
    break;

  case 360:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8825 "Parser/parser.cc"
    break;

  case 361:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8831 "Parser/parser.cc"
    break;

  case 371:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8837 "Parser/parser.cc"
    break;

  case 372:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8843 "Parser/parser.cc"
    break;

  case 376:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8849 "Parser/parser.cc"
    break;

  case 378:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8855 "Parser/parser.cc"
    break;

  case 379:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8861 "Parser/parser.cc"
    break;

  case 380:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8867 "Parser/parser.cc"
    break;

  case 381:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8873 "Parser/parser.cc"
    break;

  case 382:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8879 "Parser/parser.cc"
    break;

  case 383:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8885 "Parser/parser.cc"
    break;

  case 385:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8891 "Parser/parser.cc"
    break;

  case 386:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8897 "Parser/parser.cc"
    break;

  case 387:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8903 "Parser/parser.cc"
    break;

  case 388:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8914 "Parser/parser.cc"
    break;

  case 389:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8920 "Parser/parser.cc"
    break;

  case 390:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8926 "Parser/parser.cc"
    break;

  case 391:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8932 "Parser/parser.cc"
    break;

  case 392:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 393:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8947 "Parser/parser.cc"
    break;

  case 394:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8956 "Parser/parser.cc"
    break;

  case 395:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8965 "Parser/parser.cc"
    break;

  case 396:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8974 "Parser/parser.cc"
    break;

  case 397:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8983 "Parser/parser.cc"
    break;

  case 398:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8992 "Parser/parser.cc"
    break;

  case 399:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9001 "Parser/parser.cc"
    break;

  case 400:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9010 "Parser/parser.cc"
    break;

  case 401:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9018 "Parser/parser.cc"
    break;

  case 402:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9026 "Parser/parser.cc"
    break;

  case 403:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9032 "Parser/parser.cc"
    break;

  case 407:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 408:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9044 "Parser/parser.cc"
    break;

  case 421:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9050 "Parser/parser.cc"
    break;

  case 424:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9056 "Parser/parser.cc"
    break;

  case 427:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9062 "Parser/parser.cc"
    break;

  case 428:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9068 "Parser/parser.cc"
    break;

  case 429:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9074 "Parser/parser.cc"
    break;

  case 430:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9080 "Parser/parser.cc"
    break;

  case 432:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9086 "Parser/parser.cc"
    break;

  case 434:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9092 "Parser/parser.cc"
    break;

  case 435:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9098 "Parser/parser.cc"
    break;

  case 437:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9104 "Parser/parser.cc"
    break;

  case 438:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9110 "Parser/parser.cc"
    break;

  case 439:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9116 "Parser/parser.cc"
    break;

  case 440:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9122 "Parser/parser.cc"
    break;

  case 441:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9128 "Parser/parser.cc"
    break;

  case 442:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9134 "Parser/parser.cc"
    break;

  case 443:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9140 "Parser/parser.cc"
    break;

  case 444:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9146 "Parser/parser.cc"
    break;

  case 445:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9152 "Parser/parser.cc"
    break;

  case 446:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9158 "Parser/parser.cc"
    break;

  case 447:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9164 "Parser/parser.cc"
    break;

  case 448:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9170 "Parser/parser.cc"
    break;

  case 449:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9176 "Parser/parser.cc"
    break;

  case 450:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9182 "Parser/parser.cc"
    break;

  case 451:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 452:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9194 "Parser/parser.cc"
    break;

  case 453:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9200 "Parser/parser.cc"
    break;

  case 454:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9206 "Parser/parser.cc"
    break;

  case 455:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9212 "Parser/parser.cc"
    break;

  case 456:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9218 "Parser/parser.cc"
    break;

  case 457:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9224 "Parser/parser.cc"
    break;

  case 458:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9230 "Parser/parser.cc"
    break;

  case 459:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9236 "Parser/parser.cc"
    break;

  case 460:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9242 "Parser/parser.cc"
    break;

  case 461:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9248 "Parser/parser.cc"
    break;

  case 462:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9254 "Parser/parser.cc"
    break;

  case 463:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9260 "Parser/parser.cc"
    break;

  case 464:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9266 "Parser/parser.cc"
    break;

  case 465:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9272 "Parser/parser.cc"
    break;

  case 466:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9278 "Parser/parser.cc"
    break;

  case 467:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9284 "Parser/parser.cc"
    break;

  case 468:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9290 "Parser/parser.cc"
    break;

  case 469:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9296 "Parser/parser.cc"
    break;

  case 470:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9302 "Parser/parser.cc"
    break;

  case 471:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9308 "Parser/parser.cc"
    break;

  case 472:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9314 "Parser/parser.cc"
    break;

  case 474:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9320 "Parser/parser.cc"
    break;

  case 476:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9326 "Parser/parser.cc"
    break;

  case 477:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9332 "Parser/parser.cc"
    break;

  case 478:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9338 "Parser/parser.cc"
    break;

  case 480:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9344 "Parser/parser.cc"
    break;

  case 481:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9350 "Parser/parser.cc"
    break;

  case 482:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9356 "Parser/parser.cc"
    break;

  case 483:
#line 2006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 485:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 487:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9374 "Parser/parser.cc"
    break;

  case 488:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9380 "Parser/parser.cc"
    break;

  case 489:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9386 "Parser/parser.cc"
    break;

  case 490:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9392 "Parser/parser.cc"
    break;

  case 491:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9398 "Parser/parser.cc"
    break;

  case 492:
#line 2032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9404 "Parser/parser.cc"
    break;

  case 493:
#line 2034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9410 "Parser/parser.cc"
    break;

  case 494:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9416 "Parser/parser.cc"
    break;

  case 495:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9422 "Parser/parser.cc"
    break;

  case 497:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9428 "Parser/parser.cc"
    break;

  case 498:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9434 "Parser/parser.cc"
    break;

  case 499:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9440 "Parser/parser.cc"
    break;

  case 501:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9446 "Parser/parser.cc"
    break;

  case 502:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9452 "Parser/parser.cc"
    break;

  case 503:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9461 "Parser/parser.cc"
    break;

  case 505:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9467 "Parser/parser.cc"
    break;

  case 506:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9473 "Parser/parser.cc"
    break;

  case 507:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9479 "Parser/parser.cc"
    break;

  case 509:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9485 "Parser/parser.cc"
    break;

  case 510:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9491 "Parser/parser.cc"
    break;

  case 512:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9497 "Parser/parser.cc"
    break;

  case 513:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9503 "Parser/parser.cc"
    break;

  case 514:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 516:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9515 "Parser/parser.cc"
    break;

  case 517:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9521 "Parser/parser.cc"
    break;

  case 518:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9527 "Parser/parser.cc"
    break;

  case 519:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9533 "Parser/parser.cc"
    break;

  case 520:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9539 "Parser/parser.cc"
    break;

  case 522:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9545 "Parser/parser.cc"
    break;

  case 523:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9551 "Parser/parser.cc"
    break;

  case 524:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9557 "Parser/parser.cc"
    break;

  case 525:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9563 "Parser/parser.cc"
    break;

  case 526:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9569 "Parser/parser.cc"
    break;

  case 531:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9575 "Parser/parser.cc"
    break;

  case 532:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9581 "Parser/parser.cc"
    break;

  case 533:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9590 "Parser/parser.cc"
    break;

  case 534:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 535:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9605 "Parser/parser.cc"
    break;

  case 536:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9614 "Parser/parser.cc"
    break;

  case 537:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9623 "Parser/parser.cc"
    break;

  case 538:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9632 "Parser/parser.cc"
    break;

  case 540:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9638 "Parser/parser.cc"
    break;

  case 541:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9644 "Parser/parser.cc"
    break;

  case 542:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9654 "Parser/parser.cc"
    break;

  case 543:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9669 "Parser/parser.cc"
    break;

  case 546:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9675 "Parser/parser.cc"
    break;

  case 547:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9681 "Parser/parser.cc"
    break;

  case 548:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9687 "Parser/parser.cc"
    break;

  case 549:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9693 "Parser/parser.cc"
    break;

  case 550:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9699 "Parser/parser.cc"
    break;

  case 551:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9705 "Parser/parser.cc"
    break;

  case 552:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9711 "Parser/parser.cc"
    break;

  case 553:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9717 "Parser/parser.cc"
    break;

  case 554:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9723 "Parser/parser.cc"
    break;

  case 555:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9729 "Parser/parser.cc"
    break;

  case 556:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9735 "Parser/parser.cc"
    break;

  case 557:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9741 "Parser/parser.cc"
    break;

  case 558:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9747 "Parser/parser.cc"
    break;

  case 559:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9753 "Parser/parser.cc"
    break;

  case 560:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9759 "Parser/parser.cc"
    break;

  case 561:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9772 "Parser/parser.cc"
    break;

  case 562:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9778 "Parser/parser.cc"
    break;

  case 565:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9784 "Parser/parser.cc"
    break;

  case 566:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9790 "Parser/parser.cc"
    break;

  case 569:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9796 "Parser/parser.cc"
    break;

  case 571:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9802 "Parser/parser.cc"
    break;

  case 572:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9808 "Parser/parser.cc"
    break;

  case 573:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9814 "Parser/parser.cc"
    break;

  case 574:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9820 "Parser/parser.cc"
    break;

  case 575:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9826 "Parser/parser.cc"
    break;

  case 577:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9832 "Parser/parser.cc"
    break;

  case 579:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9838 "Parser/parser.cc"
    break;

  case 580:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9844 "Parser/parser.cc"
    break;

  case 582:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9850 "Parser/parser.cc"
    break;

  case 583:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9856 "Parser/parser.cc"
    break;

  case 585:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9862 "Parser/parser.cc"
    break;

  case 586:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9868 "Parser/parser.cc"
    break;

  case 587:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9874 "Parser/parser.cc"
    break;

  case 588:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9880 "Parser/parser.cc"
    break;

  case 589:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9886 "Parser/parser.cc"
    break;

  case 590:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) 
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 9897 "Parser/parser.cc"
    break;

  case 591:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9906 "Parser/parser.cc"
    break;

  case 592:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 9914 "Parser/parser.cc"
    break;

  case 593:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 9924 "Parser/parser.cc"
    break;

  case 595:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9930 "Parser/parser.cc"
    break;

  case 596:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9936 "Parser/parser.cc"
    break;

  case 597:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 9942 "Parser/parser.cc"
    break;

  case 598:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 9948 "Parser/parser.cc"
    break;

  case 599:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 9954 "Parser/parser.cc"
    break;

  case 600:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 9960 "Parser/parser.cc"
    break;

  case 601:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9966 "Parser/parser.cc"
    break;

  case 602:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9972 "Parser/parser.cc"
    break;

  case 603:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9978 "Parser/parser.cc"
    break;

  case 604:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9984 "Parser/parser.cc"
    break;

  case 607:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9990 "Parser/parser.cc"
    break;

  case 608:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9996 "Parser/parser.cc"
    break;

  case 609:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10002 "Parser/parser.cc"
    break;

  case 611:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10008 "Parser/parser.cc"
    break;

  case 612:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10014 "Parser/parser.cc"
    break;

  case 613:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10020 "Parser/parser.cc"
    break;

  case 615:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10026 "Parser/parser.cc"
    break;

  case 616:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10032 "Parser/parser.cc"
    break;

  case 617:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10038 "Parser/parser.cc"
    break;

  case 619:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10044 "Parser/parser.cc"
    break;

  case 622:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10050 "Parser/parser.cc"
    break;

  case 623:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10056 "Parser/parser.cc"
    break;

  case 625:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10062 "Parser/parser.cc"
    break;

  case 626:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10068 "Parser/parser.cc"
    break;

  case 627:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10074 "Parser/parser.cc"
    break;

  case 632:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10080 "Parser/parser.cc"
    break;

  case 634:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10086 "Parser/parser.cc"
    break;

  case 635:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10092 "Parser/parser.cc"
    break;

  case 636:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10098 "Parser/parser.cc"
    break;

  case 637:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10104 "Parser/parser.cc"
    break;

  case 638:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10110 "Parser/parser.cc"
    break;

  case 639:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10116 "Parser/parser.cc"
    break;

  case 645:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10122 "Parser/parser.cc"
    break;

  case 648:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10128 "Parser/parser.cc"
    break;

  case 649:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10134 "Parser/parser.cc"
    break;

  case 650:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10140 "Parser/parser.cc"
    break;

  case 651:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10146 "Parser/parser.cc"
    break;

  case 652:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10152 "Parser/parser.cc"
    break;

  case 653:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10158 "Parser/parser.cc"
    break;

  case 654:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10164 "Parser/parser.cc"
    break;

  case 656:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10170 "Parser/parser.cc"
    break;

  case 657:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10176 "Parser/parser.cc"
    break;

  case 658:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10182 "Parser/parser.cc"
    break;

  case 660:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10188 "Parser/parser.cc"
    break;

  case 662:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10194 "Parser/parser.cc"
    break;

  case 663:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10200 "Parser/parser.cc"
    break;

  case 664:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10206 "Parser/parser.cc"
    break;

  case 665:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10212 "Parser/parser.cc"
    break;

  case 666:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10218 "Parser/parser.cc"
    break;

  case 667:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10224 "Parser/parser.cc"
    break;

  case 669:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10230 "Parser/parser.cc"
    break;

  case 670:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10236 "Parser/parser.cc"
    break;

  case 671:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10242 "Parser/parser.cc"
    break;

  case 672:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10253 "Parser/parser.cc"
    break;

  case 673:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10259 "Parser/parser.cc"
    break;

  case 674:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10265 "Parser/parser.cc"
    break;

  case 675:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10271 "Parser/parser.cc"
    break;

  case 676:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10280 "Parser/parser.cc"
    break;

  case 677:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10286 "Parser/parser.cc"
    break;

  case 678:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10292 "Parser/parser.cc"
    break;

  case 679:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10298 "Parser/parser.cc"
    break;

  case 680:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10304 "Parser/parser.cc"
    break;

  case 681:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10310 "Parser/parser.cc"
    break;

  case 682:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10316 "Parser/parser.cc"
    break;

  case 683:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10322 "Parser/parser.cc"
    break;

  case 684:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10328 "Parser/parser.cc"
    break;

  case 685:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10334 "Parser/parser.cc"
    break;

  case 686:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10340 "Parser/parser.cc"
    break;

  case 689:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10346 "Parser/parser.cc"
    break;

  case 690:
#line 2655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10352 "Parser/parser.cc"
    break;

  case 691:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10358 "Parser/parser.cc"
    break;

  case 692:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10364 "Parser/parser.cc"
    break;

  case 694:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10370 "Parser/parser.cc"
    break;

  case 695:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10376 "Parser/parser.cc"
    break;

  case 696:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10382 "Parser/parser.cc"
    break;

  case 697:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10388 "Parser/parser.cc"
    break;

  case 698:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10394 "Parser/parser.cc"
    break;

  case 699:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10400 "Parser/parser.cc"
    break;

  case 700:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10406 "Parser/parser.cc"
    break;

  case 701:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10415 "Parser/parser.cc"
    break;

  case 702:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10424 "Parser/parser.cc"
    break;

  case 703:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10430 "Parser/parser.cc"
    break;

  case 704:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10436 "Parser/parser.cc"
    break;

  case 706:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10442 "Parser/parser.cc"
    break;

  case 711:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10448 "Parser/parser.cc"
    break;

  case 712:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10454 "Parser/parser.cc"
    break;

  case 713:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10460 "Parser/parser.cc"
    break;

  case 715:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10466 "Parser/parser.cc"
    break;

  case 716:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10472 "Parser/parser.cc"
    break;

  case 717:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10478 "Parser/parser.cc"
    break;

  case 718:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10484 "Parser/parser.cc"
    break;

  case 720:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10490 "Parser/parser.cc"
    break;

  case 721:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10496 "Parser/parser.cc"
    break;

  case 722:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10502 "Parser/parser.cc"
    break;

  case 725:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10511 "Parser/parser.cc"
    break;

  case 726:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 727:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10526 "Parser/parser.cc"
    break;

  case 728:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10536 "Parser/parser.cc"
    break;

  case 729:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10545 "Parser/parser.cc"
    break;

  case 730:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10555 "Parser/parser.cc"
    break;

  case 731:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10564 "Parser/parser.cc"
    break;

  case 732:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10574 "Parser/parser.cc"
    break;

  case 733:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10583 "Parser/parser.cc"
    break;

  case 734:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10593 "Parser/parser.cc"
    break;

  case 735:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10602 "Parser/parser.cc"
    break;

  case 736:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10612 "Parser/parser.cc"
    break;

  case 738:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 739:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 740:
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10630 "Parser/parser.cc"
    break;

  case 741:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10636 "Parser/parser.cc"
    break;

  case 742:
#line 2853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10647 "Parser/parser.cc"
    break;

  case 743:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10656 "Parser/parser.cc"
    break;

  case 744:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10665 "Parser/parser.cc"
    break;

  case 745:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 746:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10677 "Parser/parser.cc"
    break;

  case 747:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10683 "Parser/parser.cc"
    break;

  case 748:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10692 "Parser/parser.cc"
    break;

  case 749:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 750:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 751:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10710 "Parser/parser.cc"
    break;

  case 755:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10716 "Parser/parser.cc"
    break;

  case 756:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10722 "Parser/parser.cc"
    break;

  case 757:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10732 "Parser/parser.cc"
    break;

  case 758:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10738 "Parser/parser.cc"
    break;

  case 761:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10744 "Parser/parser.cc"
    break;

  case 762:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10750 "Parser/parser.cc"
    break;

  case 764:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10756 "Parser/parser.cc"
    break;

  case 765:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10762 "Parser/parser.cc"
    break;

  case 766:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10768 "Parser/parser.cc"
    break;

  case 767:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10774 "Parser/parser.cc"
    break;

  case 772:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10780 "Parser/parser.cc"
    break;

  case 773:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10786 "Parser/parser.cc"
    break;

  case 774:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10792 "Parser/parser.cc"
    break;

  case 775:
#line 2995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10798 "Parser/parser.cc"
    break;

  case 776:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10804 "Parser/parser.cc"
    break;

  case 778:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10810 "Parser/parser.cc"
    break;

  case 779:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10816 "Parser/parser.cc"
    break;

  case 780:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10822 "Parser/parser.cc"
    break;

  case 781:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10828 "Parser/parser.cc"
    break;

  case 782:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10834 "Parser/parser.cc"
    break;

  case 783:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10840 "Parser/parser.cc"
    break;

  case 784:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10846 "Parser/parser.cc"
    break;

  case 785:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10852 "Parser/parser.cc"
    break;

  case 786:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10858 "Parser/parser.cc"
    break;

  case 787:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10864 "Parser/parser.cc"
    break;

  case 788:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10870 "Parser/parser.cc"
    break;

  case 789:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10876 "Parser/parser.cc"
    break;

  case 790:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10882 "Parser/parser.cc"
    break;

  case 791:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10888 "Parser/parser.cc"
    break;

  case 792:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10894 "Parser/parser.cc"
    break;

  case 793:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10900 "Parser/parser.cc"
    break;

  case 794:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10906 "Parser/parser.cc"
    break;

  case 795:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10912 "Parser/parser.cc"
    break;

  case 797:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10918 "Parser/parser.cc"
    break;

  case 798:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10924 "Parser/parser.cc"
    break;

  case 799:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10930 "Parser/parser.cc"
    break;

  case 800:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10936 "Parser/parser.cc"
    break;

  case 801:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10942 "Parser/parser.cc"
    break;

  case 802:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10948 "Parser/parser.cc"
    break;

  case 803:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10954 "Parser/parser.cc"
    break;

  case 804:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10960 "Parser/parser.cc"
    break;

  case 805:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10966 "Parser/parser.cc"
    break;

  case 806:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10972 "Parser/parser.cc"
    break;

  case 807:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10978 "Parser/parser.cc"
    break;

  case 808:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10984 "Parser/parser.cc"
    break;

  case 809:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10990 "Parser/parser.cc"
    break;

  case 810:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10996 "Parser/parser.cc"
    break;

  case 811:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11002 "Parser/parser.cc"
    break;

  case 812:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11008 "Parser/parser.cc"
    break;

  case 816:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11014 "Parser/parser.cc"
    break;

  case 817:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11020 "Parser/parser.cc"
    break;

  case 818:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11026 "Parser/parser.cc"
    break;

  case 819:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11032 "Parser/parser.cc"
    break;

  case 820:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11038 "Parser/parser.cc"
    break;

  case 821:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11044 "Parser/parser.cc"
    break;

  case 822:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11050 "Parser/parser.cc"
    break;

  case 823:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11056 "Parser/parser.cc"
    break;

  case 824:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11062 "Parser/parser.cc"
    break;

  case 825:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11068 "Parser/parser.cc"
    break;

  case 826:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11074 "Parser/parser.cc"
    break;

  case 827:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11080 "Parser/parser.cc"
    break;

  case 828:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11086 "Parser/parser.cc"
    break;

  case 829:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11092 "Parser/parser.cc"
    break;

  case 830:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11098 "Parser/parser.cc"
    break;

  case 831:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11107 "Parser/parser.cc"
    break;

  case 832:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11113 "Parser/parser.cc"
    break;

  case 833:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 835:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 836:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 837:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11137 "Parser/parser.cc"
    break;

  case 838:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11143 "Parser/parser.cc"
    break;

  case 839:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 840:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 841:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 842:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 843:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11173 "Parser/parser.cc"
    break;

  case 844:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11179 "Parser/parser.cc"
    break;

  case 845:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 846:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11191 "Parser/parser.cc"
    break;

  case 847:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 848:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 849:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 850:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11215 "Parser/parser.cc"
    break;

  case 851:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11221 "Parser/parser.cc"
    break;

  case 852:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 853:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 854:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 856:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 857:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 858:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 859:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 860:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 861:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 862:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 863:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 864:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11293 "Parser/parser.cc"
    break;

  case 865:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11299 "Parser/parser.cc"
    break;

  case 866:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11305 "Parser/parser.cc"
    break;

  case 867:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11311 "Parser/parser.cc"
    break;

  case 868:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 869:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 871:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 872:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 873:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 874:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11347 "Parser/parser.cc"
    break;

  case 875:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 876:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 877:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 878:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 879:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 880:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 881:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 883:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 884:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 885:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 886:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 887:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11419 "Parser/parser.cc"
    break;

  case 888:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 889:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11431 "Parser/parser.cc"
    break;

  case 891:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 892:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 893:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11449 "Parser/parser.cc"
    break;

  case 894:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11455 "Parser/parser.cc"
    break;

  case 895:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 896:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11467 "Parser/parser.cc"
    break;

  case 897:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11473 "Parser/parser.cc"
    break;

  case 898:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11479 "Parser/parser.cc"
    break;

  case 899:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 901:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11491 "Parser/parser.cc"
    break;

  case 902:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11497 "Parser/parser.cc"
    break;

  case 903:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 904:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 906:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 907:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11521 "Parser/parser.cc"
    break;

  case 908:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11527 "Parser/parser.cc"
    break;

  case 909:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11533 "Parser/parser.cc"
    break;

  case 910:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11539 "Parser/parser.cc"
    break;

  case 911:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11545 "Parser/parser.cc"
    break;

  case 912:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11551 "Parser/parser.cc"
    break;

  case 913:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11557 "Parser/parser.cc"
    break;

  case 915:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11563 "Parser/parser.cc"
    break;

  case 916:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11569 "Parser/parser.cc"
    break;

  case 917:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11575 "Parser/parser.cc"
    break;

  case 918:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11581 "Parser/parser.cc"
    break;

  case 919:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11587 "Parser/parser.cc"
    break;

  case 920:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11593 "Parser/parser.cc"
    break;

  case 922:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11599 "Parser/parser.cc"
    break;

  case 924:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11605 "Parser/parser.cc"
    break;

  case 925:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11611 "Parser/parser.cc"
    break;

  case 926:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11617 "Parser/parser.cc"
    break;

  case 927:
#line 3487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11623 "Parser/parser.cc"
    break;

  case 928:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11629 "Parser/parser.cc"
    break;

  case 929:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11635 "Parser/parser.cc"
    break;

  case 931:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11641 "Parser/parser.cc"
    break;

  case 932:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11647 "Parser/parser.cc"
    break;

  case 933:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11653 "Parser/parser.cc"
    break;

  case 934:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11659 "Parser/parser.cc"
    break;

  case 935:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11665 "Parser/parser.cc"
    break;

  case 936:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11671 "Parser/parser.cc"
    break;

  case 937:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11677 "Parser/parser.cc"
    break;

  case 939:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11683 "Parser/parser.cc"
    break;

  case 940:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 941:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11695 "Parser/parser.cc"
    break;

  case 942:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11701 "Parser/parser.cc"
    break;

  case 943:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11707 "Parser/parser.cc"
    break;

  case 946:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11713 "Parser/parser.cc"
    break;

  case 949:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11719 "Parser/parser.cc"
    break;

  case 950:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11725 "Parser/parser.cc"
    break;

  case 951:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11731 "Parser/parser.cc"
    break;

  case 952:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11737 "Parser/parser.cc"
    break;

  case 953:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11743 "Parser/parser.cc"
    break;

  case 954:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11749 "Parser/parser.cc"
    break;

  case 955:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11755 "Parser/parser.cc"
    break;

  case 956:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11761 "Parser/parser.cc"
    break;

  case 957:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11767 "Parser/parser.cc"
    break;

  case 958:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11773 "Parser/parser.cc"
    break;

  case 959:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11779 "Parser/parser.cc"
    break;

  case 960:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11785 "Parser/parser.cc"
    break;

  case 961:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11791 "Parser/parser.cc"
    break;

  case 962:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11797 "Parser/parser.cc"
    break;

  case 963:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11803 "Parser/parser.cc"
    break;

  case 964:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11809 "Parser/parser.cc"
    break;

  case 965:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11815 "Parser/parser.cc"
    break;

  case 966:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11821 "Parser/parser.cc"
    break;

  case 967:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11827 "Parser/parser.cc"
    break;

  case 968:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11833 "Parser/parser.cc"
    break;

  case 970:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11839 "Parser/parser.cc"
    break;

  case 974:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11845 "Parser/parser.cc"
    break;

  case 975:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11851 "Parser/parser.cc"
    break;

  case 976:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11857 "Parser/parser.cc"
    break;

  case 977:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11863 "Parser/parser.cc"
    break;

  case 978:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11869 "Parser/parser.cc"
    break;

  case 979:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11875 "Parser/parser.cc"
    break;

  case 980:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11881 "Parser/parser.cc"
    break;

  case 981:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11887 "Parser/parser.cc"
    break;

  case 982:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11893 "Parser/parser.cc"
    break;

  case 983:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11899 "Parser/parser.cc"
    break;

  case 984:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11905 "Parser/parser.cc"
    break;

  case 985:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11911 "Parser/parser.cc"
    break;

  case 986:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11917 "Parser/parser.cc"
    break;

  case 987:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11923 "Parser/parser.cc"
    break;

  case 988:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11929 "Parser/parser.cc"
    break;

  case 989:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11935 "Parser/parser.cc"
    break;

  case 990:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11941 "Parser/parser.cc"
    break;

  case 993:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11947 "Parser/parser.cc"
    break;

  case 994:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11953 "Parser/parser.cc"
    break;


#line 11957 "Parser/parser.cc"

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
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
